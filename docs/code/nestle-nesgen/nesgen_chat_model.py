"""LangChain-compatible chat model for an internal NesGen-style gateway.

Security note: every endpoint / credential value is read from the environment.
Never hardcode a real internal URL, header name, or secret in this file — copy
.env.example to .env, fill in your own values (obtained internally), and keep
.env out of git (already covered by the repo .gitignore).

The request/response shapes below are best-effort guesses at a generic
Anthropic-Messages-style ("claude") and OpenAI-Chat-Completions-style ("gpt")
gateway — the two formats the reference internal client (zeur-ds-agentic)
targets. Your actual NesGen contract may differ slightly: the two spots most
likely to need adjustment are marked "ADAPT ME" below.
"""

from __future__ import annotations

import os
import uuid
from pathlib import Path
from typing import Any, List, Optional

import requests
from langchain_core.language_models.chat_models import BaseChatModel
from langchain_core.messages import AIMessage, BaseMessage, HumanMessage, SystemMessage, ToolMessage
from langchain_core.outputs import ChatGeneration, ChatResult
from pydantic import Field


def _load_dotenv(path: Path) -> None:
    """Tiny stdlib-only .env loader — does not override already-set env vars."""
    if not path.exists():
        return
    for line in path.read_text(encoding="utf-8").splitlines():
        line = line.strip()
        if not line or line.startswith("#") or "=" not in line:
            continue
        key, _, value = line.partition("=")
        os.environ.setdefault(key.strip(), value.strip().strip('"').strip("'"))


_load_dotenv(Path(__file__).parent / ".env")


def _tool_schema(tool: Any) -> dict:
    """Minimal BaseTool -> JSON-schema-ish dict, shared by both payload builders."""
    schema = tool.args_schema.model_json_schema() if getattr(tool, "args_schema", None) else {
        "type": "object", "properties": {}
    }
    return {"name": tool.name, "description": tool.description or "", "schema": schema}


class NesgenChatModel(BaseChatModel):
    """Thin LangChain chat model wrapping an internal NesGen-style HTTP endpoint.

    All config comes from environment variables (see .env.example):
      NESGEN_FORMAT             "claude" (Anthropic Messages shape) or "gpt" (OpenAI shape)
      NESGEN_URL                completion endpoint (your own — never commit it)
      NESGEN_API_KEY            bearer/api-key credential
      NESGEN_ANTHROPIC_VERSION  "claude" format only (default: bedrock-2023-05-31)
      NESGEN_VERIFY_SSL         "false" to disable TLS verification (internal proxies only)
    """

    format: str = Field(default_factory=lambda: os.environ.get("NESGEN_FORMAT", "claude"))
    url: str = Field(default_factory=lambda: os.environ.get("NESGEN_URL", ""))
    api_key: str = Field(default_factory=lambda: os.environ.get("NESGEN_API_KEY", ""))
    anthropic_version: str = Field(
        default_factory=lambda: os.environ.get("NESGEN_ANTHROPIC_VERSION", "bedrock-2023-05-31")
    )
    verify_ssl: bool = Field(
        default_factory=lambda: os.environ.get("NESGEN_VERIFY_SSL", "true").lower() != "false"
    )
    max_tokens: int = 1024

    @property
    def _llm_type(self) -> str:
        return "nesgen-chat"

    def bind_tools(self, tools: List[Any], **kwargs: Any):
        return self.bind(tools=[_tool_schema(t) for t in tools], **kwargs)

    # -- request / response mapping -----------------------------------------
    # ADAPT ME: these two pairs are the part most likely to differ from your
    # actual NesGen contract (header name, response envelope, tool_use shape).

    def _build_payload(self, messages: List[BaseMessage], tools: Optional[list]) -> dict:
        system = "\n".join(m.content for m in messages if isinstance(m, SystemMessage))
        turns = [m for m in messages if not isinstance(m, SystemMessage)]

        if self.format == "gpt":
            body_messages = []
            if system:
                body_messages.append({"role": "system", "content": system})
            for m in turns:
                role = "assistant" if isinstance(m, AIMessage) else "user"
                body_messages.append({"role": role, "content": m.content})
            payload: dict = {"messages": body_messages, "max_tokens": self.max_tokens}
            if tools:
                payload["tools"] = [
                    {"type": "function", "function": {"name": t["name"], "description": t["description"],
                                                        "parameters": t["schema"]}}
                    for t in tools
                ]
            return payload

        # "claude" format (Anthropic Messages / Bedrock Converse style)
        body_messages = [
            {"role": "assistant" if isinstance(m, AIMessage) else "user", "content": m.content}
            for m in turns
        ]
        payload = {
            "anthropic_version": self.anthropic_version,
            "max_tokens": self.max_tokens,
            "messages": body_messages,
        }
        if system:
            payload["system"] = system
        if tools:
            payload["tools"] = [
                {"name": t["name"], "description": t["description"], "input_schema": t["schema"]}
                for t in tools
            ]
        return payload

    def _parse_response(self, data: dict) -> AIMessage:
        if self.format == "gpt":
            choice = data["choices"][0]["message"]
            tool_calls = [
                {"name": tc["function"]["name"], "args": tc["function"].get("arguments", {}), "id": tc["id"]}
                for tc in choice.get("tool_calls", []) or []
            ]
            return AIMessage(content=choice.get("content") or "", tool_calls=tool_calls)

        blocks = data.get("content", [])
        text = "".join(b.get("text", "") for b in blocks if b.get("type") == "text")
        tool_calls = [
            {"name": b["name"], "args": b.get("input", {}), "id": b.get("id", str(uuid.uuid4()))}
            for b in blocks if b.get("type") == "tool_use"
        ]
        return AIMessage(content=text, tool_calls=tool_calls)

    # -- LangChain entry point ------------------------------------------------

    def _generate(self, messages: List[BaseMessage], stop: Optional[List[str]] = None,
                   run_manager: Any = None, **kwargs: Any) -> ChatResult:
        if not self.url:
            raise RuntimeError(
                "NESGEN_URL is not set. Copy .env.example to .env and fill in your own "
                "internal NesGen endpoint before using NesgenChatModel."
            )
        payload = self._build_payload(messages, kwargs.get("tools"))
        response = requests.post(
            self.url,
            headers={"Authorization": f"Bearer {self.api_key}", "Content-Type": "application/json"},
            json=payload,
            timeout=60,
            verify=self.verify_ssl,
        )
        response.raise_for_status()
        message = self._parse_response(response.json())
        return ChatResult(generations=[ChatGeneration(message=message)])
