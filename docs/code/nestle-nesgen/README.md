# Variante Nestlé / NesGen

Même notebook d'atelier (`agent_memory_langgraph_deepagents.ipynb`), mais câblé sur un
endpoint **NesGen** interne plutôt que sur une clé Anthropic directe — pour les collègues
Nestlé qui n'ont accès qu'à NesGen.

**Ce dossier ne contient aucune URL ni secret Nestlé réel.** `nesgen_chat_model.py` lit
toute sa config depuis des variables d'environnement (voir `.env.example`). C'est un choix
délibéré : ce repo est public, `zeur-ds-agentic` (le client interne dont ce pattern
s'inspire) est marqué *Internal Nestle use only* et contient de vraies URLs `*.nestle.com` —
elles n'ont pas leur place ici.

## Setup

```bash
cp .env.example .env
# éditez .env : NESGEN_URL, NESGEN_API_KEY (vos identifiants internes NesGen)
pip install -r requirements.txt
```

`nesgen_chat_model.py` charge automatiquement `.env` au premier import (petit loader
stdlib, pas de dépendance `python-dotenv`).

## Utilisation

```python
from nesgen_chat_model import NesgenChatModel

llm = NesgenChatModel()
llm.invoke([HumanMessage(content="Bonjour")])
```

`NesgenChatModel` implémente l'interface `BaseChatModel` de LangChain — il se branche
directement là où le notebook original utilise `ChatAnthropic` ou une string
`"anthropic:claude-..."` (checkpointer LangGraph, `create_react_agent`, `create_deep_agent`).

## Limite connue : le tool-calling (sections 4 et 5)

Le client interne de référence (`zeur-ds-agentic`) expose une API de complétion simple, pas
de tool-calling natif documenté. `NesgenChatModel._build_payload` / `_parse_response`
tentent un mapping vers le format standard Anthropic Messages / OpenAI Chat Completions
(blocs `tool_use`, `tool_calls`) — marqué `# ADAPT ME` dans le code. Si votre déploiement
NesGen ne proxy pas fidèlement ce contrat, les sections 4 (`langmem`) et 5 (`DeepAgents`) du
notebook échoueront avec une erreur claire côté HTTP plutôt que silencieusement ; les
sections 1 à 3 (maths, checkpointer LangGraph) n'ont pas besoin de tool-calling et
fonctionnent dans tous les cas.

## Fichiers

- `nesgen_chat_model.py` — le wrapper `BaseChatModel`.
- `.env.example` — gabarit de config, valeurs vides.
- `agent_memory_nesgen.ipynb` — le notebook de l'atelier, identique dans son plan, avec
  `NesgenChatModel` à la place d'Anthropic direct.
