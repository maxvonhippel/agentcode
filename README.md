# token-rich-types

A tiny typed language for web pages where the program and the agent building
it are one thing: components are prompted, refine themselves, talk (delegate ⇘
/ escalate ⇖ / refuse / scream), answer questions, and keep their own history —
with the typechecker as the guardrail on every move. Design doc: `FEATURES.txt`.

## Run

```sh
racket ui.rkt     # viewer → http://localhost:8484
racket repl.rkt   # terminal REPL
racket bench.rkt  # benchmark: list | run | rate | report | show
```

Racket binary: `/Applications/Racket v9.2/bin/racket` if not on `PATH`.
xAI key/model/effort come from `.env` (git-ignored).

## Viewer

The page renders as a real website. Hover for a devtools-style chip
(`id · witness ⊑ contract`), click to select, then **prompt** (write) or
**query** (read) it; no selection targets the page. **build** replaces the
page with one labeled hole and lets it construct itself, section by section,
while you watch. Components pulse ochre while thinking, with bubbles showing
their instructions; accepted refinements flash green and scroll into view as
they swap in; screams and refusals arrive as toasts; the right panel streams
the protocol.

## REPL

```
:show [width]      ASCII screenshot        :tree     ids, types, contracts
:get <id>          inspect a node          :pin <id> <Type>  tighten contract
:prompt <id> <…>   refine a node           <id>.prompt(<…>)  dotted form
:query <id> <…>    ask a node a question   <id>.query(<…>)   dotted form
:root <…>          prompt the page         :ask <…>  query the page
:build <goal>      page builds itself from one hole
:journal           the program's history   :screams  extension proposals
:lexicon           the live vocabulary — prompt a words node to grow the language
:new <s-expr>      replace the page        :help :quit
```

`:help` also prints the component syntax. Every prompt shows the internal
conversation: requests, replies, typecheck verdicts, hops, journaling.

## Files

| file | role |
|------|------|
| `core.rkt`   | nodes, vocabulary tables, types/subtyping, holes, store+journal |
| `layout.rkt` | box-model layout → ASCII screenshot |
| `grok.rkt`   | xAI transport (reads `.env`) |
| `agent.rkt`  | the protocol: prompt/dispatch/cascade (write), query (read) |
| `repl.rkt`   | terminal REPL |
| `ui.rkt` + `ui.html` | the viewer |
| `bench.rkt`  | 50-task benchmark with human rating |
