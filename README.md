# token-rich-types — minimal POC

A tiny typed language for UI where **the agent and the program are one thing**.
The only edit is *refinement*: a component rebuilds itself, and the type
contract is the guardrail that keeps every self-edit safe.

See `FEATURES.txt` for the full design. This POC implements the spine:
components, subtyping, holes, ASCII rendering, stable ids, and a model-backed
`prompt` primitive.

## Run

Racket lives at `/Applications/Racket v9.2/bin/racket` (not on `PATH`). From this
directory:

```sh
"/Applications/Racket v9.2/bin/racket" repl.rkt
```

The xAI key is read from `.env` (git-ignored). `prompt` calls `grok-build-0.1`.

## The language

```
(text   "…")            : Text
(header 1-6 "…")        : Header        ; Header <: Text
(button "…")            : Button
(stack  C C …)          : Stack         ; covariant in children
(hole   T)              : T             ; a TODO — top of T's lattice
```

Subtyping: `Header <: Text`, every component `<: Component`, a stack value
`<: Stack`, stacks covariant in their children. A node's **contract** (its spec)
is the loose kind it was created at; `prompt` may narrow the witness but must
stay a subtype of the contract.

## REPL commands

```
:show              render the page to ASCII
:tree              show ids, witness types, and contracts (⊑)
:get <id>          inspect one node
:prompt <id> <…>   refine that node by prompting it (it rebuilds itself)
:root <…>          refine the whole page
:new <s-expr>      replace the page
:help / :quit
```

## Demo

```
» :prompt c3 make the button say Subscribe Now
· attempt 1/3 → grok…
✓ Button ⊑ Button

# Max's Burgers
The best burgers in Flagstaff.
[ Subscribe Now ]
```

Refining a `Header`'s contract toward a button is *rejected* and retried until
Grok returns a real subtype — the contract is never violated. That is idiom 2
(spec vs. witness): you cannot subtype your way out of the spec.

## Files

| file | role |
|------|------|
| `core.rkt`  | components, types, subtyping, holes, render, the node store |
| `grok.rkt`  | xAI transport (reads `.env`) |
| `agent.rkt` | the `prompt` primitive: propose → typecheck `<:` → accept/retry |
| `repl.rkt`  | interactive REPL |

## Not yet (see FEATURES.txt)

`query` (recursive read), type-directed routing, SMT-backed refinements, the
Style/Behavior facets, and WASM codegen. This POC is the kernel they attach to.
