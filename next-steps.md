# Next Steps for Amoebum

This file is now archival.

Active operator work should be queued in local `.yarli/tranches.toml` state and
bootstrapped with `./bin/yarli-bootstrap-local-state.sh` when needed. Durable
project truth lives in `PROMPT.md`, `IMPLEMENTATION_PLAN.md`, and `yarli.toml`.
Keep this file as a short archival bridge for older references, not as an active
execution queue.

## Historical Closures

The following work items are already closed and should not be treated as active:

1. ANSI escape sanitization in Amoebum and Pseudopod
2. Tokyo Night theme variable/package fix
3. Chat input responsiveness and stuck-stream recovery

## Current Notes

1. Keep `NXT-154` deferred unless the operator explicitly reprioritizes it.
2. Preserve the PTUI verification stack (`release-checklist`, tmux harnesses, visual baselines) and keep it green.
3. Unicode-width handling is no longer a confirmed open bug; PTUI already has dedicated grapheme and emoji width coverage, so reopen only with a fresh repro.

## References

1. `docs/yarli-local-state.md`
2. `ptui/docs/testing-strategy.md`
3. `ptui/docs/benchmark-story.md`
