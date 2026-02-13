# SDK Name Ideas (Moonshot Ecosystem)

## Recommendation

1. `moonshot-sdk-cl`
- Clear ecosystem association (`moonshot`)
- Explicit language/runtime (`cl`)
- Avoids confusion with editor `common-lisp` naming

## Strong Alternatives

2. `kimi-sdk-cl`
- Aligns directly with `kimi-sdk` branding
- Best if goal is strict parity with Python `kimi-sdk`

3. `kosong-cl`
- Aligns with the deeper implementation layer (`kosong`)
- Best if you are porting orchestration semantics, not only API surface

4. `moonshot-common-lisp`
- Already used in this repo
- Good continuity, but longer and less package-index friendly

## Naming Guidance

- If public API should mirror `kimi-sdk` most closely: choose `kimi-sdk-cl`.
- If this is a broad Moonshot client for CL users: choose `moonshot-sdk-cl`.
- If porting full `kosong` abstractions (`generate/step/tooling`): choose `kosong-cl`.

## Suggested Decision

Adopt `moonshot-sdk-cl` as package/project name and keep `moonshot-common-lisp` as a transitional alias during migration.
