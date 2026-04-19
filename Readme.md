[![Github actions build status](https://img.shields.io/github/actions/workflow/status/jappeace/does-it-live/nix.yaml?branch=master)](https://github.com/jappeace/does-it-live/actions)

> "The reports of my death are greatly exaggerated." -- Every Hackage package with a 0.1.0.0 release from 2014

# does-it-live

A maintenance health scorer for Hackage packages. Scans all ~19,000 packages
and rates each on a 0-100 scale based on signals like upload recency, Stackage
inclusion, reverse dependency count, version history, and deprecation status.

Optionally attempts to actually build each package against Stackage LTS
constraints, recording whether it compiles, needs `--allow-newer`, or is
completely broken.

## Scoring

| Signal | Max points |
|---|---|
| Upload recency | 30 |
| Stackage nightly inclusion | 25 |
| Reverse dependency count | 20 |
| Version count | 15 |
| Not deprecated | 10 |

## Usage

```
nix-shell
cabal run does-it-live -- --help
```

```
does-it-live - Hackage package maintenance scorer

Usage: does-it-live [-o|--output FILE] [-c|--concurrency N]
                    [-m|--min-score N] [--max-score N]
                    [--check-builds] [--build-timeout SECONDS]

Options:
  -o,--output FILE         Output CSV file path (default: "output.csv")
  -c,--concurrency N       Maximum concurrent Hackage requests (default: 20)
  -m,--min-score N         Only output packages scoring at or above this threshold (default: 0)
  --max-score N            Only output packages scoring at or below this threshold (default: 100)
  --check-builds           Attempt to build each package using Stackage LTS
                           constraints, falling back to --allow-newer if solving fails
  --build-timeout SECONDS  Timeout in seconds for each package build (default: 600)
```

### Examples

Score all packages, output to CSV:
```
cabal run does-it-live
```

Only well-maintained packages, with build verification:
```
cabal run does-it-live -- --min-score 90 --check-builds
```

Low-scoring packages to see what's truly dead:
```
cabal run does-it-live -- --min-score 5 --max-score 10 --check-builds --build-timeout 120
```

### Build checking

When `--check-builds` is enabled, each package is:

1. Unpacked via `cabal get`
2. Given a `cabal.project` with Stackage LTS constraints
3. Tested with `cabal build --dry-run` to check constraint solving
4. If solving fails, retried with `--allow-newer` (jailbreak)
5. If solving succeeds, compiled with `cabal build`

The CSV output includes three extra columns:

- `can_solve` -- whether dependency constraints could be solved
- `builds` -- whether compilation succeeded
- `jailbroken` -- whether `--allow-newer` was needed

Results are written incrementally, so partial data survives crashes.

## Docker

Build and run inside a fat Docker image with GHC, cabal, and all native
C libraries pre-installed. No nix-shell needed on the host.

```
./scripts/build-docker.sh     # nix-build + docker load (~5 min first time)
./scripts/run-docker.sh --check-builds --min-score 50
```

Output lands in `./output/results.csv`. All CLI flags pass through to
`run-docker.sh`.

## Development

```
nix-shell
cabal build   # typecheck
cabal test    # run tests
```
