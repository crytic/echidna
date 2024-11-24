# Static GHC & musl container

This container is used as part of `.github/workflows/ci.yml` to produce
statically-linked amd64 linux builds of Echidna. It is based on the following
container produced by FP Complete and maintained in the
[`fpco/alpine-haskell-stack`](https://github.com/fpco/alpine-haskell-stack/tree/ghc927)
repository, and contains a few extra dependencies and configurations to make it
suitable for the GitHub Actions environment.
