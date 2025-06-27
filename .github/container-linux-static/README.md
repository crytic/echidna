# Static GHC & musl container

This container is used as part of `.github/workflows/ci.yml` to produce
statically-linked amd64 linux builds of Echidna. It is based on the following
container produced by Olivier Benz and maintained in the
[`benz0li/ghc-musl`](https://gitlab.com/benz0li/ghc-musl)
repository, and contains a few extra dependencies and configurations to make it
suitable for the GitHub Actions environment.
