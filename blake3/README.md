# Haskell BLAKE3

Bindings to
the [official fast BLAKE3 implementations in assembly and C](https://github.com/BLAKE3-team/BLAKE3),
with support for AVX-512, AVX2 and SSE 4.1, SSE 2.

# Development

* Build with `nix-build -A ghc865` (or `ghc883`).

* Enter into a development environment with `nix-shell -A ghc865` (or `ghc883`).
  From this environment, you can build with `cabal new-build blake3`, test with
  `cabal new-test blake3`, render documentation with `cabal new-haddock blake3`,
  etc.
