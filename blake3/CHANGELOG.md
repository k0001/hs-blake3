# Version 0.3

* COMPILER ASSISTED BACKWARDS INCOMPATIBLE CHANGE: The `hash` function now takes
  an optional `Key`.

* COMPILER ASSISTED BACKWARDS INCOMPATIBLE CHANGE: The `hasher` function was
  removed. Instead, use `init` without specifying a `Key`.

* COMPILER ASSISTED BACKWARDS INCOMPATIBLE CHANGE: The `hashKeyed` and
  `hasherKeyed` functions were removed. Instead, specify the `Key` when using
  `hash` and `init`.

* COMPILER ASSISTED BACKWARDS INCOMPATIBLE CHANGE: The `digest` functions is not
  exported anymore. The `Digest` constructor is exported instead.

* COMPILER ASSISTED BACKWARDS INCOMPATIBLE CHANGE: The `Context` datatype and the
  `context` function are not exported anymore. The `derive` function takes a
  polymorphic context instead.

* Functions that previously returned a `Digest` now return a polymorphic
  `ByteArrayN`. This makes it easy for downstream libraries to reuse any BLAKE3
  output for other purposes without having to copy bytes over. The `Digest`
  datatype is still exported as a convenience.

* Added `sse2` Cabal flag to enable SSE2 optimizations.

* Bumped upstream BLAKE3 C sources to latest version as of February 4, 2023.


# Version 0.2

* COMPILER ASSISTED BACKWARDS INCOMPATIBLE CHANGE: Drop the `BLAKE3.Raw` module
  in favor of `BLAKE.IO`. Re-export internals from there.

* COMPILER ASSISTED BACKWARDS INCOMPATIBLE CHANGE: Drop `HasherInternal` and
  related functions in favor of `Ptr Hasher`.

* COMPILER ASSISTED BACKWARDS INCOMPATIBLE CHANGE: Drop `allocRetHasher`,
  `allocRetKey`, `allocRetDigest`.

* Added `Eq`, `Show`, `Storable`, `ByteArrayAccess` and `ByteArrayN` instances
  for `Hasher`, `Key` and `Digest`.

* Added `ByteArrayAccess` instance for `Context`.

* Added `finalizeSeek`, `modifyHasher`, `digest`.

* More tests.

* Documentation improvements.


# Version 0.1.1

* Enabled AVX-512, AVX2 and SSE-4.1 support.

  On x86_64 Linux, Darwin and Windows, assembly implementations are used.
  Elsewhere, C intrinsics are used.

  The `avx512`, `avx2` and `sse41` Cabal flags can be used to disable these
  optimizations.

* Documentation fixes.


# Version 0.1

* Initial version.

* Using upstream C code from
  [https://github.com/BLAKE3-team/BLAKE3](https://github.com/BLAKE3-team/BLAKE3)
  version `5651ce7ee0b0ad2f577beef7efcef87e2f39fbe2`.
