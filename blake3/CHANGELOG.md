# Version 0.2

* COMPILER ASSISTED BACKWARDS INCOMPATIBLE CHANGE: Drop the `BLAKE3.Raw` module
  in favor of `BLAKE.IO`. Re-export internals from there.

* COMPILER ASSISTED BACKWARDS INCOMPATIBLE CHANGE: Drop `HasherInternal` and
  related functions in favor of `Ptr Hasher`.

* COMPILER ASSISTED BACKWARDS INCOMPATIBLE CHANGE: Drop `allocRetHasher`,
  `allocRetKey`, `allocRetDigest`.

* Added `Storable`, `ByteArrayAccess` and `ByteArrayN` instances for `Hasher`,
  `Key` and `Digest`.

* Added `ByteArrayAccess` instance for `Context`.

* Added `modifyHasher`, `digest`.

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
