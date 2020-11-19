# Version 0.3

* COMPILER ASSISTED BACKWARDS INCOMPATIBLE CHANGE: Rely on the `by` library rather
  than the `memory` library.

* COMPILER ASSISTED BACKWARDS INCOMPATIBLE CHANGE: The `hasher` function was
  renamed to `initHash`.

* COMPILER ASSISTED BACKWARDS INCOMPATIBLE CHANGE: The `hasherKeyed` function was
  renamed to `initKeyed`.

* COMPILER ASSISTED BACKWARDS INCOMPATIBLE CHANGE: The `hashKeyed` function was
  renamed to `keyed`.

* COMPILER ASSISTED BACKWARDS INCOMPATIBLE CHANGE: The `Digest` type and `digest` 
  function are not exported anymore. Functions return a polymorphic digest instead.

* COMPILER ASSISTED BACKWARDS INCOMPATIBLE CHANGE: The `Context` datatype and the 
  `context` function are not exported anymore. The `derive` function takes a 
  polymorphic context instead.

* COMPILER ASSISTED BACKWARDS INCOMPATIBLE CHANGE: The `finalizeSeek` function was 
  removed. The `finalize` function takes an offset parameter instead.

* COMPILER ASSISTED BACKWARDS INCOMPATIBLE CHANGE: `BLAKE3.IO` module removed.

* COMPILER ASSISTED BACKWARDS INCOMPATIBLE CHANGE: `Storable` instances removed.

* COMPILER ASSISTED BACKWARDS INCOMPATIBLE CHANGE: Reamed `KEY_LEN` to `KeyLength`. 
  Renamed `BLOCK_SIZE` to `BlockSize`. Renamed `DEFAULT_DIGEST_LEN` to 
  `DefaultDigestLength`.
 
* Export `initDerive`.

* Added SSE2 instructions support.

* Bumped C BLAKE3 code. 

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
