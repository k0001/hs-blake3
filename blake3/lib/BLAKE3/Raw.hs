{-# LANGUAGE DataKinds #-}
{-# OPTIONS_HADDOCK hide not_home #-}

module BLAKE3.Raw where

import Data.Word
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Prelude hiding (init)

--------------------------------------------------------------------------------

type HASHER_ALIGNMENT = 8
-- | In bytes.
type HASHER_SIZE = 1912
-- | In bytes.
type KEY_LEN = 32
-- | In bytes.
type DEFAULT_DIGEST_LEN = 32
-- | In bytes.
type BLOCK_SIZE = 64

type CHUNK_LEN = 1024
type MAX_DEPTH = 54
type MAX_SIMD_DEGREE = 16

-- | Opaque datatype of size 'HASHER_SIZE' and alignment 'HASHER_ALIGNMENT'.
--
-- Obtain with 'BLAKE3.IO.withHasherInternal'.
data HasherInternal

-- | @void blake3_hasher_init(blake3_hasher *self)@
foreign import ccall unsafe
  "blake3.h blake3_hasher_init"
  init
    :: Ptr HasherInternal
    -> IO () -- ^

-- | @void blake3_hasher_init_keyed(blake3_hasher *self, const uint8_t key['KEY_LEN'])@
foreign import ccall unsafe
  "blake3.h blake3_hasher_init_keyed"
  init_keyed
    :: Ptr HasherInternal
    -> Ptr Word8   -- ^ Key of length 'KEY_LEN'.
    -> IO ()

-- | @void blake3_hasher_init_derive_key(blake3_hasher *self, const char *context)@
foreign import ccall unsafe
  "blake3.h blake3_hasher_init_derive_key"
  init_derive_key
    :: Ptr HasherInternal
    -> CString  -- ^ Context.
    -> IO ()    -- ^

-- | @void blake3_hasher_update(blake3_hasher *self, const void *input, size_t input_len)@
foreign import ccall unsafe
  "blake3.h blake3_hasher_update"
  update
    :: Ptr HasherInternal
    -> Ptr Word8 -- ^ Data.
    -> CSize     -- ^ Data length.
    -> IO () -- ^

-- | @void blake3_hasher_finalize(const blake3_hasher *self, uint8_t *out, size_t out_len)@
foreign import ccall unsafe
  "blake3.h blake3_hasher_finalize"
  finalize
    :: Ptr HasherInternal
    -> Ptr Word8 -- ^ Out.
    -> CSize     -- ^ Out length.
    -> IO () -- ^

-- | @void blake3_hasher_finalize_seek(const blake3_hasher *self, uint64_t seek, uint8_t *out, size_t out_len)@
foreign import ccall unsafe
  "blake3.h blake3_hasher_finalize"
  finalize_seek
    :: Ptr HasherInternal
    -> Word64      -- ^ Seek.
    -> Ptr Word8   -- ^ Out.
    -> CSize       -- ^ Out length.
    -> IO () -- ^
