{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Haskell bindings to the fast [official BLAKE3 hashing
-- implementation in assembly and C](https://github.com/BLAKE3-team/BLAKE3).
-- With support for AVX-512, AVX2, SSE 2, and 4.1.
--
-- The original assembly and C implementation is released into the public domain with CC0 1.0.
-- Alternatively, it is licensed under the Apache License 2.0, copyright of Jack
-- O'Connor and Samuel Neves. See its [LICENSE](https://github.com/BLAKE3-team/BLAKE3/blob/88dcee7005be962a81516f7863e70009d9caa2c9/LICENSE)
-- for details.
--
-- This Haskell library is the copyright of Renzo Carbonara,
-- licensed under the terms of
-- the [Apache License 2.0](https://github.com/k0001/hs-blake3/blob/master/blake3/LICENSE).
module BLAKE3
  ( -- * Hashing
    hash
  , keyed
  , derive
    -- * Incremental hashing
  , Hasher
  , initHash
  , initKeyed
  , initDerive
  , update
  , finalize
    -- * Constants
  , KeyLength
  , BlockSize
  , DefaultDigestLength
  )
  where

import qualified By
import Data.Foldable
import Data.Proxy
import Data.Word
import Foreign.C.Types
import Foreign.Ptr
import GHC.TypeLits

--------------------------------------------------------------------------------

-- | BLAKE3 hashing.
--
-- For incremental hashing, see 'initHash', 'update' and 'finalize':
--
-- @
-- 'hash' = 'finalize' 0 '.' 'update' 'initHash' 
-- @
hash
  :: forall out msg
  .  (By.AllocN out, 
      By.Peek msg)
  => [msg]  -- ^ Data to hash.
  -> out    -- ^ Output digest.
hash bins = 
  let lout = fromIntegral (natVal (Proxy @(By.Length out)))
      (_ :: Hasher, out) = 
        By.allocFreezeN $ \ph -> do
          let ph' = castPtr ph
          c_init ph' 
          for_ bins $ \msg -> 
            By.peek msg $ \pmsg -> 
              c_update ph' pmsg (fromIntegral (By.length msg))
          fmap fst $ By.allocN $ \pout ->
            c_finalize ph' pout lout
  in out

-- | BLAKE3 hashing in keyed mode (for MAC, PRF).
--
-- For incremental hashing, see 'initKeyed', 'update' and 'finalize':
--
-- @
-- 'keyed' k = 'finalize' 0 '.' 'update' ('initKeyed' k)
-- @
keyed
  :: forall out msg key
  .  (By.AllocN out,
      By.Peek msg, 
      By.Peek key, 
      By.Length key ~ KeyLength)
  => key        -- ^ Secret hashing key.
  -> [msg]      -- ^ Data to hash.
  -> out        -- ^ Output digest.
keyed key bins = 
  let lout = fromIntegral (natVal (Proxy @(By.Length out)))
      (_ :: Hasher, out) = 
        By.allocFreezeN $ \ph -> do
          let ph' = castPtr ph
          By.peek key $ \pkey -> 
            c_init_keyed ph' pkey
          for_ bins $ \msg -> 
            By.peek msg $ \pmsg -> 
              c_update ph' pmsg (fromIntegral (By.length msg))
          fmap fst $ By.allocN $ \pout ->
            c_finalize ph' pout lout
  in out

-- | BLAKE3 key derivation.
--
-- This can be used for KDF (key derivation function) purposes.
-- 
-- The key derivation @context@ should be hardcoded, globally unique, 
-- application-specific well-known string. A good format for the 
-- context string is:
--
-- @
-- [application] [commit timestamp] [purpose]
-- @
--
-- For example:
--
-- @
-- example.com 2019-12-25 16:18:03 session tokens v1
-- @
--
-- For incremental hashing, see 'initDerive', 'update' and 'finalize':
--
-- @
-- 'derive' c = 'finalize' 0 '.' 'update' ('initDerive' c)
-- @
derive
  :: forall out ikm context
  .  (By.AllocN out, 
      By.Peek ikm, 
      By.Peek context)
  => context -- ^ Key derivation context. 
  -> [ikm]   -- ^ Input key material. 
  -> out     -- ^ Output key material.
derive ctx ikms = 
  let lout = fromIntegral (natVal (Proxy @(By.Length out)))
      (_ :: Hasher, out) = 
        By.allocFreezeN $ \ph -> do
          let ph' = castPtr ph
          By.peek ctx $ \pctx ->
            c_init_derive_key_raw ph' pctx (fromIntegral (By.length ctx))
          for_ ikms $ \ikm -> 
            By.peek ikm $ \pikm -> 
              c_update ph' pikm (fromIntegral (By.length ikm))
          fmap fst $ By.allocN $ \pout ->
            c_finalize ph' pout lout
  in out

-- | Initial 'Hasher' for incremental hashing.
initHash :: Hasher -- ^
initHash = 
  fst $ By.allocFreezeN $ \ph -> 
  c_init (castPtr ph)

-- | Initial 'Hasher' for incremental keyed hashing.
initKeyed
  :: forall key
  .  (By.Peek key,
      By.Length key ~ KeyLength)
  => key
  -> Hasher -- ^
initKeyed key = 
  fst $ By.allocFreezeN $ \ph -> 
  By.peek key $ \pkey ->
  c_init_keyed (castPtr ph) pkey

-- | Initial 'Hasher' for incremental key derivation.
initDerive
  :: forall context
  .  By.Peek context
  => context -- ^ Key derivation context. See 'derive' for format details.
  -> Hasher -- ^
initDerive ctx = 
  fst $ By.allocFreezeN $ \ph -> 
  By.peek ctx $ \pc ->
  c_init_derive_key_raw (castPtr ph) pc (fromIntegral (By.length ctx))

-- | Update 'Hasher' with new data.
update
  :: forall msg
  .  By.Peek msg
  => Hasher
  -> [msg]  -- ^ New data to hash.
  -> Hasher
update h0 bins =
  fst $ By.copyFreezeN h0 $ \ph1 ->
  for_ bins $ \msg ->
  By.peek msg $ \pmsg -> 
  c_update (castPtr ph1) pmsg (fromIntegral (By.length msg))

-- | Finalize incremental hashing and obtain the specified @len@gth of BLAKE3
-- output starting at the specified offset.
finalize
  :: forall out
  .  By.AllocN out
  => Word64     -- ^ BLAKE3 output offset. Usually @0@.
  -> Hasher
  -> out 
finalize off h0 = 
  let lout = fromIntegral (natVal (Proxy @(By.Length out)))
      (_ :: Hasher, out) = 
        By.copyFreezeN h0 $ \ph1 -> 
        fmap fst $ By.allocN $ \pout -> 
        case off of 0 -> c_finalize (castPtr ph1) pout lout 
                    _ -> c_finalize_seek (castPtr ph1) off pout lout
  in out

--------------------------------------------------------------------------------

-- | BLAKE3 internal state.
--
-- Obtain with 'initHash', 'initKeyed', 'initDerive'.
-- Update with 'update'.
-- Finalize with 'finalize'.
newtype Hasher = Hasher (By.Sized HasherSize By.ByeString)
  deriving newtype
    ( Eq -- ^ Constant time.
    , By.Peek
    , By.Poke
    , By.GetLength
    , By.KnownLength
    , By.AllocN -- ^ The allocated memory is wiped and freed as soon if unused.
    )

--------------------------------------------------------------------------------
-- Constants

-- | 32 bytes.
type KeyLength = 32

-- | 32 bytes. 
-- 
-- BLAKE3 can produce digests of arbitrary length, but 32 bytes is the default.
type DefaultDigestLength = 32

-- | 64 bytes.
type BlockSize = 64

-- | In bytes.
type HasherSize = 1912

--------------------------------------------------------------------------------

-- | @void blake3_hasher_init(blake3_hasher *self)@
foreign import ccall unsafe
  "blake3.h blake3_hasher_init"
  c_init
    :: Ptr Hasher  -- ^ You can obtain with 'BAS.alloc'.
                   -- Otherwise, any chunk of 'HasherSize' bytes aligned to
                   -- 'HASHER_ALIGNMENT' will do.
    -> IO ()

-- | @void blake3_hasher_init_keyed(blake3_hasher *self, const uint8_t key['KeyLength'])@
foreign import ccall unsafe
  "blake3.h blake3_hasher_init_keyed"
  c_init_keyed
    :: Ptr Hasher  -- ^ You can obtain with 'BAS.alloc'.
                   -- Otherwise, any chunk of 'HasherSize' bytes aligned to
                   -- 'HASHER_ALIGNMENT' will do.
    -> Ptr Word8   -- ^ You can obtain with 'BAS.alloc'.
                   -- Otherwise, any chunk of length 'KeyLength' will do.
    -> IO ()


-- | @void blake3_hasher_init_derive_key_raw(blake3_hasher *self, const void *context, size_t context_len)@
foreign import ccall unsafe
  "blake3.h blake3_hasher_init_derive_key_raw"
  c_init_derive_key_raw
    :: Ptr Hasher  -- ^ You can obtain with 'BAS.alloc'.
                   -- Otherwise, any chunk of 'HasherSize' bytes aligned to
                   -- 'HASHER_ALIGNMENT' will do.
    -> Ptr Word8   -- ^ Context.
    -> CSize       -- ^ Context length.
    -> IO ()

-- | @void blake3_hasher_update(blake3_hasher *self, const void *input, size_t input_len)@
foreign import ccall unsafe
  "blake3.h blake3_hasher_update"
  c_update
    :: Ptr Hasher -- ^ Must have been previously initializedi. See 'c_init',
                  -- 'c_init_keyed', 'c_init_derive_key'.
    -> Ptr Word8  -- ^ Data.
    -> CSize      -- ^ Data length.
    -> IO ()

-- | @void blake3_hasher_finalize(const blake3_hasher *self, uint8_t *out, size_t out_len)@
foreign import ccall unsafe
  "blake3.h blake3_hasher_finalize"
  c_finalize
    :: Ptr Hasher -- ^ Must have been previously initializedi. See 'c_init',
                  -- 'c_init_keyed', 'c_init_derive_key'.
    -> Ptr Word8  -- ^ Out.
    -> CSize      -- ^ Out length.
    -> IO ()

-- | @void blake3_hasher_finalize_seek(const blake3_hasher *self, uint64_t seek, uint8_t *out, size_t out_len)@
foreign import ccall unsafe
  "blake3.h blake3_hasher_finalize_seek"
  c_finalize_seek
    :: Ptr Hasher  -- ^ Must have been previously initializedi. See 'c_init',
                   -- 'c_init_keyed', 'c_init_derive_key'.
    -> Word64      -- ^ Seek position.
    -> Ptr Word8   -- ^ Out.
    -> CSize       -- ^ Out length.
    -> IO ()
