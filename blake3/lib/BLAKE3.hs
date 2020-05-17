{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Haskell bindings to the fast [official BLAKE3 hashing
-- implementation in assembly and C](https://github.com/BLAKE3-team/BLAKE3).
-- With support for AVX-512, AVX2 and SSE 4.1.
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
  , BIO.Digest
    -- * Keyed hashing
  , hashKeyed
  , BIO.Key
  , BIO.key
    -- * Key derivation
  , derive
  , BIO.Context
  , BIO.context
    -- * Incremental hashing
  , BIO.Hasher
  , hasher
  , hasherKeyed
  , update
  , finalize
    -- * Constants
  , BIO.KEY_LEN
  , BIO.BLOCK_SIZE
  , BIO.DEFAULT_DIGEST_LEN
  )
  where

import qualified Data.ByteArray as BA
import GHC.TypeLits
import System.IO.Unsafe (unsafeDupablePerformIO)

import qualified BLAKE3.IO as BIO

--------------------------------------------------------------------------------

-- | BLAKE3 hashing.
--
-- For incremental hashing, see 'hasher', 'update' and 'finalize':
--
-- @
-- 'hash' = 'finalize' '.' 'update' 'hasher'
-- @
hash
  :: forall len bin
  .  (KnownNat len, BA.ByteArrayAccess bin)
  => [bin]           -- ^ Data to hash.
  -> BIO.Digest len  -- ^ Default digest length is 'BIO.DEFAULT_DIGEST_LEN'.
hash bins = unsafeDupablePerformIO $ do
  fmap fst $ BIO.allocRetHasher $ \ph -> do
    BIO.init ph
    BIO.update ph bins
    BIO.finalize ph
{-# NOINLINE hash #-}

-- | BLAKE3 hashing with a 'BIO.Key'.
--
-- For incremental hashing, see 'hasherKeyed', 'update' and 'finalize':
--
-- @
-- 'hashKeyed' key = 'finalize' '.' 'update' ('hasherKeyed' key)
-- @
hashKeyed
  :: forall len bin
  .  (KnownNat len, BA.ByteArrayAccess bin)
  => BIO.Key
  -> [bin]           -- ^ Data to hash.
  -> BIO.Digest len  -- ^ Default digest length is 'BIO.DEFAULT_DIGEST_LEN'.
hashKeyed key0 bins = unsafeDupablePerformIO $ do
  fmap fst $ BIO.allocRetHasher $ \ph -> do
    BIO.initKeyed ph key0
    BIO.update ph bins
    BIO.finalize ph
{-# NOINLINE hashKeyed #-}

-- | BLAKE3 key derivation.
derive
  :: forall len ikm
  .  (KnownNat len, BA.ByteArrayAccess ikm)
  => BIO.Context
  -> [ikm]  -- ^ Input key material.
  -> BIO.Digest len  -- ^ Default digest length is 'BIO.DEFAULT_DIGEST_LEN'.
derive ctx ikms = unsafeDupablePerformIO $
  fmap fst $ BIO.allocRetHasher $ \ph -> do
    BIO.initDerive ph ctx
    BIO.update ph ikms
    BIO.finalize ph
{-# NOINLINE derive #-}

-- | Initial 'BIO.Hasher' for incremental hashing.
hasher :: BIO.Hasher -- ^
hasher = unsafeDupablePerformIO $
  fmap snd $ BIO.allocRetHasher BIO.init
{-# NOINLINE hasher #-}

-- | Initial 'BIO.Hasher' for incremental /keyed/ hashing.
hasherKeyed :: BIO.Key -> BIO.Hasher -- ^
hasherKeyed key0 = unsafeDupablePerformIO $
  fmap snd $ BIO.allocRetHasher $ \ph ->
  BIO.initKeyed ph key0
{-# NOINLINE hasherKeyed #-}

-- | Update 'BIO.Hasher' with new data.
update
  :: forall bin
  .  BA.ByteArrayAccess bin
  => BIO.Hasher
  -> [bin]  -- ^ New data to hash.
  -> BIO.Hasher
update h0 bins = unsafeDupablePerformIO $ do
  h1 <- BIO.copyHasher h0
  BIO.modifyHasher h1 $ \ph1 -> do
    BIO.update ph1 bins
    pure h1
{-# NOINLINE update #-}

-- | Finish hashing and obtain a 'BIO.Digest' of the specified @len@gth.
finalize
  :: forall len
  .  KnownNat len
  => BIO.Hasher -- ^
  -> BIO.Digest len
finalize h0 = unsafeDupablePerformIO $ do
  h1 <- BIO.copyHasher h0
  BIO.modifyHasher h1 $ \ph1 ->
    BIO.finalize ph1
{-# NOINLINE finalize #-}
