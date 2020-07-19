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
  , BIO.Digest(..)
    -- * Keyed hashing
  , BIO.Key
  , BIO.key
    -- * Key derivation
  , derive
  , BIO.Context
  , BIO.context
    -- * Incremental hashing
  , BIO.Hasher
  , init
  , update
  , finalize
  , finalizeSeek
    -- * Constants
  , BIO.KEY_LEN
  , BIO.BLOCK_SIZE
  , BIO.DEFAULT_DIGEST_LEN
  )
  where

import qualified Data.ByteArray as BA
import qualified Data.ByteArray.Sized as BAS
import Data.Proxy
import Data.Word
import Prelude hiding (init)
import System.IO.Unsafe (unsafeDupablePerformIO)

import qualified BLAKE3.IO as BIO

--------------------------------------------------------------------------------

-- | BLAKE3 hashing.
--
-- For incremental hashing, see 'init', 'update' and 'finalize':
--
-- @
-- 'hash' yk = 'finalize' '.' 'update' ('init' yk)
-- @
hash
  :: forall len digest bin
  .  (BAS.ByteArrayN len digest, BA.ByteArrayAccess bin)
  => Maybe BIO.Key -- ^ Whether to use keyed hashing mode (for MAC, PRF).
  -> [bin]  -- ^ Data to hash.
  -> digest -- ^ The @digest@ type could be @'BIO.Digest' len@.
hash yk = unsafeDupablePerformIO . BIO.hash yk
{-# NOINLINE hash #-}

-- | BLAKE3 key derivation.
--
-- This can be used for KDF (key derivation function) purposes.
derive
  :: forall len okm ikm
  .  (BAS.ByteArrayN len okm, BA.ByteArrayAccess ikm)
  => BIO.Context
  -> [ikm]  -- ^ Input key material.
  -> okm    -- ^ Output key material of the specified @len@ght.
derive ctx ikms = unsafeDupablePerformIO $ do
  (dig, _ :: BIO.Hasher) <- BAS.allocRet Proxy $ \ph -> do
    BIO.initDerive ph ctx
    BIO.update ph ikms
    BIO.finalize ph
  pure dig
{-# NOINLINE derive #-}

-- | Initial 'BIO.Hasher' for incremental hashing.
init
  :: Maybe BIO.Key -- ^ Whether to use keyed hashing mode (for MAC, PRF).
  -> BIO.Hasher
init yk =
  BAS.allocAndFreeze $ \ph ->
  BIO.init ph yk

-- | Update 'BIO.Hasher' with new data.
update
  :: forall bin
  .  BA.ByteArrayAccess bin
  => BIO.Hasher
  -> [bin]  -- ^ New data to hash.
  -> BIO.Hasher
update h0 bins =
  BAS.copyAndFreeze h0 $ \ph1 ->
  BIO.update ph1 bins

-- | Finalize incremental hashing and obtain a the BLAKE3 output of the
-- specified @len@gth.
finalize
  :: forall len output
  .  BAS.ByteArrayN len output
  => BIO.Hasher
  -> output -- ^ The @output@ type could be @'BIO.Digest' len@.
finalize h0 = unsafeDupablePerformIO $ do
  (dig, _ :: BIO.Hasher) <- BAS.copyRet h0 BIO.finalize
  pure dig
{-# NOINLINE finalize #-}

-- | Finalize incremental hashing and obtain the specified @len@gth of BLAKE3
-- output starting at the specified offset.
--
-- @
-- 'finalize' h = 'finalizeSeek' h 0
-- @
finalizeSeek
  :: forall len output
  .  BAS.ByteArrayN len output
  => BIO.Hasher
  -> Word64     -- ^ BLAKE3 output offset.
  -> output     -- ^ The @output@ type could be @'BIO.Digest' len@.
finalizeSeek h0 pos = unsafeDupablePerformIO $ do
  (dig, _ :: BIO.Hasher) <- BAS.copyRet h0 $ \ph -> BIO.finalizeSeek ph pos
  pure dig
{-# NOINLINE finalizeSeek #-}

--------------------------------------------------------------------------------

