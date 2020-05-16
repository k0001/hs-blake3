{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module BLAKE3.IO
  ( -- * Hashing
    init
  , update
  , finalize
  , Hasher
  , allocRetHasher
  , Digest
  , allocRetDigest
  -- ** Memory
  , Raw.HasherInternal
  , copyHasher
  , withHasherInternal
  -- * Keyed hashing
  , Key
  , key
  , allocRetKey
  , initKeyed
  -- * Key derivation
  , Context
  , context
  , initDerive
  -- * Constants
  , Raw.HASHER_ALIGNMENT
  , Raw.HASHER_SIZE
  , Raw.KEY_LEN
  , Raw.BLOCK_SIZE
  , Raw.DEFAULT_DIGEST_LEN
  )
  where

import Control.Monad (guard)
import qualified Data.ByteArray as BA
import qualified Data.ByteArray.Encoding as BA
import Data.Foldable
import qualified Data.Memory.PtrMethods as BA
import Data.Proxy
import Data.String
import Data.Word
import Foreign.Ptr
import Foreign.Storable
import GHC.TypeLits
import Prelude hiding (init)

import qualified BLAKE3.Raw as Raw

--------------------------------------------------------------------------------

-- | Immutable BLAKE3 hashing state.
--
-- Obtain with 'BLAKE3.hasher' or 'BLAKE3.hasherKeyed'.
newtype Hasher = Hasher BA.ScrubbedBytes
  -- deriving newtype (BA.ByteArrayAccess)

-- | Allocate 'Hasher'.
--
-- The 'Hasher' is wiped and freed as soon as it becomes unused.
allocRetHasher
  :: forall a
  .  (Ptr Raw.HasherInternal -> IO a)  -- ^ Initialize 'Raw.HASHER_SIZE' bytes.
  -> IO (a, Hasher)
allocRetHasher g = do
  let size = fromIntegral (natVal (Proxy @Raw.HASHER_SIZE))
  (a, bs) <- BA.allocRet size g
  pure (a, Hasher bs)

-- | Mutate the given 'Hasher'.
withHasherInternal
  :: Hasher
  -> (Ptr Raw.HasherInternal -> IO a) -- ^ Read or write.
  -> IO a
withHasherInternal (Hasher x) = BA.withByteArray x

-- | Copy an inmutable 'Hasher'.
copyHasher :: Hasher -> IO Hasher -- ^
copyHasher (Hasher x) = fmap Hasher $ BA.copy x (const (pure ()))

--------------------------------------------------------------------------------

-- | Output from BLAKE3 algorithm, of @len@ bytes.
--
-- The default digest length for BLAKE3 is 'Raw.DEFAULT_DIGEST_LEN'.
newtype Digest (len :: Nat) = Digest BA.ScrubbedBytes
  deriving newtype ( Eq -- ^ Constant time.
                   , BA.ByteArrayAccess)

-- | Base 16 (hexadecimal).
instance Show (Digest len) where
  show (Digest x) = showBase16 x

-- | Allocate a 'Digest'.
--
-- The 'Digest' is wiped and freed as soon as it becomes unused.
allocRetDigest
  :: forall len a
  .  KnownNat len
  => (Ptr Word8 -> IO a)  -- ^ Initialize @len@ bytes.
  -> IO (a, Digest len)
allocRetDigest g = do
  let size = fromIntegral (natVal (Proxy @len))
  (a, bs) <- BA.allocRet size g
  pure (a, Digest bs)

--------------------------------------------------------------------------------

-- | Key used for keyed hashing mode.
--
-- Obtain with 'BLAKE3.key'.
--
-- See 'BLAKE3.hashKeyed'.
newtype Key = Key BA.ScrubbedBytes
  deriving newtype ( Eq -- ^ Constant time.
                   , BA.ByteArrayAccess)

-- | Base 16 (hexadecimal).
instance Show Key where
  show (Key x) = showBase16 x

keyLen :: Int
keyLen = fromIntegral (natVal (Proxy @Raw.KEY_LEN))

-- | Obtain a 'Key' for use in BLAKE3 keyed hashing.
--
-- See 'BLAKE3.hashKeyed'.
key
  :: BA.ByteArrayAccess bin
  => bin -- ^ Key bytes. Must have length 'Raw.KEY_LEN'.
  -> Maybe Key -- ^
key bin | BA.length bin == keyLen = Just (Key (BA.convert bin))
        | otherwise = Nothing

-- | Allocate a 'Key'.
--
-- The 'Key' is wiped and freed as soon as it becomes unused.
allocRetKey
  :: forall a
  . (Ptr Word8 -> IO a) -- ^ Initialize 'Raw.KEY_LEN' bytes.
  -> IO (a, Key)
allocRetKey g = do
  (a, bs) <- BA.allocRet keyLen g
  pure (a, Key bs)

--------------------------------------------------------------------------------

-- | Context for BLAKE3 key derivation. Obtain with 'context'.
newtype Context = Context BA.Bytes -- ^ NUL-terminated 'CString'.
  deriving newtype (Eq)

-- | Base 16 (hexadecimal).
instance Show Context where
  show (Context x) = showBase16 (BA.takeView x (BA.length x - 1))

-- | 'fromString' is a /partial/ function that fails if the given 'String'
-- contains 'Char's outside the range @['toEnum' 1 .. 'toEnum' 255]@. 
--
-- See 'context' for more details.
instance IsString Context where
  fromString s = case traverse charToWord8 s of
      Nothing -> error "Not a valid String for Context"
      Just w8s -> Context $! BA.pack (w8s <> [0])
    where
      charToWord8 :: Char -> Maybe Word8
      charToWord8 c = do
        let i = fromEnum c
        guard (i > 0 && i < 256)
        pure (fromIntegral i)

-- | Obtain a 'Context' for BLAKE3 key derivation.
--
-- The context should be hardcoded, globally unique, and
-- application-specific.
--
-- A good format for the context string is:
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
context
  :: BA.ByteArrayAccess bin
  => bin -- ^ If @bin@ contains null bytes, this function returns 'Nothing'.
  -> Maybe Context
context src
  | BA.any (0 ==) src = Nothing
  | otherwise = Just $ Context $
      let srcLen = BA.length src
          dstLen = srcLen + 1
      in BA.allocAndFreeze dstLen $ \pdst ->
         BA.withByteArray src $ \psrc -> do
           BA.memCopy pdst psrc srcLen
           pokeByteOff pdst srcLen (0 :: Word8)

--------------------------------------------------------------------------------

-- | Initialize a 'Raw.HasherInternal'.
init
  :: Ptr Raw.HasherInternal -- ^ Will be mutated.
  -> IO ()
init = Raw.init

-- | Initialize a 'Raw.HasherInternal' in keyed mode.
initKeyed
  :: Ptr Raw.HasherInternal -- ^ Will be mutated.
  -> Key
  -> IO () -- ^
initKeyed ph key0 =
  BA.withByteArray key0 $ \pkey ->
  Raw.init_keyed ph pkey

-- | Initialize a 'Raw.HasherInternal' in derivation mode.
--
-- The input key material must be provided afterwards, using 'update'.
initDerive
  :: Ptr Raw.HasherInternal -- ^ Will be mutated.
  -> Context
  -> IO ()
initDerive ph (Context ctx) =
  BA.withByteArray ctx $ \pc ->
  Raw.init_derive_key ph pc

-- | Update 'Raw.HasherInternal' state with new data.
update
  :: forall bin
  .  BA.ByteArrayAccess bin
  => Ptr Raw.HasherInternal -- ^ Will be mutated.
  -> [bin]
  -> IO () -- ^
update ph bins =
  for_ bins $ \bin ->
  BA.withByteArray bin $ \pbin ->
  Raw.update ph pbin (fromIntegral (BA.length bin))

-- | Finalize 'Raw.HasherInternal' state and obtain a digest.
--
-- The 'Raw.HasherInternal' is mutated.
finalize
  :: forall len
  .  KnownNat len
  => Ptr Raw.HasherInternal -- ^ Will be mutated.
  -> IO (Digest len) -- ^
finalize ph =
  fmap snd $ allocRetDigest $ \pd ->
  Raw.finalize ph pd (fromIntegral (natVal (Proxy @len)))

--------------------------------------------------------------------------------

showBase16 :: BA.ByteArrayAccess x => x -> String
showBase16 = fmap (toEnum . fromIntegral)
           . BA.unpack @BA.ScrubbedBytes
           . BA.convertToBase BA.Base16
