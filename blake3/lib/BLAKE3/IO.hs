{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | IO and low level tools.
module BLAKE3.IO
  ( -- * Hashing
    init
  , update
  , finalize
    -- * Digest
  , Digest
  , allocRetDigest
    -- * Keyed hashing
  , Key
  , key
  , allocRetKey
  , initKeyed
    -- * Key derivation
  , Context
  , context
  , initDerive
    -- * Hasher
  , Hasher
  , modifyHasher
  , allocRetHasher
  , copyHasher
    -- * Constants
  , HASHER_ALIGNMENT
  , HASHER_SIZE
  , KEY_LEN
  , BLOCK_SIZE
  , DEFAULT_DIGEST_LEN
  , CHUNK_LEN
  , MAX_DEPTH
  , MAX_SIMD_DEGREE
    -- * Low-level C bindings
  , c_init
  , c_init_keyed
  , c_init_derive_key
  , c_update
  , c_finalize
  , c_finalize_seek
  )
  where

import Control.Monad (guard)
import Data.Foldable
import Data.Proxy
import Data.String
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Array (copyArray)
import Foreign.Ptr
import Foreign.Storable
import GHC.TypeLits
import Prelude hiding (init)
import qualified Data.ByteArray as BA
import qualified Data.ByteArray.Encoding as BA
import qualified Data.Memory.PtrMethods as BA

--------------------------------------------------------------------------------

-- | Output from BLAKE3 algorithm, of @len@ bytes.
--
-- The default digest length for BLAKE3 is 'DEFAULT_DIGEST_LEN'.
newtype Digest (len :: Nat) = Digest BA.ScrubbedBytes
  deriving newtype ( Eq -- ^ Constant time.
                   , BA.ByteArrayAccess)

-- | Base 16 (hexadecimal).
instance Show (Digest len) where
  show (Digest x) = showBase16 x

-- | Allocate a 'Digest'.
--
-- The memory is wiped and freed as soon the 'Digest' becomes unused.
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
keyLen = fromIntegral (natVal (Proxy @KEY_LEN))

-- | Obtain a 'Key' for use in BLAKE3 keyed hashing.
--
-- See 'BLAKE3.hashKeyed'.
key
  :: BA.ByteArrayAccess bin
  => bin -- ^ Key bytes. Must have length 'KEY_LEN'.
  -> Maybe Key -- ^
key bin | BA.length bin == keyLen = Just (Key (BA.convert bin))
        | otherwise = Nothing

-- | Allocate a 'Key'.
--
-- The memory is wiped and freed as soon as the 'Digest' becomes unused.
allocRetKey
  :: forall a
  .  (Ptr Word8 -> IO a) -- ^ Initialize 'KEY_LEN' bytes.
  -> IO (a, Key)
allocRetKey g = do
  (a, bs) <- BA.allocRet keyLen g
  pure (a, Key bs)

--------------------------------------------------------------------------------

-- | Context for BLAKE3 key derivation. Obtain with 'context'.
newtype Context = Context BA.Bytes -- ^ NUL-terminated 'CString'.
  deriving newtype (Eq)

-- We exclude the NUL-terminating byte. That's internal.
instance BA.ByteArrayAccess Context where
  length (Context x) = max 0 (BA.length x - 1)
  withByteArray c@(Context x) = BA.withByteArray (BA.takeView x (BA.length c))

-- | Base 16 (hexadecimal).
instance Show Context where
  show = showBase16

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

showBase16 :: BA.ByteArrayAccess x => x -> String
showBase16 = fmap (toEnum . fromIntegral)
           . BA.unpack @BA.ScrubbedBytes
           . BA.convertToBase BA.Base16

--------------------------------------------------------------------------------

-- | Initialize a 'Hasher'.
init :: Ptr Hasher -> IO () -- ^
init = c_init

-- | Initialize a 'Hasher' in keyed mode.
initKeyed :: Ptr Hasher -> Key -> IO () -- ^
initKeyed ph key0 =
  BA.withByteArray key0 $ \pkey ->
  c_init_keyed ph pkey

-- | Initialize a 'Hasher' in derivation mode.
--
-- The input key material must be provided afterwards, using 'update'.
initDerive :: Ptr Hasher -> Context -> IO () -- ^
initDerive ph (Context ctx) =
  BA.withByteArray ctx $ \pc ->
  c_init_derive_key ph pc

-- | Update 'Hasher' state with new data.
update :: forall bin. BA.ByteArrayAccess bin => Ptr Hasher -> [bin] -> IO () -- ^
update ph bins =
  for_ bins $ \bin ->
  BA.withByteArray bin $ \pbin ->
  c_update ph pbin (fromIntegral (BA.length bin))

-- | Finalize 'Hasher' state and obtain a digest.
finalize :: forall len. KnownNat len => Ptr Hasher -> IO (Digest len) -- ^
finalize ph =
  fmap snd $ allocRetDigest $ \pd ->
  c_finalize ph pd (fromIntegral (natVal (Proxy @len)))

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

--------------------------------------------------------------------------------

-- | BLAKE3 internal state.
--
-- Obtain with 'BLAKE3.hasher', 'BLAKE3.hasherKeyed', 'allocRetHasher'.
newtype Hasher = Hasher BA.ScrubbedBytes
  deriving newtype (BA.ByteArrayAccess)

-- | Modify a 'Hasher'.
modifyHasher :: Hasher -> (Ptr Hasher -> IO a) -> IO a -- ^
modifyHasher = BA.withByteArray

hasherSize :: Int
hasherSize = fromIntegral (natVal (Proxy @HASHER_SIZE))

-- | Allocate a 'Hasher'.
--
-- The memory is wiped and freed as soon as the 'Hasher' becomes unused.
allocRetHasher
  :: (Ptr Hasher -> IO a) -- ^ Initialize 'HASHER_SIZE' bytes.
  -> IO (a, Hasher) -- ^
allocRetHasher g = do
  (a, sb) <- BA.allocRet hasherSize g
  pure (a, Hasher sb)

-- | Copy an inmutable 'Hasher'.
copyHasher :: Hasher -> IO Hasher -- ^
copyHasher h0 =
  fmap snd $ allocRetHasher $ \pdst ->
  poke pdst h0

-- | When allocating a 'Hasher', prefer to use  'allocRetHasher', which
-- wipes and releases the memory as soon it becomes unused.
instance Storable Hasher where
  sizeOf _ = hasherSize
  alignment _ = fromIntegral (natVal (Proxy @HASHER_ALIGNMENT))
  peek ps = fmap Hasher $ BA.alloc hasherSize $ \pd -> copyArray pd ps 1
  poke pd (Hasher src) = BA.withByteArray src $ \ps -> copyArray pd ps 1

--------------------------------------------------------------------------------

-- | @void blake3_hasher_init(blake3_hasher *self)@
foreign import ccall unsafe
  "blake3.h blake3_hasher_init"
  c_init
    :: Ptr Hasher  -- ^ You can obtain with 'allocRetDigest'. Otherwise,
                   -- any chunk of 'HASHER_SIZE' bytes aligned to
                   -- 'HASHER_ALIGNMENT' will do.
    -> IO ()

-- | @void blake3_hasher_init_keyed(blake3_hasher *self, const uint8_t key['KEY_LEN'])@
foreign import ccall unsafe
  "blake3.h blake3_hasher_init_keyed"
  c_init_keyed
    :: Ptr Hasher  -- ^ You can obtain with 'allocRetDigest'. Otherwise,
                   -- any chunk of 'HASHER_SIZE' bytes aligned to
                   -- 'HASHER_ALIGNMENT' will do.
    -> Ptr Word8   -- ^ Key of length 'KEY_LEN'.
    -> IO ()

-- | @void blake3_hasher_init_derive_key(blake3_hasher *self, const char *context)@
foreign import ccall unsafe
  "blake3.h blake3_hasher_init_derive_key"
  c_init_derive_key
    :: Ptr Hasher  -- ^ You can obtain with 'allocRetDigest'. Otherwise,
                   -- any chunk of 'HASHER_SIZE' bytes aligned to
                   -- 'HASHER_ALIGNMENT' will do.
    -> CString  -- ^ Context.
    -> IO ()

-- | @void blake3_hasher_update(blake3_hasher *self, const void *input, size_t input_len)@
foreign import ccall unsafe
  "blake3.h blake3_hasher_update"
  c_update
    :: Ptr Hasher -- ^ Must have been previously initialized. See 'c_init',
                  -- 'c_init_keyed', 'c_init_derive_key'.
    -> Ptr Word8  -- ^ Data.
    -> CSize      -- ^ Data length.
    -> IO ()

-- | @void blake3_hasher_finalize(const blake3_hasher *self, uint8_t *out, size_t out_len)@
foreign import ccall unsafe
  "blake3.h blake3_hasher_finalize"
  c_finalize
    :: Ptr Hasher -- ^ Must have been previously initialized. See 'c_init',
                  -- 'c_init_keyed', 'c_init_derive_key'.
    -> Ptr Word8  -- ^ Out.
    -> CSize      -- ^ Out length.
    -> IO ()

-- | @void blake3_hasher_finalize_seek(const blake3_hasher *self, uint64_t seek, uint8_t *out, size_t out_len)@
foreign import ccall unsafe
  "blake3.h blake3_hasher_finalize_seek"
  c_finalize_seek
    :: Ptr Hasher  -- ^ Must have been previously initialized. See 'c_init',
                   -- 'c_init_keyed', 'c_init_derive_key'.
    -> Word64      -- ^ Seek position.
    -> Ptr Word8   -- ^ Out.
    -> CSize       -- ^ Out length.
    -> IO ()

