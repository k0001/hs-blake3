{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | IO and low level tools.
module BLAKE3.IO
  ( -- * Hashing
    hash
  , init
  , update
  , finalize
  , finalizeSeek
    -- * Digest
  , Digest(..)
    -- * Keyed hashing
  , Key
  , key
    -- * Key derivation
  , initDerive
    -- * Hasher
  , Hasher
  , modifyHasher
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
  , c_init_derive_key_raw
  , c_update
  , c_finalize
  , c_finalize_seek
  )
  where

import Data.Foldable
import Data.Proxy
import Data.String
import Data.Word
import Foreign.C.Types
import Foreign.Marshal.Array (copyArray)
import Foreign.Ptr
import Foreign.Storable
import GHC.TypeLits
import Prelude hiding (init)
import qualified Data.ByteArray as BA
import qualified Data.ByteArray.Sized as BAS
import qualified Data.ByteArray.Encoding as BA

--------------------------------------------------------------------------------

-- | Output from BLAKE3 algorithm, of @len@ bytes.
--
-- The default digest length for BLAKE3 is 'DEFAULT_DIGEST_LEN'.
newtype Digest (len :: Nat)
  = Digest (BAS.SizedByteArray len BA.ScrubbedBytes)
  deriving newtype ( Eq -- ^ Constant time.
                   , Ord
                   , BA.ByteArrayAccess
                   , BAS.ByteArrayN len )

-- | Base 16 (hexadecimal).
instance Show (Digest len) where
  show (Digest x) = showBase16 (BAS.unSizedByteArray x)

-- | When allocating a 'Digest', prefer to use 'BAS.alloc', which
-- wipes and releases the memory as soon it becomes unused.
instance forall len. KnownNat len => Storable (Digest len) where
  sizeOf _ = fromIntegral (natVal (Proxy @len))
  alignment _ = 8 -- Not sure.
  peek ps = BAS.alloc $ \pd -> copyArray pd ps 1
  poke pd src = BA.withByteArray src $ \ps -> copyArray pd ps 1

--------------------------------------------------------------------------------

-- | Key used for keyed hashing mode.
--
-- Obtain with 'BLAKE3.key'.
--
-- See 'BLAKE3.hashKeyed'.
data Key where
  -- | We store things this way to avoid unnecessary conversions between
  -- different 'BA.ByteArrayAccess' when using 'key' for reading a 'Key'
  -- from a third party source.
  Key :: BA.ByteArrayAccess x => x -> Key

-- | Constant time.
instance Eq Key where
  (==) = BA.constEq
  {-# INLINE (==) #-}

-- | Base 16 (hexadecimal).
instance Show Key where
  show (Key x) = showBase16 x

-- | Length is 'KEY_LEN'.
instance BA.ByteArrayAccess Key where
  length (Key x) = BA.length x
  withByteArray (Key x) = BA.withByteArray x

-- | Allocate a 'Key'.
--
-- The memory is wiped and freed as soon as the 'Key' becomes unused.
instance BAS.ByteArrayN KEY_LEN Key where
  allocRet _ g = do
    (a, bs :: BA.ScrubbedBytes) <- BA.allocRet keyLen g
    pure (a, Key bs)

-- | When allocating a 'Key', prefer to use 'BAS.alloc', which
-- wipes and releases the memory as soon it becomes unused.
instance Storable Key where
  sizeOf _ = keyLen
  alignment _ = 8 -- Not sure.
  peek ps = BAS.alloc $ \pd -> copyArray pd ps 1
  poke pd src = BA.withByteArray src $ \ps -> copyArray pd ps 1

-- | Obtain a 'Key' for use in BLAKE3 keyed hashing.
--
-- See 'BLAKE3.hashKeyed'.
key
  :: BA.ByteArrayAccess bin
  => bin -- ^ Key bytes. Must have length 'KEY_LEN'.
  -> Maybe Key -- ^
key bin | BA.length bin /= keyLen = Nothing
        | otherwise = Just (Key bin)

--------------------------------------------------------------------------------

showBase16 :: BA.ByteArrayAccess x => x -> String
showBase16 = fmap (toEnum . fromIntegral)
           . BA.unpack @BA.ScrubbedBytes
           . BA.convertToBase BA.Base16

--------------------------------------------------------------------------------

-- | BLAKE3 hashing.
hash
  :: forall len digest bin
  .  (BAS.ByteArrayN len digest, BA.ByteArrayAccess bin)
  => Maybe Key  -- ^ Whether to use keyed hashing mode (for MAC, PRF).
  -> [bin]      -- ^ Data to hash.
  -> IO digest  -- ^ The @digest@ type could be @'Digest' len@.
hash yk bins = do
  (dig, _ :: Hasher) <- BAS.allocRet Proxy $ \ph -> do
    init ph yk
    update ph bins
    finalize ph
  pure dig

-- | Initialize a 'Hasher'.
init
  :: Ptr Hasher  -- ^ Obtain with 'BAS.alloc' or similar. It will be mutated.
  -> Maybe Key   -- ^ Whether to use keyed hashing mode (for MAC, PRF).
  -> IO ()
init ph Nothing     = c_init ph
init ph (Just key0) = BA.withByteArray key0 $ \pkey ->
                      c_init_keyed ph pkey

-- | Initialize a 'Hasher' in derivation mode.
--
-- The input key material must be provided afterwards, using 'update'.
initDerive
  :: forall context
  .  BA.ByteArrayAccess context
  => Ptr Hasher  -- ^ Obtain with 'BAS.alloc' or similar. It will be mutated.
  -> context
  -> IO ()
initDerive ph ctx =
  BA.withByteArray ctx $ \pc ->
  c_init_derive_key_raw ph pc (fromIntegral (BA.length ctx))

-- | Update 'Hasher' state with new data.
update
  :: forall bin
  .  BA.ByteArrayAccess bin
  => Ptr Hasher -- ^ Obtain with 'modifyHasher'. It will be mutated.
  -> [bin]
  -> IO ()
update ph bins =
  for_ bins $ \bin ->
  BA.withByteArray bin $ \pbin ->
  c_update ph pbin (fromIntegral (BA.length bin))

-- | Finalize incremental hashing and obtain a the BLAKE3 output of the
-- specified @len@gth.
finalize
  :: forall len output
  .  BAS.ByteArrayN len output
  => Ptr Hasher -- ^ Obtain with 'modifyHasher'. It will be mutated.
  -> IO output  -- ^ The @output@ type could be @'Digest' len@.
finalize ph =
  BAS.alloc $ \pd ->
  c_finalize ph pd (fromIntegral (natVal (Proxy @len)))

-- | Finalize incremental hashing and obtain the specified @len@gth of BLAKE3
-- output starting at the specified offset.
--
-- @
-- 'finalize' h = 'finalizeSeek' h 0
-- @
finalizeSeek
  :: forall len output
  .  BAS.ByteArrayN len output
  => Ptr Hasher -- ^ Obtain with 'modifyHasher'. It will be mutated.
  -> Word64     -- ^ BLAKE3 output offset.
  -> IO output
finalizeSeek ph pos =
  BAS.alloc $ \pd ->
  c_finalize_seek ph pos pd (fromIntegral (natVal (Proxy @len)))

--------------------------------------------------------------------------------

type HASHER_ALIGNMENT = 8

-- | In bytes.
type HASHER_SIZE = 1912

hasherSize :: Int
hasherSize = fromIntegral (natVal (Proxy @HASHER_SIZE))

-- | In bytes.
type KEY_LEN = 32

keyLen :: Int
keyLen = fromIntegral (natVal (Proxy @KEY_LEN))

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
-- Obtain with 'BLAKE3.hasher', 'BLAKE3.hasherKeyed'.
newtype Hasher = Hasher (BAS.SizedByteArray HASHER_SIZE BA.ScrubbedBytes)
  deriving newtype
    ( Eq -- ^ Constant time.
    , BA.ByteArrayAccess -- ^ Length is 'HASHER_SIZE'.
    , BAS.ByteArrayN HASHER_SIZE
      -- ^ Allocate a 'Hasher'.
      -- The memory is wiped and freed as soon as the 'Hasher' becomes unused.
    )

-- | Base 16 (hexadecimal).
instance Show Hasher where
  show = showBase16

-- | Obtain a @'Ptr' 'Hasher'@ to use with functions like 'initDerive', etc.
modifyHasher
  :: Hasher
  -> (Ptr Hasher -> IO a) -- ^ 'HASHER_SIZE' bytes.
  -> IO a
modifyHasher = BA.withByteArray

-- | When allocating a 'Hasher', prefer to use 'BAS.alloc', which
-- wipes and releases the memory as soon it becomes unused.
instance Storable Hasher where
  sizeOf _ = hasherSize
  alignment _ = fromIntegral (natVal (Proxy @HASHER_ALIGNMENT))
  peek ps = BAS.alloc $ \pd -> copyArray pd ps 1
  poke pd src = BA.withByteArray src $ \ps -> copyArray pd ps 1

--------------------------------------------------------------------------------

-- | @void blake3_hasher_init(blake3_hasher *self)@
foreign import ccall unsafe
  "blake3.h blake3_hasher_init"
  c_init
    :: Ptr Hasher  -- ^ You can obtain with 'BAS.alloc'.
                   -- Otherwise, any chunk of 'HASHER_SIZE' bytes aligned to
                   -- 'HASHER_ALIGNMENT' will do.
    -> IO ()

-- | @void blake3_hasher_init_keyed(blake3_hasher *self, const uint8_t key['KEY_LEN'])@
foreign import ccall unsafe
  "blake3.h blake3_hasher_init_keyed"
  c_init_keyed
    :: Ptr Hasher  -- ^ You can obtain with 'BAS.alloc'.
                   -- Otherwise, any chunk of 'HASHER_SIZE' bytes aligned to
                   -- 'HASHER_ALIGNMENT' will do.
    -> Ptr Word8   -- ^ You can obtain with 'BAS.alloc'.
                   -- Otherwise, any chunk of length 'KEY_LEN' will do.
    -> IO ()


-- | @void blake3_hasher_init_derive_key_raw(blake3_hasher *self, const void *context, size_t context_len)@
foreign import ccall unsafe
  "blake3.h blake3_hasher_init_derive_key_raw"
  c_init_derive_key_raw
    :: Ptr Hasher  -- ^ You can obtain with 'BAS.alloc'.
                   -- Otherwise, any chunk of 'HASHER_SIZE' bytes aligned to
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

