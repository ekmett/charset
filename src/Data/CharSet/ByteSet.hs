{-# LANGUAGE BangPatterns, MagicHash #-}
{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.CharSet.ByteSet
-- Copyright   :  Edward Kmett 2011
--                Bryan O'Sullivan 2008
-- License     :  BSD3
--
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (BangPatterns, MagicHash)
--
-- Fast set membership tests for byte values. The set representation is
-- unboxed for efficiency and uses one bit per byte to represent the presence
-- or absence of a byte in the set.
--
-- This is a fairly minimal API. You probably want to use CharSet.
-----------------------------------------------------------------------------
module Data.CharSet.ByteSet
    (
    -- * Data type
      ByteSet(..)
    -- * Construction
    , fromList
    -- * Lookup
    , member
    ) where

import Data.Bits ((.&.), (.|.))
import Foreign.Storable (peekByteOff, pokeByteOff)
import GHC.Exts ( Int(I#), Word#, iShiftRA#, shiftL#
#if MIN_VERSION_base(4,16,0)
                , Word8#, word8ToWord#, wordToWord8#
#else
                , narrow8Word#
#endif
                )
import GHC.Word (Word8(W8#))
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as I
import qualified Data.ByteString.Unsafe as U

#if MIN_VERSION_base(4,8,0)
import Foreign.Marshal.Utils (fillBytes)
#endif

newtype ByteSet = ByteSet B.ByteString deriving (Eq, Ord, Show)

-- | Representation of the index of a bit inside a bytestring
-- in terms of a byte index and a bit index inside the byte
data I = I
    {-# UNPACK #-} !Int         -- byte index
    {-# UNPACK #-} !Word8       -- bit index

shiftR :: Int -> Int -> Int
shiftR (I# x#) (I# i#) = I# (x# `iShiftRA#` i#)

shiftL :: Word8 -> Int -> Word8
shiftL (W8# x#) (I# i#) = W8# (narrow8WordCompat# (word8ToWordCompat# x# `shiftL#` i#))

-- | Convert a bit index to a byte index and bit index inside the byte
index :: Int -> I
index i = I (i `shiftR` 3) (1 `shiftL` (i .&. 7))
{-# INLINE index #-}

fromList :: [Word8] -> ByteSet
fromList s0 = ByteSet $ I.unsafeCreate 32 $ \t -> do
  _ <-
#if MIN_VERSION_base(4,8,0)
    fillBytes t 0 32
#else
    I.memset t 0 32
#endif
  let go [] = return ()
      go (c:cs) = do
        prev <- peekByteOff t byte :: IO Word8
        pokeByteOff t byte (prev .|. bit)
        go cs
        where I byte bit = index (fromIntegral c)
  go s0

-- | Check the set for membership.
member :: Word8 -> ByteSet -> Bool
member w (ByteSet t) = U.unsafeIndex t byte .&. bit /= 0
  where
    I byte bit = index (fromIntegral w)

#if MIN_VERSION_base(4,16,0)
word8ToWordCompat# :: Word8# -> Word#
word8ToWordCompat# = word8ToWord#

narrow8WordCompat# :: Word# -> Word8#
narrow8WordCompat# = wordToWord8#
#else
word8ToWordCompat# :: Word# -> Word#
word8ToWordCompat# x = x

narrow8WordCompat# :: Word# -> Word#
narrow8WordCompat# = narrow8Word#
#endif
