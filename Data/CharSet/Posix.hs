-----------------------------------------------------------------------------
-- |
-- Module      :  Data.CharSet.Posix
-- Copyright   :  (c) Edward Kmett 2011-2012
-- License     :  BSD3
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-------------------------------------------------------------------------------

module Data.CharSet.Posix
    ( posixAscii
    , lookupPosixAsciiCharSet
    , posixUnicode
    , lookupPosixUnicodeCharSet
    ) where

import Data.CharSet.Posix.Ascii
import Data.CharSet.Posix.Unicode
import Prelude ()
