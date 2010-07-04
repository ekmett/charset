-----------------------------------------------------------------------------
-- |
-- Module      :  Data.CharSet.Posix.Ascii
-- Copyright   :  (c) Edward Kmett 2010
-- License     :  BSD3
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-------------------------------------------------------------------------------

module Data.CharSet.Posix.Ascii
    ( posixAscii
    -- * POSIX ASCII \"classes\"
    , alnum, alpha, blank, cntrl, digit, graph, print, word, punct, space, upper, lower, xdigit
    ) where

import Prelude hiding (print)
import Data.CharSet
import Data.Map (Map)
import qualified Data.Map as Map

alnum, alpha, blank, cntrl, digit, graph, print, word, punct, space, upper, lower, xdigit :: CharSet
alnum = alpha `union` digit
alpha = lower `union` upper
blank = fromList " \t"
cntrl = insert '\x7f' $ range '\x00' '\x1f'
digit = range '0' '9'
lower = range 'a' 'z'
upper = range 'A' 'Z'
graph = range '\x21' '\x7e'
print = insert '\x20' graph
word  = insert '_' alnum
punct = fromList "-!\"#$%&'()*+,./:;<=>?@[\\]^_`{|}~"
space = fromList " \t\r\n\v\f"
xdigit = digit `union` range 'a' 'f' `union` range 'A' 'F'

-- :digit:, etc.
posixAscii :: Map String CharSet
posixAscii = Map.fromList
    [ ("alnum", alnum)
    , ("alpha", alpha)
    , ("blank", blank)
    , ("cntrl", cntrl)
    , ("digit", digit)
    , ("graph", graph) 
    , ("print", print)
    , ("word",  word)
    , ("punct", punct)
    , ("space", space)
    , ("upper", upper)
    , ("lower", lower)
    , ("xdigit", xdigit)
    ]
