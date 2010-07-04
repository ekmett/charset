{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.CharSet
-- Copyright   :  (c) Edward Kmett 2010
-- License     :  BSD3
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  portable (CPP)
--
-- Fast complementable character sets
--
-- Designed to be imported qualified:
-- 
-- > import Data.CharSet (CharSet)
-- > import qualified Data.CharSet as CharSet
-------------------------------------------------------------------------------

module Data.CharSet 
    ( 
    -- * Set type
      CharSet
    -- * Operators
    , (\\)
    -- * Query
    , null
    , size
    , member
    , notMember
    , overlaps, isSubsetOf
    , isComplemented 
    -- * Construction
    , build
    , empty
    , singleton
    , full
    , insert
    , delete
    , complement
    , range
    -- * Combine
    , union
    , intersection
    , difference
    -- * Filter
    , filter
    , partition
    -- * Map
    , map
    -- * Fold
    , fold
    -- * Conversion
    -- ** List
    , toList
    , fromList
    -- ** Ordered list
    , toAscList
    , fromAscList
    , fromDistinctAscList
    -- ** IntMaps
    , fromCharSet
    , toCharSet
    -- ** Array
    , toArray
    ) where

import Data.Array.Unboxed hiding (range)
import Data.Data
import Data.Function (on)
import Data.IntSet (IntSet)
import Data.Monoid (Monoid(..))
import qualified Data.IntSet as I
import qualified Data.List as L
import Prelude hiding (filter, map, null)
import qualified Prelude as P
import Text.Read

data CharSet = P IntSet | N IntSet

(\\) :: CharSet -> CharSet -> CharSet
(\\) = difference

build :: (Char -> Bool) -> CharSet
build p = fromDistinctAscList $ P.filter p [minBound .. maxBound]
{-# INLINE build #-}

map :: (Char -> Char) -> CharSet -> CharSet
map f (P i) = P (I.map (fromEnum . f . toEnum) i)
map f (N i) = fromList $ P.map f $ P.filter (\x -> fromEnum x `I.notMember` i) [ul..uh] 
{-# INLINE map #-}

isComplemented :: CharSet -> Bool
isComplemented (P _) = False
isComplemented (N _) = True
{-# INLINE isComplemented #-}

toList :: CharSet -> String
toList (P i) = P.map toEnum (I.toList i)
toList (N i) = P.filter (\x -> fromEnum x `I.notMember` i) [ul..uh]
{-# INLINE toList #-}

toAscList :: CharSet -> String
toAscList (P i) = P.map toEnum (I.toAscList i)
toAscList (N i) = P.filter (\x -> fromEnum x `I.notMember` i) [ul..uh]
{-# INLINE toAscList #-}
    
empty :: CharSet
empty = P I.empty
{-# INLINE empty #-}

singleton :: Char -> CharSet
singleton = P . I.singleton . fromEnum
{-# INLINE singleton #-}

full :: CharSet
full = N I.empty
{-# INLINE full #-}

null :: CharSet -> Bool
null (P i) = I.null i
null (N i) = I.size i == numChars -- badly normalized!
{-# INLINE null #-}

size :: CharSet -> Int
size (P i) = I.size i
size (N i) = numChars - I.size i
{-# INLINE size #-}

insert :: Char -> CharSet -> CharSet
insert c (P i) = P (I.insert (fromEnum c) i)
insert c (N i) = P (I.delete (fromEnum c) i)
{-# INLINE insert #-}

range :: Char -> Char -> CharSet
range a b 
    | a <= b = fromDistinctAscList [a..b]
    | otherwise = empty

delete :: Char -> CharSet -> CharSet
delete c (P i) = P (I.delete (fromEnum c) i)
delete c (N i) = N (I.insert (fromEnum c) i)
{-# INLINE delete #-}

complement :: CharSet -> CharSet
complement (P i) = N i
complement (N i) = P i
{-# INLINE complement #-}

union :: CharSet -> CharSet -> CharSet
union (P i) (P j) = P (I.union i j)
union (P i) (N j) = N (I.difference j i)
union (N i) (P j) = N (I.difference i j)
union (N i) (N j) = N (I.intersection i j)
{-# INLINE union #-}

intersection :: CharSet -> CharSet -> CharSet
intersection (P i) (P j) = P (I.intersection i j)
intersection (P i) (N j) = P (I.difference i j)
intersection (N i) (P j) = P (I.difference j i)
intersection (N i) (N j) = N (I.union i j)
{-# INLINE intersection #-}

difference :: CharSet -> CharSet -> CharSet 
difference (P i) (P j) = P (I.difference i j)
difference (P i) (N j) = P (I.intersection i j)
difference (N i) (P j) = N (I.union i j)
difference (N i) (N j) = P (I.difference j i)
{-# INLINE difference #-}

member :: Char -> CharSet -> Bool
member c (P i) = I.member (fromEnum c) i
member c (N i) = I.notMember (fromEnum c) i
{-# INLINE member #-}

notMember :: Char -> CharSet -> Bool
notMember c (P i) = I.notMember (fromEnum c) i
notMember c (N i) = I.member (fromEnum c) i
{-# INLINE notMember #-}

fold :: (Char -> b -> b) -> b -> CharSet -> b
fold f z (P i) = I.fold (f . toEnum) z i
fold f z (N i) = foldr f z $ P.filter (\x -> fromEnum x `I.notMember` i) [ul..uh]
{-# INLINE fold #-}

filter :: (Char -> Bool) -> CharSet -> CharSet 
filter p (P i) = P (I.filter (p . toEnum) i)
filter p (N i) = N $ foldr (I.insert) i $ P.filter (\x -> (x `I.notMember` i) && not (p (toEnum x))) [ol..oh]
{-# INLINE filter #-}

partition :: (Char -> Bool) -> CharSet -> (CharSet, CharSet)
partition p (P i) = (P l, P r)
    where (l,r) = I.partition (p . toEnum) i
partition p (N i) = (N (foldr I.insert i l), N (foldr I.insert i r))
    where (l,r) = L.partition (p . toEnum) $ P.filter (\x -> x `I.notMember` i) [ol..oh]
{-# INLINE partition #-}

overlaps :: CharSet -> CharSet -> Bool
overlaps (P i) (P j) = not (I.null (I.intersection i j))
overlaps (P i) (N j) = not (I.isSubsetOf j i)
overlaps (N i) (P j) = not (I.isSubsetOf i j)
overlaps (N i) (N j) = any (\x -> I.notMember x i && I.notMember x j) [ol..oh] -- not likely
{-# INLINE overlaps #-}

isSubsetOf :: CharSet -> CharSet -> Bool
isSubsetOf (P i) (P j) = I.isSubsetOf i j
isSubsetOf (P i) (N j) = I.null (I.intersection i j)
isSubsetOf (N i) (P j) = all (\x -> I.member x i && I.member x j) [ol..oh]-- not bloody likely
isSubsetOf (N i) (N j) = I.isSubsetOf j i
{-# INLINE isSubsetOf #-}

fromList :: String -> CharSet 
fromList = P . I.fromList . P.map fromEnum
{-# INLINE fromList #-}

fromAscList :: String -> CharSet
fromAscList = P . I.fromAscList . P.map fromEnum
{-# INLINE fromAscList #-}

fromDistinctAscList :: String -> CharSet
fromDistinctAscList = P . I.fromDistinctAscList . P.map fromEnum
{-# INLINE fromDistinctAscList #-}

-- isProperSubsetOf :: CharSet -> CharSet -> Bool
-- isProperSubsetOf (P i) (P j) = I.isProperSubsetOf i j
-- isProperSubsetOf (P i) (N j) = null (I.intersection i j) && ...
-- isProperSubsetOf (N i) (N j) = I.isProperSubsetOf j i

ul, uh :: Char
ul = minBound
uh = maxBound
{-# INLINE ul #-}
{-# INLINE uh #-}

ol, oh :: Int
ol = fromEnum ul
oh = fromEnum uh
{-# INLINE ol #-}
{-# INLINE oh #-}

numChars :: Int
numChars = oh - ol + 1
{-# INLINE numChars #-}

instance Typeable CharSet where
    typeOf _ = mkTyConApp charSetTyCon []

charSetTyCon :: TyCon
charSetTyCon = mkTyCon "Data.CharSet.CharSet"
{-# NOINLINE charSetTyCon #-}

instance Data CharSet where
    gfoldl k z set | isComplemented set = z complement `k` complement set
                   | otherwise          = z fromList `k` toList set

    toConstr set 
        | isComplemented set = complementConstr
        | otherwise = fromListConstr

    dataTypeOf _ = charSetDataType

    gunfold k z c = case constrIndex c of
        1 -> k (z fromList)
        2 -> k (z complement)
        _ -> error "gunfold"

fromListConstr :: Constr
fromListConstr   = mkConstr charSetDataType "fromList" [] Prefix
{-# NOINLINE fromListConstr #-}

complementConstr :: Constr
complementConstr = mkConstr charSetDataType "complement" [] Prefix
{-# NOINLINE complementConstr #-}

charSetDataType :: DataType
charSetDataType  = mkDataType "Data.CharSet.CharSet" [fromListConstr, complementConstr]
{-# NOINLINE charSetDataType #-}

-- returns an intset and if the intset should be complemented to obtain the contents of the CharSet
fromCharSet :: CharSet -> (Bool, IntSet)
fromCharSet (P i) = (False, i)
fromCharSet (N i) = (True, i) 
{-# INLINE fromCharSet #-}

toCharSet :: IntSet -> CharSet
toCharSet = P
{-# INLINE toCharSet #-}

instance Eq CharSet where
    (==) = (==) `on` toAscList

instance Ord CharSet where
    compare = compare `on` toAscList

instance Bounded CharSet where
    minBound = empty
    maxBound = full

toArray :: CharSet -> UArray Char Bool
toArray set = array (minBound, maxBound) $ fmap (\x -> (x, x `member` set)) [minBound .. maxBound]
 
instance Show CharSet where
   showsPrec d i
        | isComplemented i = showParen (d > 10) $ showString "complement " . showsPrec 11 (complement i)
        | otherwise        = showParen (d > 10) $ showString "fromDistinctAscList " . showsPrec 11 (toAscList i)

instance Read CharSet where
#ifdef __GLASGOW_HASKELL__ 
    readPrec = parens $ complemented +++ normal 
      where
        complemented = prec 10 $ do 
                Ident "complement" <- lexP
                complement `fmap` step readPrec
        normal = prec 10 $ do
                Ident "fromDistinctAscList" <- lexP
                fromDistinctAscList `fmap` step readPrec
#else
    readsPrec d r = 
        readParen (d > 10) (\r -> [ (complement m, t) 
                                  | ("complement", s) <- lex r
                                  , (m, t) <- readsPrec 11 s]) r
     ++ readParen (d > 10) (\r -> [ (fromDistinctAscList m, t) 
                                  | ("fromDistinctAscList", s) <- lex r
                                  , (m, t) <- readsPrec 11 s]) r
#endif

instance Monoid CharSet where
    mempty = empty
    mappend = union
