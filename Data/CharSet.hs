{-# LANGUAGE BangPatterns, CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.CharSet
-- Copyright   :  (c) Edward Kmett 2010
-- License     :  BSD3
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Encode unicode character sets as arbitrary precision floating point values
-- using the least character in the set as the exponent. Can efficiently represent
-- reasonably tightly grouped character sets, but may use up to 139KiB to represent
-- a particularly sparse set.
-- 
-- Designed to be imported qualified:
-- 
-- > import Data.CharSet (CharSet)
-- > import qualified Data.CharSet as CharSet
-------------------------------------------------------------------------------

module Data.CharSet
    ( 
    -- * CharSet
      CharSet
    , build
    -- * Manipulation
    , empty
    , singleton
    , full
    , union
    , intersection
    , complement
    , insert
    , delete
    , (\\)
    , fromList
    , fromDistinctAscList
    , toArray
    -- * Accessors
    , null
    , size
    , member
    , elem
    , notElem
    , isComplemented
    , toInteger
    -- * Builtins
    -- ** POSIX
    , posixAscii
    -- ** Unicode
    , UnicodeCategory(..)
    , unicodeCategories
    -- ** Data.Char classifiers
    , control, space, lower, upper, alpha, alphaNum
    , print, digit, octDigit, letter, mark, number
    , punctuation, symbol, separator, ascii, latin1, asciiUpper, asciiLower
    ) where

import Data.Array hiding (range)
import qualified Data.Bits as Bits
import Data.Bits hiding (complement)
import Data.Char
import Data.Data
import Data.Function (on)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid (Monoid(..))
import Prelude hiding (null, exponent, toInteger, elem, notElem, print, pi)
import Text.Read

data CharSet = CS
        { _countAtLeast  :: {-# UNPACK #-} !Int       -- ^ A conservative upper bound on the element count.
                                                      --   If negative, we are complemented with respect to the universe
        , _countAtMost   :: {-# UNPACK #-} !Int       -- ^ A conservative lower bound on the element count.
                                                      --   If negative, we are complemented with respect to the universe
        , _count         :: Int                       -- ^ Lazy element count used when the above two disagree. O(1) environment size
        , exponent       :: {-# UNPACK #-} !Int       -- ^ Low water mark. index of the least element potentially in the set.
        , _hwm           :: {-# UNPACK #-} !Int       -- ^ High water mark. index of the greatest element potentially in the set.
        , mantissa       :: {-# UNPACK #-} !Integer   -- ^ the set of bits starting from the exponent.
                                                      --   if negative, then we are complemented with respect to universe
        }


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

-- | Internal smart constructor. Forces count whenever it is pigeonholed.
bs :: Int -> Int -> Int -> Int -> Int -> Integer -> CharSet
bs !a !b c !l !h !m | a == b = CS a a a l h m 
                    | otherwise = CS a b c l h m 
{-# INLINE bs #-}

-- | /O(d)/ where /d/ is absolute deviation in fromEnum over the set
toList :: CharSet -> String
toList (CS _ _ _ l h m) 
    | m < 0 = map toEnum [ol..max (pred l) ol] ++ toList' l (map toEnum [min (succ h) oh..oh])
    | otherwise = toList' 0 []
    where
        toList' :: Int -> String -> String
        toList' !n t | n > h = t
                     | testBit m (n - l) = toEnum n : toList' (n+1) t
                     | otherwise         = toList' (n+1) t
{-# INLINE toList #-}

-- | /O(1)/ The empty set. Permits /O(1)/ null and size.
empty :: CharSet
empty = CS 0 0 0 0 0 0 
{-# INLINE empty #-}

-- | /O(1)/ Construct a @CharSet@ with a single element. Permits /O(1)/ null and size
singleton :: Char -> CharSet 
singleton x = CS 1 1 1 e e 1 where e = fromEnum x
{-# INLINE singleton #-}

-- | /O(1|d)/ Is the 'CharSet' empty? May be faster than checking if @'size' == 0@ after union.
--   Operations that require a recount are noted.
null :: CharSet -> Bool
null (CS a b c _ _ _) 
    | a > 0 = False
    | b == 0 = True
    | otherwise = c == 0 
{-# INLINE null #-}

-- | /O(1|d)/ The number of elements in the bit set.
size :: CharSet -> Int
size (CS a b c _ _ m)
    | (a == b) && (m >= 0) = a
    | a == b = oh - ol - a 
    | m >= 0 = c
    | otherwise = oh - ol - c 
{-# INLINE size #-}

-- | /O(d)/ A 'CharSet' containing every member of the enumeration of @a@.
full :: CharSet
full = complement empty 
{-# INLINE full #-}

-- | /O(d)/ Complements a 'CharSet' with respect to the bounds of @a@. Preserves order of 'null' and 'size'
complement :: CharSet -> CharSet 
complement (CS a b c l h m) = CS (Bits.complement b) (Bits.complement a) (Bits.complement c) l h (Bits.complement m)
{-# INLINE complement #-}

-- | /O(d * n)/ Make a 'CharSet' from a list of items.
fromList :: String -> CharSet
fromList = foldr insert empty 
{-# INLINE fromList #-}

-- | /O(d * n)/ Make a 'CharSet' from a distinct ascending list of items
fromDistinctAscList :: String -> CharSet 
fromDistinctAscList [] = empty
fromDistinctAscList (c:cs) = fromDistinctAscList' cs 1 0 1 
    where
        l = fromEnum c
        fromDistinctAscList' :: String -> Int -> Int -> Integer -> CharSet
        fromDistinctAscList' [] !n !h !m  = CS n n n l h m 
        fromDistinctAscList' (c':cs') !n _ !m = fromDistinctAscList' cs' (n+1) h' (setBit m (h' - l))
            where
                h' = fromEnum c'
{-# INLINE fromDistinctAscList #-}

-- | /O(d)/ Insert a single element of type @a@ into the 'CharSet'. Preserves order of 'null' and 'size'
insert :: Char -> CharSet -> CharSet
insert x r@(CS a b c l h m) 
    | (m < 0) && (e < l) = r 
    | (m < 0) && (e > h) = r
    | e < l = bs (a+1) (b+1) (c+1) e h (shiftL m (l - e) .|. 1)
    | e > h = bs (a+1) (b+1) (c+1) l p (setBit m p)
    | testBit m p = r 
    | otherwise = bs (a+1) (b+1) (c+1) l h (setBit m p)
    where 
        e = fromEnum x
        p = e - l 
{-# INLINE insert #-}

-- | /O(d)/ Delete a single item from the 'CharSet'. Preserves order of 'null' and 'size'
delete :: Char -> CharSet -> CharSet
delete x r@(CS a b c l h m) 
    | (m < 0) && (e < l) = bs (a+1) (b+1) (c+1) e h (shiftL m (l - e) .&. Bits.complement 1)
    | (m < 0) && (e > h) = bs (a+1) (b+1) (c+1) l p (clearBit m p)
    | e < l       = r
    | e > h       = r
    | testBit m p = bs (a-1) (b-1) (c-1) l h (clearBit m p)
    | otherwise   = r
    where 
        e = fromEnum x
        p = e - l
{-# INLINE delete #-}

-- | /O(1)/ Test for membership in a 'CharSet'
member :: Char -> CharSet -> Bool
member x (CS _ _ _ l h m) 
    | e < l     = m < 0 
    | e > h     = m > 0
    | otherwise = testBit m (e - l)
    where 
        e = fromEnum x
{-# INLINE member #-}

{-
notMember :: Char -> CharSet -> Bool
notMember x - not . member x
{-# INLINE notMember #-}
-}

-- | /O(1)/ Alias for member
elem :: Char -> CharSet -> Bool
elem = member
{-# INLINE elem #-}

-- | /O(1)/ Alias for notMember
notElem :: Char -> CharSet -> Bool
notElem x = not . elem x
{-# INLINE notElem #-}

-- | /O(d)/ convert to an Integer representation. Discards negative elements
toInteger :: CharSet -> Integer
toInteger x = mantissa x `shift` exponent x
{-# INLINE toInteger #-}

-- | /O(d)/. May force 'size' to take /O(d)/ if ranges overlap, preserves order of 'null'
union :: CharSet -> CharSet -> CharSet 
union x@(CS _ _ _ l _ _) y@(CS _ _ _ l' _ _)
    | l' < l        = union' y x -- ensure left side has lower exponent
    | otherwise     = union' x y 
{-# INLINE union #-}

union' :: CharSet -> CharSet -> CharSet 
union' x@(CS a b c l h m) y@(CS a' b' c' l' h' m')
    | b == 0        = y                                                         -- fast empty union
    | b' == 0       = x                                                         -- fast empty union
    | a == -1       = full                                                      -- fast full union
    | a' == -1      = full                                                      -- fast full union
    | (m < 0) && (m' < 0) = complement (intersection' (complement x) (complement y))  -- appeal to intersection
    | m' < 0        = complement (diff (complement y) x)                        -- union with complement
    | m < 0         = complement (diff (complement x) y)                        -- union with complement
    | h < l'        = bs (a + a') (b + b') (c + c') l h' m''                    -- disjoint positive ranges
    | otherwise     = bs (a `max` a') (b + b') (recount m'') l (h `max` h') m'' -- overlapped positives
    where 
        m'' = m .|. shiftL m' (l' - l)

-- | /O(1)/ check to see if we are represented as a complemented 'CharSet'. 
isComplemented :: CharSet -> Bool
isComplemented = (<0) . mantissa 
{-# INLINE isComplemented #-}

-- | /O(d)/. May force 'size' and 'null' both to take /O(d)/.
intersection :: CharSet -> CharSet -> CharSet 
intersection x@(CS _ _ _ l _ _) y@(CS _ _ _ l' _ _)
    | l' < l = intersection' y x
    | otherwise = intersection' x y
{-# INLINE intersection #-}

-- | /O(d)/. May force 'size' and 'null' both to take /O(d)/.
intersection' :: CharSet -> CharSet -> CharSet 
intersection' x@(CS a b _ l h m) y@(CS a' b' _ l' h' m')
    | b == 0  = empty
    | b' == 0 = empty
    | a == -1 = y
    | a' == -1 = x
    | (m < 0) && (m' < 0) = complement (union' (complement x) (complement y))
    | m' < 0 = diff x (complement y) 
    | m < 0  = diff y (complement x) 
    | h < l' = empty 
    | otherwise = bs 0 (b `min` b') (recount m'') l'' (h `min` h') m''
    where
        l'' = max l l'
        m'' = shift m (l'' - l) .&. shift m' (l'' - l')

-- | Unsafe internal method for computing differences 
-- preconditions:
--  m >= 0, m' >= 0, a /= -1, a' /= -1, b /= 0, b' /= 0
diff :: CharSet -> CharSet -> CharSet 
diff x@(CS a _ _ l h m) (CS _ b' _ l' h' m') 
    | h < l' = x
    | h' < l = x
    | otherwise = bs (max (a - b') 0) a (recount m'') l h m''
    where 
        m'' = m .&. shift (Bits.complement m') (l' - l)

-- | /O(d)/. Preserves order of 'null'. May force /O(d)/ 'size'.
difference :: CharSet -> CharSet -> CharSet 
difference x@(CS a b _ _ _ m)  y@(CS a' b' _ _ _ m') 
   | a == -1       = complement y
   | a' == -1      = empty
   | b == 0        = empty
   | b' == 0       = x
   | (m < 0) && (m' < 0) = diff (complement y) (complement x)
   | m < 0         = complement (complement x `union` y)
   | m' < 0        = x `union` complement y 
   | otherwise     = diff x y
    
-- | /O(d)/. Preserves order of 'null'. May force /O(d)/ 'size'.
(\\) :: CharSet -> CharSet -> CharSet 
(\\) = difference

instance Eq CharSet where
    x@(CS _ _ _ l _ m) == y@(CS _ _ _ l' _ m')
        | signum m == signum m' = shift m (l - l'') == shift m' (l - l'') 
        | m' < 0 = y == x
        | otherwise = mask .&. shift m (l - ol) == shift m' (l - ol)
        where 
            l'' = min l l'
            mask = setBit 0 (oh - ol + 1) - 1

instance Ord CharSet where
    compare = compare `on` toInteger

instance Bounded CharSet where
    minBound = empty
    maxBound = CS n n n ol oh m
        where
            n = oh - ol + 1
            m = setBit 0 n - 1

-- | Return a charset based on a character range
range :: Char -> Char -> CharSet
range l h 
    | l <= h    = CS n n n l' h' m
    | otherwise = empty
    where 
        l' = fromEnum l
        h' = fromEnum h
        n = h' - l' + 1
        m = setBit 0 n - 1

-- | /O(d)/
recount :: Integer -> Int
recount !n 
    | n < 0     = Bits.complement (recount (Bits.complement n))
    | otherwise = recount' 0 0 
    where
        h = hwm n
        recount' !i !c
            | i > h = c
            | otherwise = recount' (i+1) (if testBit n i then c+1 else c)

-- | /O(d)/. Computes the equivalent of (truncate . logBase 2 . abs) extended with 0 at 0
-- This could be computed faster by directly appealing to GMP, but that is tricky in GHC.
hwm :: Integer -> Int
hwm !n 
    | n < 0 = hwm (-n)
    | n > 1 = scan p (2*p) 
    | otherwise = 0
    where
        p = probe 1
        -- incrementally compute 2^(2^(i+1)) until it exceeds n
        probe :: Int -> Int
        probe !i
            | bit (2*i) > n = i
            | otherwise     = probe (2*i)

        -- then binary search the powers for the highest set bit
        scan :: Int -> Int -> Int
        scan !l !h
            | l == h = l
            | bit (m+1) > n = scan l m
            | otherwise = scan (m+1) h
            where m = l + (h - l) `div` 2

toArray :: CharSet -> Array Char Bool
toArray set = array (minBound, maxBound) $ fmap (\x -> (x, x `elem` set)) [minBound .. maxBound]
 
instance Show CharSet where
   showsPrec d x@(CS _ _ _ _ _ m)
        | m < 0     = showParen (d > 10) $ showString "complement " . showsPrec 11 (complement x)
        | otherwise = showParen (d > 10) $ showString "fromDistinctAscList " . showsPrec 11 (toList x)


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

build :: (Char -> Bool) -> CharSet
build p = fromDistinctAscList $ filter p [minBound .. maxBound]

-- :digit:, etc.
posixAscii :: Map String CharSet
posixAscii = Map.fromList
    [ ("alnum", alnum')
    , ("alpha", alpha')
    , ("blank", fromList " \t")
    , ("cntrl", insert '\x7f' $ range '\x00' '\x1f')
    , ("digit", digit')
    , ("graph", range '\x21' '\x7e')
    , ("print", range '\x20' '\x7e')
    , ("word",  insert '_' alnum')
    , ("punct", fromList "-!\"#$%&'()*+,./:;<=>?@[\\]^_`{|}~")
    , ("space", fromList " \t\r\n\v\f")
    , ("upper", upper')
    , ("lower", lower')
    , ("xdigit", digit `union` range 'a' 'f' `union` range 'A' 'F')
    ]
    where
        lower' = range 'a' 'z'
        upper' = range 'A' 'Z'
        alpha' = lower' `union` upper'
        digit' = range '0' '9'
        alnum' = alpha' `union` digit'

data UnicodeCategory = UnicodeCategory String String CharSet String

-- \p{Letter} or \p{Mc}
unicodeCategories :: [UnicodeCategory]
unicodeCategories =
    [ UnicodeCategory "Letter" "L" l "any kind of letter from any language."
    ,     UnicodeCategory "Lowercase_Letter" "Ll" ll "a lowercase letter that has an uppercase variant"
    ,     UnicodeCategory "Uppercase_Letter" "Lu" lu "an uppercase letter that has a lowercase variant"
    ,     UnicodeCategory "Titlecase_Letter" "Lt" lt "a letter that appears at the start of a word when only the first letter of the word is capitalized"
    ,     UnicodeCategory "Letter&" "L&" la "a letter that exists in lowercase and uppercase variants (combination of Ll, Lu and Lt)"
    ,     UnicodeCategory "Modifier_Letter" "Lm" lm "a special character that is used like a letter"
    ,     UnicodeCategory "Other_Letter" "Lo" lo "a letter or ideograph that does not have lowercase and uppercase variants"
    , UnicodeCategory "Mark" "M" m "a character intended to be combined with another character (e.g. accents, umlauts, enclosing boxes, etc.)"
    ,     UnicodeCategory "Non_Spacing_Mark" "Mn" mn "a character intended to be combined with another character without taking up extra space (e.g. accents, umlauts, etc.)"
    ,     UnicodeCategory "Spacing_Combining_Mark" "Mc" mc "a character intended to be combined with another character that takes up extra space (vowel signs in many Eastern languages)"
    ,     UnicodeCategory "Enclosing_Mark" "Me" me "a character that encloses the character is is combined with (circle, square, keycap, etc.)"
    , UnicodeCategory "Separator" "Z" z "any kind of whitespace or invisible separator"
    ,     UnicodeCategory "Space_Separator" "Zs" zs "a whitespace character that is invisible, but does take up space"
    ,     UnicodeCategory "Line_Separator" "Zl" zl "line separator character U+2028"
    ,     UnicodeCategory "Paragraph_Separator" "Zp" zp "paragraph separator character U+2029"
    , UnicodeCategory "Symbol" "S" s "math symbols, currency signs, dingbats, box-drawing characters, etc."
    ,     UnicodeCategory "Math_Symbol" "Sm" sm "any mathematical symbol"
    ,     UnicodeCategory "Currency_Symbol" "Sc" sc "any currency sign"
    ,     UnicodeCategory "Modifier_Symbol" "Sk" sk "a combining character (mark) as a full character on its own"
    ,     UnicodeCategory "Other_Symbol" "So" so "various symbols that are not math symbols, currency signs, or combining characters"
    , UnicodeCategory "Number" "N" n "any kind of numeric character in any script"
    ,     UnicodeCategory "Decimal_Digit_Number" "Nd" nd "a digit zero through nine in any script except ideographic scripts"
    ,     UnicodeCategory "Letter_Number" "Nl" nl "a number that looks like a letter, such as a Roman numeral"
    ,     UnicodeCategory "Other_Number" "No" no "a superscript or subscript digit, or a number that is not a digit 0..9 (excluding numbers from ideographic scripts)"
    , UnicodeCategory "Punctuation" "P" p "any kind of punctuation character"
    ,     UnicodeCategory "Dash_Punctuation" "Pd" pd "any kind of hyphen or dash"
    ,     UnicodeCategory "Open_Punctuation" "Ps" ps "any kind of opening bracket"
    ,     UnicodeCategory "Close_Punctuation" "Pe" pe "any kind of closing bracket"
    ,     UnicodeCategory "Initial_Punctuation" "Pi" pi "any kind of opening quote"
    ,     UnicodeCategory "Final_Punctuation" "Pf" pf "any kind of closing quote"
    ,     UnicodeCategory "Connector_Punctuation" "Pc" pc "a punctuation character such as an underscore that connects words"
    ,     UnicodeCategory "Other_Punctuation" "Po" po "any kind of punctuation character that is not a dash, bracket, quote or connector"
    , UnicodeCategory "Other" "C" c "invisible control characters and unused code points"
    ,     UnicodeCategory "Control" "Cc" cc "an ASCII 0x00..0x1F or Latin-1 0x80..0x9F control character"
    ,     UnicodeCategory "Format" "Cf" cf "invisible formatting indicator"
    ,     UnicodeCategory "Private_Use" "Co" co "any code point reserved for private use"
    ,     UnicodeCategory "Surrogate" "Cs" cs "one half of a surrogate pair in UTF-16 encoding"
    ,     UnicodeCategory "Unassigned" "Cn" cn "any code point to which no character has been assigned.properties" ]
    where
        cat category = build ((category ==) . generalCategory)
        ll = cat LowercaseLetter
        lu = cat UppercaseLetter
        lt = cat TitlecaseLetter
        la = ll `union` lu `union` lt
        lm = cat ModifierLetter
        lo = cat OtherLetter
        l = la `union` lm `union` lo
        mn = cat NonSpacingMark
        mc = cat SpacingCombiningMark
        me = cat EnclosingMark
        m = mn `union` mc `union` me
        zs = cat Space
        zl = cat LineSeparator
        zp = cat ParagraphSeparator
        z = zs `union` zl `union` zp
        sm = cat MathSymbol
        sc = cat CurrencySymbol
        sk = cat ModifierSymbol
        so = cat OtherSymbol
        s = sm `union` sc `union` sk `union` so
        nd = cat DecimalNumber
        nl = cat LetterNumber
        no = cat OtherNumber
        n = nd `union` nl `union` no
        pd = cat DashPunctuation
        ps = cat OpenPunctuation
        pe = cat ClosePunctuation
        pi = cat InitialQuote
        pf = cat FinalQuote
        pc = cat ConnectorPunctuation
        po = cat OtherPunctuation
        p = pd `union` ps `union` pe `union` pi `union` pf `union` pc `union` po
        cc = cat Control
        cf = cat Format
        co = cat PrivateUse
        cs = cat Surrogate
        cn = cat NotAssigned
        c = cc `union` cf `union` co `union` cs `union` cn
        
-- Haskell character classes from Data.Char
control, space, lower, upper, alpha, alphaNum, print, digit, octDigit, letter, mark, number, punctuation, symbol, separator, ascii, latin1, asciiUpper, asciiLower :: CharSet
control = build isControl
space = build isSpace
lower = build isLower
upper = build isUpper
alpha = build isAlpha
alphaNum = build isAlphaNum
print = build isPrint
digit = build isDigit
octDigit = build isOctDigit
letter = build isLetter
mark = build isMark
number = build isNumber
punctuation = build isPunctuation
symbol = build isSymbol
separator = build isSeparator
ascii = build isAscii
latin1 = build isLatin1
asciiUpper = build isAsciiUpper
asciiLower = build isAsciiLower

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
