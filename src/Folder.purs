module Folder ( Node(Node)
              , Path
              , Size(Size)
              , Prefix
              , Suffix(Suffix)
              , showListNode
              , sortSubFolders
              , nodeDepth
              , flatten
              , nodeToFolder
              , Unit(B, K, M, G)
              , Folder(..) ) where

import Prelude (class Eq, class Show, Ordering, compare, map, show, (&&), (-), (>>>), (<>), (==), (<=))
import Data.Ordering (Ordering(..))
import Data.Ord (class Ord)
import Data.List (List(Nil), length, sortBy, (:))
import Data.Foldable (foldr, foldMap, intercalate)
import Data.Maybe (Maybe(..))

type Prefix = Int -- the 8 in 8.1M
newtype Suffix = Suffix (Maybe Int) -- the 1 in 8.1M
data Unit = B | K | M | G -- is this not "ordered"?
data Size = Size Prefix Suffix Unit -- `8.1M` becomes `Size 8 (Suffix (Just 1)) M`, `8M` becomes `8 Nothing M`

-- (Bread-) crumb is the (list of) parent folder(s) with a list of folders each but always only with the *first* item
-- missing (the “hole”), so there is no *visible* hole in this data structure.

type Path = List String
data Node = Node Path Size
data Folder = Folder Node (List Folder)

-- WHY is this needed?  Why is this not implied?  (I think in Haskell it is.)

instance eqUnit :: Eq Unit where
    eq B B = true
    eq K K = true
    eq M M = true
    eq G G = true
    eq _ _ = false

-- WHY is this needed?  Why is this not implied?  Why do I have to specify both directions?

instance ordUnit :: Ord Unit where
    compare G G = EQ
    compare G _ = GT
    compare _ G = LT
    compare M M = EQ
    compare M _ = GT
    compare _ M = LT
    compare K K = EQ
    compare K _ = GT
    compare _ K = LT
    compare B B = EQ

instance ordSuffix :: Ord Suffix where
    compare (Suffix Nothing) (Suffix Nothing)     = EQ
    compare (Suffix (Just _)) (Suffix Nothing)    = GT
    compare (Suffix Nothing) (Suffix (Just _))    = LT
    compare (Suffix (Just s1)) (Suffix (Just s2)) = compare s1 s2

instance eqSuffix :: Eq Suffix where
    eq (Suffix Nothing) (Suffix Nothing)     = true
    eq (Suffix Nothing) (Suffix (Just 0))    = true
    eq (Suffix (Just 0)) (Suffix Nothing)    = true
    eq (Suffix (Just _)) (Suffix Nothing)    = false
    eq (Suffix Nothing) (Suffix (Just _))    = false
    eq (Suffix (Just s1)) (Suffix (Just s2)) = s1 == s2

instance showSuffix :: Show Suffix where
    show (Suffix (Just s)) = "." <> show s
    show (Suffix Nothing)  = ""

instance eqSize :: Eq Size where
    eq (Size p1 s1 u1) (Size p2 s2 u2) = (p1 == p2) && (s1 == s2) && (u1 == u2)

instance ordSize :: Ord Size where
    compare (Size p1 s1 u1) (Size p2 s2 u2) = case compare u1 u2 of
                                                GT -> GT
                                                LT -> LT
                                                EQ -> case compare p1 p2 of
                                                        GT -> GT
                                                        LT -> LT
                                                        EQ -> compare s1 s2

instance showSize :: Show Size where
    show (Size p s unit) = show p <> show s <> case unit of
                                                       B -> "B"
                                                       K -> "K"
                                                       M -> "M"
                                                       G -> "G"

compareFolderSize :: Folder -> Folder -> Ordering
compareFolderSize (Folder (Node _ size1) _) (Folder (Node _ size2) _) = compare size2 size1 -- high to low

sortSubFolders :: Folder -> Folder
sortSubFolders (Folder n fs) = Folder n (sortFolders fs)

sortFolders :: List Folder -> List Folder
sortFolders Nil = Nil
sortFolders folders = (map sortSubFolders >>> sortBy compareFolderSize) folders

indent :: Int -> String
indent n | n <= 1 = ""
         | true   = indent (n - 1) <> ".\t"

--   instead of `intercalate` could also use `foldMap (\y -> y <> "/") ss`

instance showNode :: Show Node where
  show (Node ss size) = indent (length ss) <> show size <> "\t" <> indent (8 - (length ss)) <> intercalate "/" ss <> "\n"

-- the following yields “orphan instance” error; see https://github.com/purescript/purescript/issues/1247

-- I can use `show xs` where xs is a list of node, but then the default is used, which does not format the way I want
-- (https://github.com/purescript/purescript-lists/blob/v3.4.0/src/Data/List/Types.purs#L117-L117)

-- instance showList :: Show Node => Show (List Node) where
--     show Nil = ""
--     show ns  = foldMap show ns

showListNode :: (List Node) -> String
showListNode ns = foldMap show ns

flatten :: Folder -> List Node
flatten t = squish t Nil
  where squish (Folder n fs) ns = n : foldr squish ns fs

nodeDepth :: Folder -> Int
nodeDepth (Folder (Node path _) _) = length path

nodeToFolder :: Node -> Folder
nodeToFolder n = Folder n Nil
