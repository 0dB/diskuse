module Zipper ( Zipper(EmptyZipper)
              , addFolderToZipper
              , getCurrentFolder
              , goTop ) where

import Prelude ((+), (-), (<=), otherwise)
import Data.List (List(Nil), length, (:))
import Data.Either (Either(..))

import Folder (Folder(Folder), nodeDepth)

-- (Bread-) crumb is a list of Folders each of which is the respective parent folder with a list of
-- subfolder(s) with a "hole" for the current folder (of which the crumb is the parent).  In my case the
-- hole is just that the *first* item is missing, so there is no *visible* hole in this data structure.
-- Otherwise I would have to keep a “left” and “right” part to indicate where the hole is.

-- TODO: Module Zipper does not need to know that Crumb is (for now) a hard-wired case. Generalize this.
-- Should Crumb be defined in module Folder?  Or should Folder be defined in module Zipper, but Node not?

newtype Crumb = Crumb Folder
data Zipper = EmptyZipper | Zipper Folder (List Crumb)

-- Add new folder to parent *and* change focus to that new folder (used inside ```addFolderToZipper```).
-- The “hole” (in this implementation) is just the fact that the new folder isn’t in the parent (yet); it is
-- at the beginning of the folders list of the parent.

addFolder :: Folder -> Zipper -> Zipper
addFolder f EmptyZipper = Zipper f Nil -- starting Zipper
addFolder f (Zipper parent crumbs) = Zipper f (Crumb parent : crumbs)

-- TIL: (:) has low precedence; `Folder n fs:cs` = (Folder n fs):cs`

-- New folders are always consed to front (we only go up after having added a new folder)

goUpOne :: Zipper -> Zipper
goUpOne EmptyZipper = EmptyZipper -- return Left err instead?  Or is this the correct handling for the starting Zipper?
goUpOne z@(Zipper f Nil) = EmptyZipper -- this case should not happen; return Left err?
goUpOne (Zipper f (Crumb (Folder n fs) : crumbs)) = Zipper (Folder n (f : fs)) crumbs

goUpN :: Int -> Zipper -> Zipper
goUpN n z | n <= 0    = z -- or return error if negative?
          | otherwise = goUpN (n - 1) (goUpOne z)

goTop :: Zipper -> Zipper
goTop z@(Zipper f Nil) = z -- reached the top
goTop z                = goTop (goUpOne z)

-- TODO: Check if everything is handled correctly for EmptyZipper

addFolderToZipper :: Zipper -> Folder -> Zipper
addFolderToZipper z folder = addFolder folder (goUpN n z)
    where n = zipperDepth z - nodeDepth folder + 2
          zipperDepth EmptyZipper = 0 -- is this the right value?  Or -1?
          zipperDepth (Zipper _ crumbs) = length crumbs

getCurrentFolder :: Zipper -> Either String Folder
getCurrentFolder EmptyZipper = Left "Zipper is empty!"
getCurrentFolder (Zipper f _) = Right f
