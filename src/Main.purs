module Main where

-- The zipper and the sorting process is written especially for the data from "du -x -h" [data comes in a certain order;
-- the total of a directory follows the subdirectories; we don’t navigate the tree except for adding new folders) and
-- first has to reverse the output of ```du```.  The zipper always conses new nodes to the beginning of the existing
-- folder list (whereas a zipper usually allows to focus on items in the middle of the list as well, and remembers the
-- “hole” there, too).

import Prelude (Unit, bind, map, (<<<), (>>=))

import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(UTF8))

import Effect (Effect)
import Effect.Console (log)

import Data.List (List, reverse, (:))
import Data.Traversable (sequence, foldl)
import Data.Either (Either(Left, Right), either)

import Folder (Folder, Node, nodeToFolder, flatten, sortSubFolders, showListNode)
import Zipper (Zipper(EmptyZipper), addFolderToZipper, getCurrentFolder, goTop)
import DiskUse (lineToNode, splitFileIntoLines)

-- The `reverse` is necessary because the algorithm needs to see the parent folder first but `du` outputs it last.
-- `dropWhile` makes sure to grab the first real node (i. e. not a line that just had `\n`, typically the last line,
-- which then is the first line after the `reverse`).  `sequence` will return Nothing if any actual line does not match
-- the expected pattern by turning `List (Maybe Folder)` into `Maybe (List Folder)`.  All folders start out with an
-- empty list of subfolders.

-- TIL: Remember, Purescript will not let me write `let (a : b) = …`.  Need to use `case x of (a : b) -> ` instead.

-- TIL: Could also have used `foldl (\acc f -> addFolderToZipper <$> acc <*> f)`

-- Handle `\n` (last line that becomes first line because of `reverse`)

dropFirstLineIfBad :: forall a b. List (Either a b) -> List (Either a b)
dropFirstLineIfBad (Left err : ns) = ns
dropFirstLineIfBad ns = ns

-- speed of version without `reverse` seems to be about the same:
-- mnodes = sequence $ dropWhile isNothing $ foldl (\acc line -> (lineToNode line) : acc) Nil $ splitFileIntoLines input

main :: Effect Unit
main = do
  input <- readTextFile UTF8 "du.txt"

  let result = validateInput input >>= linesToNodes >>= nodesToRootFolder >>= Right <<< showRootFolder

  either log log result
  
  where validateInput :: String -> Either String String
        validateInput i = case i of
                            ""        -> Left "Input file is empty!"
                            otherwise -> Right i
        linesToNodes :: String -> Either String (List Node)
        linesToNodes = sequence <<< dropFirstLineIfBad <<< reverse <<< map lineToNode <<< splitFileIntoLines
        nodesToRootFolder :: List Node -> Either String Folder
        nodesToRootFolder = getCurrentFolder <<< goTop <<< foldl addFolderToZipper EmptyZipper <<< map nodeToFolder
        showRootFolder :: Folder -> String
        showRootFolder = showListNode <<< flatten <<< sortSubFolders
