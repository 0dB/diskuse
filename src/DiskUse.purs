module DiskUse
  ( lineToNode
  , splitFileIntoLines ) where

-- The zipper and the sorting process is written especially for the data from "du -x -h" [data comes in a
-- certain order; the total of a directory follows the subdirectories; we don’t navigate the tree except for
-- adding new folders) and first has to reverse the output of ```du```.  The zipper always conses new nodes
-- to the beginning of the existing folder list (whereas a zipper usually allows to focus on items in the
-- middle of the list as well, and remembers the “hole” there, too).

import Prelude (pure, (>>>), (<$>), (<*>), (<>), ($), (>>=))
import Data.List (List, fromFoldable)
import Data.Maybe (Maybe(..))
import Data.String (split, Pattern(Pattern))
import Data.String.Regex as R
import Data.String.Regex.Flags as RF
import Data.Either (Either(..))
import Data.Int (fromString)
import Data.Array.NonEmpty (toArray)

import Folder (Node(Node), Size(Size), Unit(G, M, K, B), Suffix(Suffix))

splitFileIntoLines :: String -> List String
splitFileIntoLines = split (Pattern "\n") >>> fromFoldable

-- TIL: `R.match` will return empty string ("") for capture group that is empty — not Nothing!  But
-- `fromString ""` will yield Nothing.

normalizeSize :: String -> String -> String -> Maybe Size
normalizeSize p s u = let unit = case u of
                                   "B" -> Just B
                                   "K" -> Just K
                                   "M" -> Just M
                                   "G" -> Just G
                                   ""  -> Just B -- sometimes `du` shows size as `0` (and not `0B`)
                                   otherwise -> Nothing
                      in Size <$> fromString p <*> pure (Suffix (fromString s)) <*> unit

-- the last `\n` in the file causes problems, this is taken care of by creating `Maybe Node` and later
-- checking the list of nodes

-- First `Left err` is for when regex is badly formatted.  I could not provoke such an error, though.

-- `p s u` = prefix, suffix, unit

-- `<$>` is just the infix version of `map`.  If I remember correctly, for Purescript, there is no `fmap`,
-- it is not needed (has to with an improved class hierarchy as opposed to Haskell)

-- TIL: the variables inside { size, path } are visible outside of {} too.  Don’t forget that “record
-- punning” is being used. Example:
-- do { size, path } <- foo
--    pure $ bar size path

-- TIL: first `Left` is the “error” that is returned

-- Or is this more readable (from DiskUse.purs210):
-- do pattern           <- R.regex re RF.noFlags
--    { p, s, u, path } <- parseLine pattern -- how come `path` is visible from here on?
--    path'             <- validatePath path
--    size              <- makeSize p s u
--    pure $ Node (fromFoldable (split (Pattern "/") path')) size

type ParsedLine = { p :: String, s :: String, u :: String, path :: String, size :: Size }

type ESP = Either String ParsedLine

dummySize :: Size
dummySize = Size 0 (Suffix Nothing) B

lineToNode :: String -> Either String Node
lineToNode line = validateRe re >>= parseLine >>= validatePath >>= makeSize >>= 
                  \pl -> pure $ Node (fromFoldable (split (Pattern "/") pl.path)) pl.size

                  where re :: String
                        re = "^ *([0-9]+)[,.]?([0-9]?)([BKMG]?)\\t(.*)"

                        validateRe :: String -> Either String R.Regex
                        validateRe r = R.regex r RF.noFlags

                        -- 2020-12 Updated to PS 0.13. `match` now returns NonEmptyArray. Cannot pattern match on that,
                        -- since it is a type and not a constructor. Could code be more elegant than this? Could not
                        -- find examples of how other people solve this.

                        parseLine :: R.Regex -> ESP
                        parseLine pattern = case R.match pattern line of
                                              Just x | [_, Just p, Just s, Just u, Just path] <- toArray x -> Right { p, s, u, path, size : dummySize }
                                              otherwise                                                    -> Left ("Line \"" <> line <> "\" did not match regex!")

                        validatePath :: ParsedLine -> ESP
                        validatePath parsed@{ path } = case path of
                                                         ""        -> Left ("Empty path in line \"" <> line <> "\"!")
                                                         otherwise -> Right parsed

                        makeSize :: ParsedLine -> ESP
                        makeSize parsed@{ p, s, u } = case normalizeSize p s u of
                                                        Just size -> Right $ parsed { size = size }
                                                        otherwise -> Left ("Could not read size from line \"" <> line <> "\"!")
