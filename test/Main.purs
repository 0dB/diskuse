module Test.Main where

import Prelude

import Effect (Effect)
-- import Effect.Console (log)

import Test.Assert (assert)
import Data.Either (Either(Left, Right))

import Node.FS.Sync (readTextFile, writeTextFile)
import Node.Encoding (Encoding(..))

import Main (go)

main :: Effect Unit
main = do
  input <- readTextFile UTF8 ("testinput.txt")
  reference <- readTextFile UTF8 ("testreference.txt")
  let output = case go input of
                 Right t -> t <> "\n"
                 Left e  -> "Error processing input."
  writeTextFile UTF8 "testoutput.txt" output
  assert (reference == output)
