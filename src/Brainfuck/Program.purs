module Brainfuck.Program where

import Prelude

import Brainfuck.Command (Command, fromChar)
import Data.Array ((!!))
import Data.Array (intercalate) as Array
import Data.Maybe (Maybe)
import Data.String.CodeUnits (toCharArray) as CodeUnits


newtype Program = Program (Array Command)


instance Show Program where
  show (Program p) =
    "\"" <> (Array.intercalate " " $ map show p) <> "\""


fromString :: String -> Program
fromString str =
  Program $ map fromChar $ CodeUnits.toCharArray str


readAt :: Int -> Program -> Maybe Command
readAt i (Program xs) = xs !! i
