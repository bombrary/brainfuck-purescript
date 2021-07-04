module Brainfuck.Command where

import Prelude

data Command
  = IncPtr -- "+"
  | DecPtr -- "-"
  | IncDat -- ">"
  | DecDat -- "<"
  | LBrace -- "["
  | RBrace -- "]"
  | Output -- "."
  | Input -- ","
  | Nop -- otherwise


instance Show Command where
  show =
    case _ of
      IncPtr -> "+"
      DecPtr -> "-"
      IncDat -> ">"
      DecDat -> "<"
      LBrace -> "["
      RBrace -> "]"
      Output -> "."
      Input -> ","
      Nop -> "nop"


fromChar :: Char -> Command
fromChar =
  case _ of
    '>' -> IncPtr
    '<' -> DecPtr
    '+' -> IncDat
    '-' -> DecDat
    '[' -> LBrace
    ']' -> RBrace
    '.' -> Output
    ',' -> Input
    _ -> Nop
