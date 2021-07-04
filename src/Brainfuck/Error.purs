module Brainfuck.Error where

import Prelude


data Error
  = IPtrOutOfRange
  | DPtrOutOfRange
  | CharDecodeFailed
  | CharInputFailed


instance Show Error where
  show err =
    case err of
      IPtrOutOfRange -> "Error: Instruction pointer out of range"
      DPtrOutOfRange -> "Error: Data oointer out of range"
      CharDecodeFailed -> "Error: Failed to decode integer to char"
      CharInputFailed -> "Error: Failed to input char"
