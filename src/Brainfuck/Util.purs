module Brainfuck.Interp.Util where

import Prelude

import Brainfuck.Command (Command)
import Brainfuck.Env (getProgram)
import Brainfuck.Error (Error(..))
import Brainfuck.Interp (Interp)
import Brainfuck.State (modifyData, modifyInstPtr, readCommand, readData)
import Control.Monad.Except.Trans (throwError)
import Control.Monad.Reader.Trans (ask)
import Control.Monad.State.Trans (get, gets, modify_, put)
import Data.Char (fromCharCode) as Char
import Data.Maybe (Maybe(..))


modifyDataOrFail ::  (Int -> Int) -> Interp Unit
modifyDataOrFail f = do
  state <- get
  case modifyData f state of
    Just newState ->
      put newState

    Nothing ->
      throwError DPtrOutOfRange


readDataOrFail ::  Interp Int
readDataOrFail = do
  gets readData >>=
    case _ of
      Just x ->
        pure x

      Nothing ->
        throwError DPtrOutOfRange


readCharOrFail :: Interp Char
readCharOrFail = do
  x <- readDataOrFail
  case Char.fromCharCode x of
    Just c ->
      pure c

    Nothing ->
      throwError CharDecodeFailed


readCommandOrFail :: Interp Command
readCommandOrFail = do
  state <- get
  program <- getProgram <$> ask
  case readCommand program state of
    Just cmd ->
      pure cmd

    Nothing ->
      throwError IPtrOutOfRange


incInstPtr ::  Interp Unit
incInstPtr = modify_ $ modifyInstPtr (_ + 1)


decInstPtr ::  Interp Unit
decInstPtr = modify_ $ modifyInstPtr (_ - 1)
