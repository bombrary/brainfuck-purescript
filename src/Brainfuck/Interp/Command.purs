module Brainfuck.Interp.Command where

import Prelude

import Brainfuck.Command (Command(..))
import Brainfuck.Interp (Interp)
import Brainfuck.Interp.Util (incInstPtr, decInstPtr, readCommandOrFail, readDataOrFail, modifyDataOrFail, readCharOrFail)
import Brainfuck.State (modifyDataPtr)
import Control.Monad.State.Class (modify_)

import Brainfuck.Interp.Stream (write, read, Stream)
import Data.Char (toCharCode) as Char


interpCommand :: forall m. Monad m => Stream m -> Command -> Interp m Unit
interpCommand stream =
  case _ of
     IncPtr -> 
       incDataPtr

     DecPtr ->
       decDataPtr

     IncDat ->
       incData

     DecDat ->
       decData

     LBrace -> do
       x <- readDataOrFail
       when (x == 0)
         goToRBrace

     RBrace -> do
       x <- readDataOrFail
       when (x /= 0)
         goToLBrace

     Output -> do
       c <- readCharOrFail
       write c stream

     Input -> do
       x <- read stream
       modifyDataOrFail (\_ -> Char.toCharCode x)

     Nop ->
       pure unit



incDataPtr :: forall m. Monad m => Interp m Unit
incDataPtr = modify_ $ modifyDataPtr (_ + 1)


decDataPtr :: forall m. Monad m => Interp m Unit
decDataPtr = modify_ $ modifyDataPtr (_ - 1)


incData :: forall m. Monad m => Interp m Unit
incData = modifyDataOrFail (_ + 1)


decData :: forall m. Monad m => Interp m Unit
decData = modifyDataOrFail (_ - 1)


goToRBrace :: forall m. Monad m => Interp m Unit
goToRBrace = goToMate incInstPtr


goToLBrace :: forall m. Monad m => Interp m Unit
goToLBrace = goToMate decInstPtr


goToMate :: forall m. Monad m => Interp m Unit -> Interp m Unit
goToMate move = go 0
  where
    go :: Int -> Interp m Unit
    go cnt = do
      cmd <- readCommandOrFail
      let newCnt =
            case cmd of
              LBrace ->
                cnt + 1

              RBrace ->
                cnt - 1

              _ ->
                cnt
      if newCnt == 0
        then
          pure unit
        else do
          move
          go newCnt
