module Brainfuck.Interp.Command where

import Prelude

import Brainfuck.Command (Command(..))
import Brainfuck.Interp (Interp)

import Brainfuck.Interp.Util (incInstPtr, decInstPtr, readCommandOrFail, readDataOrFail, modifyDataOrFail)
import Brainfuck.State (modifyDataPtr)
import Control.Monad.State.Class (modify_)


interpCommand :: Command -> Interp Unit
interpCommand =
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

     Output ->
       pure unit

     Input ->
       pure unit

     Nop ->
       pure unit



incDataPtr :: Interp Unit
incDataPtr = modify_ $ modifyDataPtr (_ + 1)


decDataPtr :: Interp Unit
decDataPtr = modify_ $ modifyDataPtr (_ - 1)


incData :: Interp Unit
incData = modifyDataOrFail (_ + 1)


decData :: Interp Unit
decData = modifyDataOrFail (_ - 1)


goToRBrace :: Interp Unit
goToRBrace = goToMate incInstPtr


goToLBrace :: Interp Unit
goToLBrace = goToMate decInstPtr


goToMate :: Interp Unit -> Interp Unit
goToMate move = go 0
  where
    go :: Int -> Interp Unit
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
