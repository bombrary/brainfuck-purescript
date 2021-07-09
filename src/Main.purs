module Main where

import Prelude

import Effect (Effect)
import Halogen.Aff (awaitBody, runHalogenAff) as HA
import Halogen.VDom.Driver (runUI)
import Component (component)

import Effect.Aff.AVar (empty) as AVar

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  avar <- AVar.empty
  runUI component avar body
