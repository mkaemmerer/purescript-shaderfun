module Main where

import Prelude

import Data.Foldable (sequence_)
import Data.Maybe (Maybe)
import Effect (Effect)
import Example.Circle (source)
import Render (getContext, compileShader, drawShader)
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.HTML (HTMLCanvasElement, window)
import Web.HTML.HTMLCanvasElement (fromElement)
import Web.HTML.HTMLDocument (toParentNode)
import Web.HTML.Window (document)

getCanvas :: Effect (Maybe HTMLCanvasElement)
getCanvas = do
  w <- window
  d <- document w
  el <- querySelector (QuerySelector "canvas") (toParentNode d)
  let canvEl = el >>= fromElement
  pure canvEl

render :: HTMLCanvasElement -> Effect Unit
render canvas = do
  let gl = getContext canvas
  let shader = compileShader gl source
  drawShader gl shader

main :: Effect Unit
main = do
  c <- getCanvas
  sequence_ $ render <$> c
