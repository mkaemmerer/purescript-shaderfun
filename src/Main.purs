module Main where

import Prelude

import Data.Foldable (sequence_)
import Data.Maybe (Maybe)
import Data.Color (Color(..), (**))
import Effect (Effect)
import Render (getContext, compileShader, drawShader)
import Shader.Expr (color, fract, num, p, projX, fromColor)
import Shader.GLSL (toGLSL)
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


source :: String
source = "return " <> (toGLSL col) <> ";"
  where
    col = (color g g g) ** tint
    tint = fromColor $ Color 0.5 0.0 0.5
    g = fract $ ((projX p) / (num 2880.0)) + (num 0.5)


render :: HTMLCanvasElement -> Effect Unit
render canvas = do
  let gl = getContext canvas
  let shader = compileShader gl source
  drawShader gl shader

main :: Effect Unit
main = do
  c <- getCanvas
  sequence_ $ render <$> c
