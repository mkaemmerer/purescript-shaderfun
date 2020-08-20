module Main where

import Prelude

import Data.Color (Color(..), (**))
import Data.Foldable (sequence_)
import Data.Maybe (Maybe)
import Data.VectorSpace ((^-^))
import Effect (Effect)
import Render (getContext, compileShader, drawShader)
import Shader.Expr (Expr(..), bindE, color, fract, fromColor, ifE, lt, num, p, projX, projY)
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


white :: Expr Color
white = fromColor $ Color 1.0 1.0 1.0

black :: Expr Color
black = fromColor $ Color 0.0 0.0 0.0

value :: Expr Number
value = EVar "value"

gray :: Expr Color
gray = EVar "gray"

source :: String
source = toGLSL expr
  where
    tint = fromColor $ Color 0.5 0.0 0.5
    {x, y} = {x: projX p, y: projY p}
    expr = bindE "value" v $ bindE "gray" (g value) (col gray)
    v = fract $ (x / (num 2880.0)) + (num 0.5)
    g = \val -> color val val val
    col = \c -> ifE (lt y (num 0.0)) (white ^-^ c ** tint) (c ** tint)

render :: HTMLCanvasElement -> Effect Unit
render canvas = do
  let gl = getContext canvas
  let shader = compileShader gl source
  drawShader gl shader

main :: Effect Unit
main = do
  c <- getCanvas
  sequence_ $ render <$> c
