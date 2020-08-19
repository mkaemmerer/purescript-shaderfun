module Render (WebGLContext, WebGLProgram, getContext, compileShader, drawShader) where

import Prelude
import Effect (Effect)
import Data.Function.Uncurried (Fn1, Fn2, runFn1, runFn2)
import Web.HTML (HTMLCanvasElement)

foreign import data WebGLContext :: Type
foreign import data WebGLProgram :: Type

foreign import getContextImpl :: Fn1 (HTMLCanvasElement) (WebGLContext)
foreign import compileShaderImpl :: Fn2 (WebGLContext) (String) (WebGLProgram)
foreign import drawShaderImpl :: Fn2 (WebGLContext) (WebGLProgram) (Effect Unit)

-- TODO: this should probably return an effect
getContext :: HTMLCanvasElement -> WebGLContext
getContext canvas = runFn1 getContextImpl canvas

-- TODO: this should probably return an effect
compileShader :: WebGLContext -> String -> WebGLProgram
compileShader gl source = runFn2 compileShaderImpl gl source

drawShader :: WebGLContext -> WebGLProgram -> Effect Unit
drawShader gl shader = runFn2 drawShaderImpl gl shader
