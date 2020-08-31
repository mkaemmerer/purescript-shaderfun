module Shader.Expr.Traversal (exl, exr, ex) where

import Shader.Expr (BinaryExpr(..), Expr, UnaryExpr(..))
import Unsafe.Coerce (unsafeCoerce)

eraseType :: forall a b. Expr a -> Expr b
eraseType = unsafeCoerce

exl :: forall a b. BinaryExpr a -> Expr b
exl (BinEq l r)        = eraseType l
exl (BinNeq l r)       = eraseType l
exl (BinAnd l r)       = eraseType l
exl (BinOr l r)        = eraseType l
exl (BinLt l r)        = eraseType l
exl (BinLte l r)       = eraseType l
exl (BinGt l r)        = eraseType l
exl (BinGte l r)       = eraseType l
exl (BinPlus l r)      = eraseType l
exl (BinMinus l r)     = eraseType l
exl (BinTimes l r)     = eraseType l
exl (BinDiv l r)       = eraseType l
exl (BinPlusV2 l r)    = eraseType l
exl (BinMinusV2 l r)   = eraseType l
exl (BinScaleV2 l r)   = eraseType l
exl (BinPlusV3 l r)    = eraseType l
exl (BinMinusV3 l r)   = eraseType l
exl (BinScaleV3 l r)   = eraseType l
exl (BinPlusC l r)     = eraseType l
exl (BinMinusC l r)    = eraseType l
exl (BinScaleC l r)    = eraseType l
exl (BinPlusCol l r)   = eraseType l
exl (BinMinusCol l r)  = eraseType l
exl (BinTimesCol l r)  = eraseType l
exl (BinScaleCol l r)  = eraseType l
exl (BinDivC l r)      = eraseType l
exl (BinTimesC l r)    = eraseType l

exr :: forall a b. BinaryExpr a -> Expr b
exr (BinEq l r)        = eraseType r
exr (BinNeq l r)       = eraseType r
exr (BinAnd l r)       = eraseType r
exr (BinOr l r)        = eraseType r
exr (BinLt l r)        = eraseType r
exr (BinLte l r)       = eraseType r
exr (BinGt l r)        = eraseType r
exr (BinGte l r)       = eraseType r
exr (BinPlus l r)      = eraseType r
exr (BinMinus l r)     = eraseType r
exr (BinTimes l r)     = eraseType r
exr (BinDiv l r)       = eraseType r
exr (BinPlusV2 l r)    = eraseType r
exr (BinMinusV2 l r)   = eraseType r
exr (BinScaleV2 l r)   = eraseType r
exr (BinPlusV3 l r)    = eraseType r
exr (BinMinusV3 l r)   = eraseType r
exr (BinScaleV3 l r)   = eraseType r
exr (BinPlusC l r)     = eraseType r
exr (BinMinusC l r)    = eraseType r
exr (BinScaleC l r)    = eraseType r
exr (BinPlusCol l r)   = eraseType r
exr (BinMinusCol l r)  = eraseType r
exr (BinTimesCol l r)  = eraseType r
exr (BinScaleCol l r)  = eraseType r
exr (BinDivC l r)      = eraseType r
exr (BinTimesC l r)    = eraseType r

ex :: forall a b. UnaryExpr a -> Expr b
ex (UnNegate e)        = eraseType e
ex (UnNot e)           = eraseType e
ex (UnProjV2X e)       = eraseType e
ex (UnProjV2Y e)       = eraseType e
ex (UnProjV3X e)       = eraseType e
ex (UnProjV3Y e)       = eraseType e
ex (UnProjV3Z e)       = eraseType e
ex (UnProjReal e)      = eraseType e
ex (UnProjImaginary e) = eraseType e
ex (UnProjR e)         = eraseType e
ex (UnProjG e)         = eraseType e
ex (UnProjB e)         = eraseType e
