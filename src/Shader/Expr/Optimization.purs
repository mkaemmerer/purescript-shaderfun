module Shader.Expr.Optimization (constantFold) where

import Prelude

import Data.Color (Color(..))
import Data.Complex (Complex(..))
import Data.Foldable (all)
import Data.Vec2 (Vec2(..))
import Data.Vec3 (Vec3(..))
import Data.VectorSpace ((*^), (^+^), (^-^))
import Math (cos, sin)
import Partial.Unsafe (unsafePartial)
import Shader.Expr (BinaryOp(..), Expr(..), UnaryOp(..), fromBoolean, fromColor, fromComplex, fromNumber, fromVec2, fromVec3)
import Unsafe.Coerce (unsafeCoerce)

constantFold :: forall a. Expr a -> Expr a
constantFold (EVar n)         = EVar n
constantFold (EBool b)        = EBool b
constantFold (ENum n)         = ENum n
constantFold (EVec2 x y)      = EVec2 (constantFold x) (constantFold y)
constantFold (EVec3 x y z)    = EVec3 (constantFold x) (constantFold y) (constantFold z)
constantFold (EComplex r i)   = EComplex (constantFold r) (constantFold i)
constantFold (EColor r g b)   = EColor (constantFold r) (constantFold g) (constantFold b)
constantFold (EUnary op e)    = constantFold' $ EUnary op (constantFold e)
constantFold (EBinary op l r) = constantFold' $ EBinary op (constantFold l) (constantFold r)
constantFold (EUnit)          = EUnit
constantFold (ETuple l r)     = ETuple (constantFold l) (constantFold r)
constantFold (EFst e)         = constantFold' $ EFst (constantFold e)
constantFold (ESnd e)         = constantFold' $ ESnd (constantFold e)
constantFold (ECall fn args)  = constantFold' $ ECall fn (constantFold <$> args)
constantFold (EIf i t e)      = constantFold' $ EIf (constantFold i) (constantFold t) (constantFold e)
constantFold (EBind name ty val body) = EBind name ty (constantFold val) (constantFold body)

constantFold' :: forall a. Expr a -> Expr a
constantFold' (EBinary op l r)
  | isConst l && isConst r            = unsafePartial $ applyBinary op l r
constantFold' (EUnary op e)
  | isConst e                         = unsafePartial $ applyUnary op e
constantFold' (EFst (ETuple l r))     = l
constantFold' (ESnd (ETuple l r))     = r
constantFold' (EIf (EBool true) t e)  = t
constantFold' (EIf (EBool false) t e) = e
constantFold' (ECall fn args)
  | all isConst args                  = unsafePartial $ applyCall fn args
-- No special cases apply
constantFold' e = e

applyBinary :: Partial => forall a. BinaryOp -> Expr a -> Expr a -> Expr a
applyBinary OpPlus     e1 e2 = eraseNumber  $ fromNumber   $ toNumber  e1 +   toNumber  e2
applyBinary OpMinus    e1 e2 = eraseNumber  $ fromNumber   $ toNumber  e1 -   toNumber  e2
applyBinary OpTimes    e1 e2 = eraseNumber  $ fromNumber   $ toNumber  e1 *   toNumber  e2
applyBinary OpDiv      e1 e2 = eraseNumber  $ fromNumber   $ toNumber  e1 /   toNumber  e2
applyBinary OpEq       e1 e2 = eraseBool    $ fromBoolean  $ toNumber  e1 ==  toNumber  e2
applyBinary OpNeq      e1 e2 = eraseBool    $ fromBoolean  $ toNumber  e1 /=  toNumber  e2
applyBinary OpLt       e1 e2 = eraseBool    $ fromBoolean  $ toNumber  e1 <   toNumber  e2
applyBinary OpLte      e1 e2 = eraseBool    $ fromBoolean  $ toNumber  e1 <=  toNumber  e2
applyBinary OpGt       e1 e2 = eraseBool    $ fromBoolean  $ toNumber  e1 >   toNumber  e2
applyBinary OpGte      e1 e2 = eraseBool    $ fromBoolean  $ toNumber  e1 >=  toNumber  e2
applyBinary OpAnd      e1 e2 = eraseBool    $ fromBoolean  $ toBool    e1 &&  toBool    e2
applyBinary OpOr       e1 e2 = eraseBool    $ fromBoolean  $ toBool    e1 ||  toBool    e2
applyBinary OpPlusV2   e1 e2 = eraseVec2    $ fromVec2     $ toVec2    e1 ^+^ toVec2    e2
applyBinary OpMinusV2  e1 e2 = eraseVec2    $ fromVec2     $ toVec2    e1 ^-^ toVec2    e2
applyBinary OpScaleV2  e1 e2 = eraseVec2    $ fromVec2     $ toNumber  e1 *^  toVec2    e2
applyBinary OpPlusV3   e1 e2 = eraseVec3    $ fromVec3     $ toVec3    e1 ^+^ toVec3    e2
applyBinary OpMinusV3  e1 e2 = eraseVec3    $ fromVec3     $ toVec3    e1 ^-^ toVec3    e2
applyBinary OpScaleV3  e1 e2 = eraseVec3    $ fromVec3     $ toNumber  e1 *^  toVec3    e2
applyBinary OpPlusC    e1 e2 = eraseComplex $ fromComplex  $ toComplex e1 +   toComplex e2
applyBinary OpMinusC   e1 e2 = eraseComplex $ fromComplex  $ toComplex e1 -   toComplex e2
applyBinary OpTimesC   e1 e2 = eraseComplex $ fromComplex  $ toComplex e1 *   toComplex e2
applyBinary OpDivC     e1 e2 = eraseComplex $ fromComplex  $ toComplex e1 /   toComplex e2
applyBinary OpScaleC   e1 e2 = eraseComplex $ fromComplex  $ toNumber  e1 *^  toComplex e2
applyBinary OpPlusCol  e1 e2 = eraseColor   $ fromColor    $ toColor   e1 +   toColor   e2
applyBinary OpMinusCol e1 e2 = eraseColor   $ fromColor    $ toColor   e1 -   toColor   e2
applyBinary OpTimesCol e1 e2 = eraseColor   $ fromColor    $ toColor   e1 *   toColor   e2
applyBinary OpScaleCol e1 e2 = eraseColor   $ fromColor    $ toNumber  e1 *^  toColor   e2

applyUnary :: Partial => forall a. UnaryOp -> Expr a -> Expr a
applyUnary OpNegate         e = eraseNumber $ fromNumber  $ negate (toNumber e)
applyUnary OpNot            e = eraseBool   $ fromBoolean $ not    (toBool e)
applyUnary OpProjV2X        e = eraseNumber $ fromNumber  $ (\(Vec2 v) -> v.x)     (toVec2 e)
applyUnary OpProjV2Y        e = eraseNumber $ fromNumber  $ (\(Vec2 v) -> v.y)     (toVec2 e)
applyUnary OpProjV3X        e = eraseNumber $ fromNumber  $ (\(Vec3 v) -> v.x)     (toVec3 e)
applyUnary OpProjV3Y        e = eraseNumber $ fromNumber  $ (\(Vec3 v) -> v.y)     (toVec3 e)
applyUnary OpProjV3Z        e = eraseNumber $ fromNumber  $ (\(Vec3 v) -> v.z)     (toVec3 e)
applyUnary OpProjReal       e = eraseNumber $ fromNumber  $ (\(Complex r i) -> r)  (toComplex e)
applyUnary OpProjImaginary  e = eraseNumber $ fromNumber  $ (\(Complex r i) -> i)  (toComplex e)
applyUnary OpProjR          e = eraseNumber $ fromNumber  $ (\(Color r g b) -> r)  (toColor e)
applyUnary OpProjG          e = eraseNumber $ fromNumber  $ (\(Color r g b) -> g)  (toColor e)
applyUnary OpProjB          e = eraseNumber $ fromNumber  $ (\(Color r g b) -> b)  (toColor e)

applyCall :: Partial => forall a. String -> Array (Expr a) -> Expr a
applyCall "cos" [e] = eraseNumber $ fromNumber $ cos (toNumber e)
applyCall "sin" [e] = eraseNumber $ fromNumber $ sin (toNumber e)
-- Can't apply optimizations
applyCall fn args   = ECall fn (eraseType <$> args)

isConst :: forall a. Expr a -> Boolean
isConst (EVar n)                 = false
isConst (EBool b)                = true
isConst (ENum n)                 = true
isConst (EVec2 x y)              = isConst x && isConst y
isConst (EVec3 x y z)            = isConst x && isConst y && isConst z
isConst (EComplex r i)           = isConst r && isConst i
isConst (EColor r g b)           = isConst r && isConst g && isConst b
isConst (EUnary op e)            = false
isConst (EBinary op l r)         = false
isConst (EUnit)                  = true
isConst (ETuple l r)             = isConst l && isConst r
isConst (EFst e)                 = false
isConst (ESnd e)                 = false
isConst (ECall fn args)          = false
isConst (EIf i t e)              = false
isConst (EBind name ty val body) = false


eraseType :: forall a b. Expr a -> Expr b
eraseType = unsafeCoerce

eraseNumber :: forall a. Expr Number -> Expr a
eraseNumber = eraseType

eraseBool :: forall a. Expr Boolean -> Expr a
eraseBool = eraseType

eraseVec2 :: forall a. Expr Vec2 -> Expr a
eraseVec2 = eraseType

eraseVec3 :: forall a. Expr Vec3 -> Expr a
eraseVec3 = eraseType

eraseComplex :: forall a. Expr Complex -> Expr a
eraseComplex = eraseType

eraseColor :: forall a. Expr Color -> Expr a
eraseColor = eraseType

-- TODO: might be classier to use maybe instead of partial
toBool :: Partial => forall a. Expr a -> Boolean
toBool (EBool b) = b

toNumber :: Partial => forall a. Expr a -> Number
toNumber (ENum n) = n

toComplex :: Partial => forall a. Expr a -> Complex
toComplex (EComplex (ENum r) (ENum i)) = Complex r i

toColor :: Partial => forall a. Expr a -> Color
toColor (EColor (ENum r) (ENum g) (ENum b)) = Color r g b

toVec2 :: Partial => forall a. Expr a -> Vec2
toVec2 (EVec2 (ENum x) (ENum y)) = Vec2 { x, y }

toVec3 :: Partial => forall a. Expr a -> Vec3
toVec3 (EVec3 (ENum x) (ENum y) (ENum z)) = Vec3 { x, y, z }