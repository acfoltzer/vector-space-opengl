{-# LANGUAGE TemplateHaskell #-}

import Data.VectorSpace
import Graphics.Rendering.OpenGL.GL

import Control.Applicative
import Data.AEq
import Data.Int
import Foreign.C.Types

import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Test.QuickCheck.All

addAssoc a b c = (a ^+^ b) ^+^ c ~== a ^+^ (b ^+^ c)
addId a = (a ^+^ zeroV) ~== a
addNeg a = x ~== y && y ~== zeroV
  where x = a ^+^ negateV a
        y = negateV a ^+^ a
addComm a b = (a ^+^ b) ~== (b ^+^ a)

prop_addAssocShort = addAssoc :: GLshort -> GLshort -> GLshort -> Bool
prop_addIdShort = addId :: GLshort -> Bool
prop_addNegShort = addNeg :: GLshort -> Bool
prop_addCommShort = addComm :: GLshort -> GLshort -> Bool

prop_addAssocInt = addAssoc :: GLint -> GLint -> GLint -> Bool
prop_addIdInt = addId :: GLint -> Bool
prop_addNegInt = addNeg :: GLint -> Bool
prop_addCommInt = addComm :: GLint -> GLint -> Bool

prop_addAssocFloat = addAssoc :: GLfloat -> GLfloat -> GLfloat -> Bool
prop_addIdFloat = addId :: GLfloat -> Bool
prop_addNegFloat = addNeg :: GLfloat -> Bool
prop_addCommFloat = addComm :: GLfloat -> GLfloat -> Bool

prop_addAssocDouble = addAssoc :: GLdouble -> GLdouble -> GLdouble -> Bool
prop_addIdDouble = addId :: GLdouble -> Bool
prop_addNegDouble = addNeg :: GLdouble -> Bool
prop_addCommDouble = addComm :: GLdouble -> GLdouble -> Bool

scMulDistr1 s v1 v2 = 
  s *^ (v1 ^+^ v2) ~== (s *^ v1) + (s *^ v2)
scMulDistr2 s1 s2 v =
  (s1 + s2) *^ v ~== (s1 *^ v) + (s2 *^ v)
scMulRespect s1 s2 v =
  s1 *^ (s2 *^ v) ~== (s1 * s2) *^ v
scMulId v = 1 *^ v ~== v

prop_scMulDistr1Short = scMulDistr1 :: GLshort -> GLshort -> GLshort -> Bool
prop_scMulDistr2Short = scMulDistr2 :: GLshort -> GLshort -> GLshort -> Bool
prop_scMulRespectShort = scMulRespect :: GLshort -> GLshort -> GLshort -> Bool
prop_scMulIdShort = scMulId :: GLshort -> Bool

prop_scMulDistr1Int = scMulDistr1 :: GLint -> GLint -> GLint -> Bool
prop_scMulDistr2Int = scMulDistr2 :: GLint -> GLint -> GLint -> Bool
prop_scMulRespectInt = scMulRespect :: GLint -> GLint -> GLint -> Bool
prop_scMulIdInt = scMulId :: GLint -> Bool

prop_scMulDistr1Float = scMulDistr1 :: GLfloat -> GLfloat -> GLfloat -> Bool
prop_scMulDistr2Float = scMulDistr2 :: GLfloat -> GLfloat -> GLfloat -> Bool
prop_scMulRespectFloat = scMulRespect :: GLfloat -> GLfloat -> GLfloat -> Bool
prop_scMulIdFloat = scMulId :: GLfloat -> Bool

prop_scMulDistr1Double = scMulDistr1 :: GLdouble -> GLdouble -> GLdouble -> Bool
prop_scMulDistr2Double = scMulDistr2 :: GLdouble -> GLdouble -> GLdouble -> Bool
prop_scMulRespectDouble = scMulRespect :: GLdouble -> GLdouble -> GLdouble -> Bool
prop_scMulIdDouble = scMulId :: GLdouble -> Bool

innerSym x y = (x <.> y) ~== (y <.> x)
innerLinMul a x y = ((a *^ x) <.> y) ~== a * (x <.> y)
innerLinAdd x y z = ((x ^+^ y) <.> z) ~== (x <.> z) + (y <.> z)

prop_innerSymFloat = innerSym :: GLfloat -> GLfloat -> Bool
prop_innerLinMulFloat = innerLinMul :: GLfloat -> GLfloat -> GLfloat -> Bool
prop_innerLinAddFloat = innerLinAdd :: GLfloat -> GLfloat -> GLfloat -> Bool

prop_innerSymDouble = innerSym :: GLdouble -> GLdouble -> Bool
prop_innerLinMulDouble = innerLinMul :: GLdouble -> GLdouble -> GLdouble -> Bool
prop_innerLinAddDouble = innerLinAdd :: GLdouble -> GLdouble -> GLdouble -> Bool


instance Arbitrary CShort where
  arbitrary = fromIntegral <$> (arbitrary :: Gen Int16)
instance Arbitrary CInt where
  arbitrary = fromIntegral <$> (arbitrary :: Gen Int32)
instance Arbitrary CFloat where
  arbitrary = realToFrac <$> (arbitrary :: Gen Float)
instance Arbitrary CDouble where
  arbitrary = realToFrac <$> (arbitrary :: Gen Double)

main :: IO ()
main = $(defaultMainGenerator)
