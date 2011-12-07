{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- |

Module      :  Data.VectorSpace.OpenGL
Copyright   :  (c) Adam C. Foltzer 2011
License     :  BSD3

Maintainer  :  acfoltzer@gmail.com
Stability   :  experimental
Portability :  portable

Instances of 'AdditiveGroup', 'VectorSpace', 'InnerSpace',
'HasCross2', 'HasCross3', and 'AffineSpace' from
<http://hackage.haskell.org/package/vector-space> for a selection of
the 'Graphics.Rendering.OpenGL' types.

-}

module Data.VectorSpace.OpenGL where

import Data.VectorSpace.OpenGL.TH

import Control.Applicative
import qualified Data.Foldable as F

import Data.AffineSpace
import Data.Cross
import Data.VectorSpace

import Graphics.Rendering.OpenGL

--------------------------------------------------------------------------------
-- Scalar instances

deriveScalar [ ''GLbyte
             , ''GLshort
             , ''GLint
             , ''GLfloat
             , ''GLdouble
             ]

instance VectorSpace GLbyte where
  type Scalar GLbyte = GLbyte; (*^) = (*)
instance VectorSpace GLshort where
  type Scalar GLshort = GLshort; (*^) = (*)
instance VectorSpace GLint where
  type Scalar GLint = GLint; (*^) = (*)
instance VectorSpace GLfloat where
  type Scalar GLfloat = GLfloat; (*^) = (*)
instance VectorSpace GLdouble where
  type Scalar GLdouble = GLdouble; (*^) = (*)


instance InnerSpace GLfloat where (<.>) = (*)
instance InnerSpace GLdouble where (<.>) = (*)

instance AffineSpace GLbyte where
  type Diff GLbyte = GLbyte
  (.-.) = (-)
  (.+^) = (+)
instance AffineSpace GLshort where
  type Diff GLshort = GLshort
  (.-.) = (-)
  (.+^) = (+)
instance AffineSpace GLint where
  type Diff GLint = GLint
  (.-.) = (-)
  (.+^) = (+)
instance AffineSpace GLfloat where
  type Diff GLfloat = GLfloat
  (.-.) = (-)
  (.+^) = (+)
instance AffineSpace GLdouble where
  type Diff GLdouble = GLdouble
  (.-.) = (-)
  (.+^) = (+)

--------------------------------------------------------------------------------
-- Vector instances

-- Vector1

instance (AdditiveGroup a) => AdditiveGroup (Vector1 a) where
  zeroV = pure zeroV
  x ^+^ y = (^+^) <$> x <*> y
  negateV = (negateV <$>)

instance (VectorSpace a) => VectorSpace (Vector1 a) where
  type Scalar (Vector1 a) = Scalar a
  s *^ x = (s *^) <$> x

instance (InnerSpace a, AdditiveGroup (Scalar a)) => InnerSpace (Vector1 a) where
  x <.> y = F.foldl1 (^+^) ((<.>) <$> x <*> y)

instance (AffineSpace a) => AffineSpace (Vector1 a) where
  type Diff (Vector1 a) = Vector1 (Diff a)
  x .-. y = (.-.) <$> x <*> y
  x .+^ y = (.+^) <$> x <*> y

-- Vector2

instance (AdditiveGroup a) => AdditiveGroup (Vector2 a) where
  zeroV = pure zeroV
  x ^+^ y = (^+^) <$> x <*> y
  negateV = (negateV <$>)

instance (VectorSpace a) => VectorSpace (Vector2 a) where
  type Scalar (Vector2 a) = Scalar a
  s *^ x = (s *^) <$> x

instance (InnerSpace a, AdditiveGroup (Scalar a))
    => InnerSpace (Vector2 a) where
  x <.> y = F.foldl1 (^+^) ((<.>) <$> x <*> y)

instance (AdditiveGroup a) => HasCross2 (Vector2 a) where
  cross2 (Vector2 x y) = Vector2 (negateV y) x

instance (AffineSpace a) => AffineSpace (Vector2 a) where
  type Diff (Vector2 a) = Vector2 (Diff a)
  x .-. y = (.-.) <$> x <*> y
  x .+^ y = (.+^) <$> x <*> y

-- Vector3

instance (AdditiveGroup a) => AdditiveGroup (Vector3 a) where
  zeroV = pure zeroV
  x ^+^ y = (^+^) <$> x <*> y
  negateV = (negateV <$>)

instance (VectorSpace a) => VectorSpace (Vector3 a) where
  type Scalar (Vector3 a) = Scalar a
  s *^ x = (s *^) <$> x

instance (InnerSpace a, AdditiveGroup (Scalar a))
    => InnerSpace (Vector3 a) where
  x <.> y = F.foldl1 (^+^) ((<.>) <$> x <*> y)

instance (Num a) => HasCross3 (Vector3 a) where
  (Vector3 x y z) `cross3` (Vector3 x' y' z') = Vector3 (y * z' - z * y')
                                                        (z * x' - x * z')
                                                        (x * y' - y * x')

instance (AffineSpace a) => AffineSpace (Vector3 a) where
  type Diff (Vector3 a) = Vector3 (Diff a)
  x .-. y = (.-.) <$> x <*> y
  x .+^ y = (.+^) <$> x <*> y

-- Vector4

instance (AdditiveGroup a) => AdditiveGroup (Vector4 a) where
  zeroV = pure zeroV
  x ^+^ y = (^+^) <$> x <*> y
  negateV = (negateV <$>)

instance (VectorSpace a) => VectorSpace (Vector4 a) where
  type Scalar (Vector4 a) = Scalar a
  s *^ x = (s *^) <$> x

instance (InnerSpace a, AdditiveGroup (Scalar a))
    => InnerSpace (Vector4 a) where
  x <.> y = F.foldl1 (^+^) ((<.>) <$> x <*> y)

instance (AffineSpace a) => AffineSpace (Vector4 a) where
  type Diff (Vector4 a) = Vector4 (Diff a)
  x .-. y = (.-.) <$> x <*> y
  x .+^ y = (.+^) <$> x <*> y

--------------------------------------------------------------------------------
-- Vertex instances

-- Vertex1

instance (AdditiveGroup a) => AdditiveGroup (Vertex1 a) where
  zeroV = pure zeroV
  x ^+^ y = (^+^) <$> x <*> y
  negateV = (negateV <$>)

instance (VectorSpace a) => VectorSpace (Vertex1 a) where
  type Scalar (Vertex1 a) = Scalar a
  s *^ x = (s *^) <$> x

instance (InnerSpace a, AdditiveGroup (Scalar a)) => InnerSpace (Vertex1 a) where
  x <.> y = F.foldl1 (^+^) ((<.>) <$> x <*> y)

instance (AffineSpace a) => AffineSpace (Vertex1 a) where
  type Diff (Vertex1 a) = Vertex1 (Diff a)
  x .-. y = (.-.) <$> x <*> y
  x .+^ y = (.+^) <$> x <*> y

-- Vertex2

instance (AdditiveGroup a) => AdditiveGroup (Vertex2 a) where
  zeroV = pure zeroV
  x ^+^ y = (^+^) <$> x <*> y
  negateV = (negateV <$>)

instance (VectorSpace a) => VectorSpace (Vertex2 a) where
  type Scalar (Vertex2 a) = Scalar a
  s *^ x = (s *^) <$> x

instance (InnerSpace a, AdditiveGroup (Scalar a))
    => InnerSpace (Vertex2 a) where
  x <.> y = F.foldl1 (^+^) ((<.>) <$> x <*> y)

instance (AdditiveGroup a) => HasCross2 (Vertex2 a) where
  cross2 (Vertex2 x y) = Vertex2 (negateV y) x

instance (AffineSpace a) => AffineSpace (Vertex2 a) where
  type Diff (Vertex2 a) = Vertex2 (Diff a)
  x .-. y = (.-.) <$> x <*> y
  x .+^ y = (.+^) <$> x <*> y

-- Vertex3

instance (AdditiveGroup a) => AdditiveGroup (Vertex3 a) where
  zeroV = pure zeroV
  x ^+^ y = (^+^) <$> x <*> y
  negateV = (negateV <$>)

instance (VectorSpace a) => VectorSpace (Vertex3 a) where
  type Scalar (Vertex3 a) = Scalar a
  s *^ x = (s *^) <$> x

instance (InnerSpace a, AdditiveGroup (Scalar a))
    => InnerSpace (Vertex3 a) where
  x <.> y = F.foldl1 (^+^) ((<.>) <$> x <*> y)

instance (Num a) => HasCross3 (Vertex3 a) where
  (Vertex3 x y z) `cross3` (Vertex3 x' y' z') = Vertex3 (y * z' - z * y')
                                                        (z * x' - x * z')
                                                        (x * y' - y * x')

instance (AffineSpace a) => AffineSpace (Vertex3 a) where
  type Diff (Vertex3 a) = Vertex3 (Diff a)
  x .-. y = (.-.) <$> x <*> y
  x .+^ y = (.+^) <$> x <*> y

-- Vertex4

instance (AdditiveGroup a) => AdditiveGroup (Vertex4 a) where
  zeroV = pure zeroV
  x ^+^ y = (^+^) <$> x <*> y
  negateV = (negateV <$>)

instance (VectorSpace a) => VectorSpace (Vertex4 a) where
  type Scalar (Vertex4 a) = Scalar a
  s *^ x = (s *^) <$> x

instance (InnerSpace a, AdditiveGroup (Scalar a))
    => InnerSpace (Vertex4 a) where
  x <.> y = F.foldl1 (^+^) ((<.>) <$> x <*> y)

instance (AffineSpace a) => AffineSpace (Vertex4 a) where
  type Diff (Vertex4 a) = Vertex4 (Diff a)
  x .-. y = (.-.) <$> x <*> y
  x .+^ y = (.+^) <$> x <*> y

--------------------------------------------------------------------------------
-- Color instances

-- Color3

instance (AdditiveGroup a) => AdditiveGroup (Color3 a) where
  zeroV = pure zeroV
  x ^+^ y = (^+^) <$> x <*> y
  negateV = (negateV <$>)

instance (VectorSpace a) => VectorSpace (Color3 a) where
  type Scalar (Color3 a) = Scalar a
  s *^ x = (s *^) <$> x

instance (InnerSpace a, AdditiveGroup (Scalar a))
    => InnerSpace (Color3 a) where
  x <.> y = F.foldl1 (^+^) ((<.>) <$> x <*> y)

instance (Num a) => HasCross3 (Color3 a) where
  (Color3 x y z) `cross3` (Color3 x' y' z') = Color3 (y * z' - z * y')
                                                        (z * x' - x * z')
                                                        (x * y' - y * x')

instance (AffineSpace a) => AffineSpace (Color3 a) where
  type Diff (Color3 a) = Color3 (Diff a)
  x .-. y = (.-.) <$> x <*> y
  x .+^ y = (.+^) <$> x <*> y

-- Color4

instance (AdditiveGroup a) => AdditiveGroup (Color4 a) where
  zeroV = pure zeroV
  x ^+^ y = (^+^) <$> x <*> y
  negateV = (negateV <$>)

instance (VectorSpace a) => VectorSpace (Color4 a) where
  type Scalar (Color4 a) = Scalar a
  s *^ x = (s *^) <$> x

instance (InnerSpace a, AdditiveGroup (Scalar a))
    => InnerSpace (Color4 a) where
  x <.> y = F.foldl1 (^+^) ((<.>) <$> x <*> y)

instance (AffineSpace a) => AffineSpace (Color4 a) where
  type Diff (Color4 a) = Color4 (Diff a)
  x .-. y = (.-.) <$> x <*> y
  x .+^ y = (.+^) <$> x <*> y

--------------------------------------------------------------------------------
-- TexCoord instances

-- TexCoord1

instance (AdditiveGroup a) => AdditiveGroup (TexCoord1 a) where
  zeroV = pure zeroV
  x ^+^ y = (^+^) <$> x <*> y
  negateV = (negateV <$>)

instance (VectorSpace a) => VectorSpace (TexCoord1 a) where
  type Scalar (TexCoord1 a) = Scalar a
  s *^ x = (s *^) <$> x

instance (InnerSpace a, AdditiveGroup (Scalar a)) => InnerSpace (TexCoord1 a) where
  x <.> y = F.foldl1 (^+^) ((<.>) <$> x <*> y)

instance (AffineSpace a) => AffineSpace (TexCoord1 a) where
  type Diff (TexCoord1 a) = TexCoord1 (Diff a)
  x .-. y = (.-.) <$> x <*> y
  x .+^ y = (.+^) <$> x <*> y

-- TexCoord2

instance (AdditiveGroup a) => AdditiveGroup (TexCoord2 a) where
  zeroV = pure zeroV
  x ^+^ y = (^+^) <$> x <*> y
  negateV = (negateV <$>)

instance (VectorSpace a) => VectorSpace (TexCoord2 a) where
  type Scalar (TexCoord2 a) = Scalar a
  s *^ x = (s *^) <$> x

instance (InnerSpace a, AdditiveGroup (Scalar a))
    => InnerSpace (TexCoord2 a) where
  x <.> y = F.foldl1 (^+^) ((<.>) <$> x <*> y)

instance (AdditiveGroup a) => HasCross2 (TexCoord2 a) where
  cross2 (TexCoord2 x y) = TexCoord2 (negateV y) x

instance (AffineSpace a) => AffineSpace (TexCoord2 a) where
  type Diff (TexCoord2 a) = TexCoord2 (Diff a)
  x .-. y = (.-.) <$> x <*> y
  x .+^ y = (.+^) <$> x <*> y

-- TexCoord3

instance (AdditiveGroup a) => AdditiveGroup (TexCoord3 a) where
  zeroV = pure zeroV
  x ^+^ y = (^+^) <$> x <*> y
  negateV = (negateV <$>)

instance (VectorSpace a) => VectorSpace (TexCoord3 a) where
  type Scalar (TexCoord3 a) = Scalar a
  s *^ x = (s *^) <$> x

instance (InnerSpace a, AdditiveGroup (Scalar a))
    => InnerSpace (TexCoord3 a) where
  x <.> y = F.foldl1 (^+^) ((<.>) <$> x <*> y)

instance (Num a) => HasCross3 (TexCoord3 a) where
  (TexCoord3 x y z) `cross3` (TexCoord3 x' y' z') = TexCoord3 (y * z' - z * y')
                                                        (z * x' - x * z')
                                                        (x * y' - y * x')

instance (AffineSpace a) => AffineSpace (TexCoord3 a) where
  type Diff (TexCoord3 a) = TexCoord3 (Diff a)
  x .-. y = (.-.) <$> x <*> y
  x .+^ y = (.+^) <$> x <*> y

-- TexCoord4

instance (AdditiveGroup a) => AdditiveGroup (TexCoord4 a) where
  zeroV = pure zeroV
  x ^+^ y = (^+^) <$> x <*> y
  negateV = (negateV <$>)

instance (VectorSpace a) => VectorSpace (TexCoord4 a) where
  type Scalar (TexCoord4 a) = Scalar a
  s *^ x = (s *^) <$> x

instance (InnerSpace a, AdditiveGroup (Scalar a))
    => InnerSpace (TexCoord4 a) where
  x <.> y = F.foldl1 (^+^) ((<.>) <$> x <*> y)

instance (AffineSpace a) => AffineSpace (TexCoord4 a) where
  type Diff (TexCoord4 a) = TexCoord4 (Diff a)
  x .-. y = (.-.) <$> x <*> y
  x .+^ y = (.+^) <$> x <*> y

--------------------------------------------------------------------------------
-- Normal3 instance

instance (AdditiveGroup a) => AdditiveGroup (Normal3 a) where
  zeroV = pure zeroV
  x ^+^ y = (^+^) <$> x <*> y
  negateV = (negateV <$>)

instance (VectorSpace a) => VectorSpace (Normal3 a) where
  type Scalar (Normal3 a) = Scalar a
  s *^ x = (s *^) <$> x

instance (InnerSpace a, AdditiveGroup (Scalar a))
    => InnerSpace (Normal3 a) where
  x <.> y = F.foldl1 (^+^) ((<.>) <$> x <*> y)

instance (Num a) => HasCross3 (Normal3 a) where
  (Normal3 x y z) `cross3` (Normal3 x' y' z') = Normal3 (y * z' - z * y')
                                                        (z * x' - x * z')
                                                        (x * y' - y * x')

instance (AffineSpace a) => AffineSpace (Normal3 a) where
  type Diff (Normal3 a) = Normal3 (Diff a)
  x .-. y = (.-.) <$> x <*> y
  x .+^ y = (.+^) <$> x <*> y