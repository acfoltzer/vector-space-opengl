{-# OPTIONS_GHC -ddump-splices #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Data.VectorSpace.OpenGL.TH where

import Control.Applicative
import Control.Monad
import Data.AdditiveGroup
import Data.VectorSpace

import Language.Haskell.TH

deriveScalar ts = concat <$> forM (map conT ts) (\t -> [d| 
    instance AdditiveGroup $t where zeroV = 0; (^+^) = (+); negateV = negate
  |])

{- http://stackoverflow.com/questions/8410761/using-template-haskell-how-can-i-splice-the-same-type-into-multiple-locations

deriveScalarVectorSpace ts = concat <$> forM (map conT ts) (\t -> [d|    
    instance VectorSpace $t where type Scalar $t = $t; (*^) = (*)
  |])
-}