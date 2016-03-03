{- |
Module      : Bio.Motions.Common
Description : Common utility functions for working with common types.
License     : Apache
Stability   : experimental
Portability : unportable
-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Bio.Motions.Common where

import Bio.Motions.Types
import Control.Lens
import qualified Data.Vector.Unboxed as U

laminType :: BinderType
laminType = BinderType 0

doesNotBind :: EnergyVector -> Bool
doesNotBind = U.all (== 0) . getEnergyVector

bindsWithLamins :: EnergyVector -> Bool
bindsWithLamins = (/= 0) . (U.! getBinderType laminType) . getEnergyVector

-- |A convenient unwrapper of 'wrappedPosition'.
position :: Lens' (Located a) Vec3
position = wrappedPosition . _Wrapping Identity
{-# INLINE position #-}

