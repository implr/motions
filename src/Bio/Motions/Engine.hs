{- |
Module      : Bio.Motions.Engine
Description : Contains the simulation engine.
License     : Apache
Stability   : experimental
Portability : unportable
 -}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
module Bio.Motions.Engine where

import Bio.Motions.Types
import Bio.Motions.Common
import Bio.Motions.Representation.Class
import Bio.Motions.Representation.Common
import Bio.Motions.Representation.Dump
import Bio.Motions.Utils.FreezePredicateParser
import Text.Parsec.String

import Control.Monad.State
import Control.Monad.Random
import Control.Monad.Trans.Maybe
import System.IO
import Control.Lens
import Data.List
import Data.Maybe

data SimulationState repr = SimulationState
    { repr :: repr
    , stepCounter :: Int
    , frameCounter :: Int
    }

-- |Describes how the simulation should run.
data RunSettings repr score = RunSettings
    { 
    numSteps :: Int
    -- ^ Number of simulation steps.
    , verboseCallbacks :: Bool
    -- ^ Enable verbose callback output.
    , freezeFile :: Maybe FilePath
    -- ^ A file containing the ranges of the frozen beads' indices.
    }

step :: (MonadRandom m, MonadState (SimulationState repr) m,
         Representation (MaybeT m) repr) => m (Maybe Move)
step = runMaybeT $ do
    st@SimulationState{..} <- get
    move <- generateMove repr

    (newRepr, _) <- performMove move repr

    put st { repr = newRepr
           }

    pure move
  where
    factor :: Double
    factor = 2


stepAndWrite :: _ => Handle -> m ()
stepAndWrite callbacksHandle = do
    step -- TODO: do something with the move

    modify $ \s -> s { stepCounter = stepCounter s + 1 }


simulate :: _ => RunSettings repr score -> Dump -> m Dump
simulate (RunSettings{..} :: RunSettings repr score) dump = do
    freezePredicate <- case freezeFile of
        Just file -> liftIO (parseFromFile freezePredicateParser file) >>= either (fail . show) pure
        Nothing -> pure freezeNothing
    repr :: repr <- loadDump dump freezePredicate

    let evs = nub . map dumpBeadEV . concat . dumpChains $ dump
        bts = nub . map (^. binderType) . dumpBinders $ dump
        chs = nub . map (^. beadChain) . concat . dumpIndexedChains $ dump

    let stepCounter = 0
        frameCounter = 0
        st = SimulationState{..}

    let callbacksHandle = stdout
    st' <- flip execStateT st $
        replicateM_ numSteps $ stepAndWrite callbacksHandle

    let SimulationState{..} = st'
    makeDump repr
  where
    pdbError = "The PDB format can't handle this number of different beads, binders or chains."
