{-# LANGUAGE FlexibleContexts #-}

module HaskellWorks.Ci.Dhall
  ( module X
  , auto
  ) where

import Dhall        as X hiding (auto)
import GHC.Generics

import qualified Data.Text.Lazy as TL

auto :: (GenericInterpret (Rep a), Generic a, Interpret a) => Type a
auto = autoWith (defaultInterpretOptions { fieldModifier = TL.toStrict . TL.dropWhile (== '_') . TL.fromStrict })
