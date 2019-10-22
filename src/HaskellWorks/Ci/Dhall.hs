{-# LANGUAGE FlexibleContexts #-}

module HaskellWorks.Ci.Dhall
  ( module X
  , auto
  ) where

import Dhall as X hiding (auto)

import qualified Data.Text.Lazy as TL

auto :: Interpret a => Type a
auto = autoWith (defaultInterpretOptions { fieldModifier = TL.toStrict . TL.dropWhile (== '_') . TL.fromStrict })
