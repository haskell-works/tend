{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE OverloadedStrings      #-}

module HaskellWorks.Ci.Types
  ( module X
  ) where

import HaskellWorks.Ci.Types.CiConfig           as X
import HaskellWorks.Ci.Types.CircleConfig       as X
import HaskellWorks.Ci.Types.GithubRemote       as X
import HaskellWorks.Ci.Types.ProjectConfig      as X
import HaskellWorks.Ci.Types.VariableAssignment as X

-- auto :: (GenericInterpret (Rep a), Generic a, Interpret a) => Type a
-- auto = autoWith (defaultInterpretOptions { fieldModifier = TL.dropWhile (== '_') })
