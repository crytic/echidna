module Echidna.Defaults where

import Echidna.Types (Campaign(..), GenDict, mkGenDict)

-- | The default value of a 'GenDict'.
defaultDict :: GenDict
defaultDict = mkGenDict 0 [] [] 0 (const Nothing)

-- | The default value of a 'Campaign'.
defaultCampaign :: Campaign
defaultCampaign = Campaign mempty mempty defaultDict
