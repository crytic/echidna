module Echidna.Constants (
    ethRunAddress 
  , ethOtherAddress
  , ethZeroAddress
  , ethOwnerAddress
) where 

import Data.DoubleWord       (Word160(..))

ethRunAddress :: Word160 
ethRunAddress = 0xd30a286ec6737b8b2a6a7b5fbb5d75b895f62956

ethOtherAddress :: Word160 
ethOtherAddress = 0x67518339e369ab3d591d3569ab0a0d83b2ff5198

ethOwnerAddress :: Word160
ethOwnerAddress = 0x00a329c0648769a73afac7f9381e08fb43dbea72

ethZeroAddress :: Word160 
ethZeroAddress = 0x0 
