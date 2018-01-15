module BeetCoin.Core.Utils
  ( sha256
  , ripemd160
  ) where

import Crypto.Hash (Digest, SHA256, RIPEMD160, hash)
import Data.ByteArray (ByteArrayAccess)

sha256 :: ByteArrayAccess ba => ba -> Digest SHA256
sha256 = hash

ripemd160 :: ByteArrayAccess ba => ba -> Digest RIPEMD160
ripemd160 = hash
