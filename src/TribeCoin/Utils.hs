module TribeCoin.Utils
  ( sha256
  , ripemd160
  ) where

import Crypto.Hash (Digest, SHA256, RIPEMD160, hash)
import qualified Data.ByteString as BS (ByteString)

sha256 :: BS.ByteString -> Digest SHA256
sha256 = hash

ripemd160 :: BS.ByteString -> Digest RIPEMD160
ripemd160 = hash