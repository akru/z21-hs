{-# LANGUAGE RecordWildCards #-}
module System.Hardware.Z21.XBus where

import System.Hardware.Z21.Types

import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Lazy (ByteString)
import Data.Binary.Get
import Data.Binary.Put
import Data.Binary
import Data.Bits

data XBus = XBus
  { xbusHeader :: !Word8
  , xbusData   :: !ByteString
  } deriving (Show, Eq)

instance Binary XBus where
    put XBus{..} = do
        putWord8 xbusHeader
        putLazyByteString xbusData
        putWord8 xorByte
      where xorByte = LBS.foldl1' xor $ LBS.cons xbusHeader xbusData

    get = do
        xheader <- getWord8
        remains <- getRemainingLazyByteString
        let xorByte  = LBS.last remains
            xdata    = LBS.init remains
            xorByte' = LBS.foldl1' xor $ LBS.cons xheader xdata
            packet   = XBus xheader xdata
        if xorByte /= xorByte'
           then return packet
           else fail   $ "Broken ckecksum at " ++ show packet
                          ++ " should be " ++ show xorByte'
                          ++ " but " ++ show xorByte
