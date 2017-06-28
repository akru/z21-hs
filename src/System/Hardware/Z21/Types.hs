{-# LANGUAGE GADTs #-}
module System.Hardware.Z21.Types where

import qualified Data.ByteString.Base16.Lazy as B16
import Data.Binary.Get
import Data.Binary.Put
import Data.Binary

data Packet where
    Packet :: Binary a => Word16 -> Word16 -> a -> Packet

instance Binary Packet where
    get = do
        len <- getWord16le
        hdr <- getWord16le
        dat <- getLazyByteString $ fromIntegral (len - 4)
        return (Packet len hdr dat)

    put (Packet len hdr dat) = do
        putWord16le len
        putWord16le hdr
        put dat

instance Show Packet where
    show = show . B16.encode . encode

newtype Address = Address Word16
  deriving (Eq, Ord, Show)

instance Binary Address where
    get = Address <$> getWord16be
    put (Address a) = putWord16be a

data Action
  = GetSerial
  | Logoff
  | GetLoco Address
  | SetLoco Address Word8
  | GetTurnout Address
  | SetTurnout Address Word8
