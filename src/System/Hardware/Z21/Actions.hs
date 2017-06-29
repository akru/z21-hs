module System.Hardware.Z21.Actions where

import System.Hardware.Z21.Types
import System.Hardware.Z21.XBus

import Control.Monad.IO.Class (liftIO)
import System.Hardware.Z21.Types
import qualified Data.ByteString.Base16.Lazy as B16
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Lazy (ByteString)
import Data.Maybe (fromJust)
import Data.Conduit
import Data.Binary
import Data.Word
import Data.Bits

data Action
  = GetSerial
  | Logoff
  | XGetLocoInfo Address
  | XSetLocoDrive Address Word8 Word8
  | XGetTurnoutInfo Address
  | XSetTurnout Address Word8

pack :: Action -> Packet
pack GetSerial              = Packet 0x10 ()
pack Logoff                 = Packet 0x30 ()
pack (XGetLocoInfo a)       = Packet 0x40 $ XBus 0xE3 $ LBS.pack [0xF0, msb a, lsb a]
pack (XSetLocoDrive a s rv) = Packet 0x40 $ XBus 0xE4 $ LBS.pack [s, msb a, lsb a, rv]
pack (XGetTurnoutInfo a)    = Packet 0x40 $ XBus 0x43 $ LBS.pack [msb a, lsb a]
pack (XSetTurnout a cmd)    = Packet 0x40 $ XBus 0x53 $ LBS.pack [msb a, lsb a, cmd]

unpack :: Binary a => Packet -> Maybe a
unpack (Packet _ dat) =
    case decodeOrFail (encode dat) of
        Right (_, _, a) -> Just a
        _               -> Nothing

zprint :: Show a => a -> Z21 ()
zprint = liftIO . print

getSerial :: Z21 (Maybe Word32)
getSerial = do
    yield $ pack GetSerial
    res <- await
    return (res >>= unpack)

getLocoInfo :: Address -> Z21 (Maybe ByteString)
getLocoInfo a = do
    yield $ pack (XGetLocoInfo a)
    res <- await
    return (B16.encode . xbusData <$> (res >>= unpack))

setLocoDrive :: Address -> Word8 -> Word8 -> Z21 ()
setLocoDrive a s rv = yield $ pack (XSetLocoDrive a s rv)

getTurnoutInfo :: Address -> Z21 (Maybe ByteString)
getTurnoutInfo a = do
    yield $ pack (XGetTurnoutInfo a)
    res <- await
    return (B16.encode . xbusData <$> (res >>= unpack))

setTurnout:: Address -> Word8 -> Z21 ()
setTurnout a c = yield $ pack (XSetTurnout a c)
