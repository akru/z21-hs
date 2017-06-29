module System.Hardware.Z21.Actions where

import System.Hardware.Z21.Types
import System.Hardware.Z21.XBus

import Control.Monad.IO.Class (liftIO)
import System.Hardware.Z21.Types
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromJust)
import Data.Conduit
import Data.Binary
import Data.Word

data Action
  = GetSerial
  | Logoff
  | XGetLocoInfo Address
  | XSetLocoDrive Address Word8 Word8

pack :: Action -> Packet
pack GetSerial              = Packet 0x10 ()
pack Logoff                 = Packet 0x30 ()
pack (XGetLocoInfo a)       = Packet 0x40 $ XBus 0xE3 $ LBS.pack [0xF0, msb a, lsb a]
pack (XSetLocoDrive a s rv) = Packet 0x40 $ XBus 0xE4 $ LBS.pack [s, msb a, lsb a, rv]

unpack :: Binary a => Packet -> Maybe a
unpack (Packet _ dat) =
    case decodeOrFail (encode dat) of
        Right (_, _, a) -> Just a
        _               -> Nothing

zprint :: Show a => a -> Z21 ()
zprint = liftIO . print

getSerial :: Z21 (Maybe Word16)
getSerial = do
    yield $ pack GetSerial
    res <- await
    return (res >>= unpack)
