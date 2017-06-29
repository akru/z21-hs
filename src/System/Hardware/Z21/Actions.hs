module System.Hardware.Z21.Actions where

import System.Hardware.Z21.Types
import Data.Maybe (fromJust)
import Data.Conduit
import Data.Word

getSerial :: Z21 (Maybe Word16)
getSerial = do
    yield GetSerial
    res <- await
    return (res >>= unpack)

getLocoMode :: Address -> Z21 (Maybe Word8)
getLocoMode a = do
    yield (GetLoco a)
    res <- await
    return (res >>= unpack)

setLocoMode :: Address -> Word8 -> Z21 ()
setLocoMode a m = yield (SetLoco a m)

getTurnoutMode :: Address -> Z21 (Maybe Word8)
getTurnoutMode a = do
    yield (GetTurnout a)
    res <- await
    return (res >>= unpack)

setTurnoutMode :: Address -> Word8 -> Z21 ()
setTurnoutMode a m = yield (SetTurnout a m)
