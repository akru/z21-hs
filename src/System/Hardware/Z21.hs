module System.Hardware.Z21 where

import Network.Socket (Socket, SockAddr)
import qualified Network.Socket as NS
import qualified Data.Conduit.List as CL
import Data.Conduit.Serialization.Binary
import Data.Conduit.Network.UDP
import Data.Conduit
import Data.Binary

import System.Hardware.Z21.Types

pack :: Action -> Packet
pack GetSerial        = Packet 0x04 0x10 ()
pack Logoff           = Packet 0x04 0x30 ()
pack (GetLoco a)      = Packet 0x06 0x60 a
pack (SetLoco a m)    = Packet 0x07 0x61 (a, m)
pack (GetTurnout a)   = Packet 0x06 0x70 a
pack (SetTurnout a m) = Packet 0x07 0x71 (a, m)

unpack :: Binary a => Packet -> Maybe a
unpack (Packet _ _ dat) =
    case decodeOrFail (encode dat) of
        Right (_, _, a) -> Just a
        _               -> Nothing

type Z21 = Conduit Packet IO Action

runZ21 :: String -> Int -> Z21 -> IO ()
runZ21 host port con = do
    (addr, sock) <- getSocket host port
    runConduit $
        sourceSocket sock 512
        =$= CL.map msgData
        =$= conduitDecode
        =$= con
        =$= CL.map pack
        =$= conduitEncode
        =$= CL.map (flip Message addr)
        =$= sinkToSocket sock
    NS.close sock

-- | Attempt to connect to the given host/port
getSocket :: String -> Int -> IO (SockAddr, Socket)
getSocket host' port' = do
    let hints = NS.defaultHints { NS.addrFlags = [NS.AI_ADDRCONFIG]
                                , NS.addrSocketType = NS.Datagram }
    (addr : _) <- NS.getAddrInfo (Just hints) (Just host') (Just $ show port')
    sock <- NS.socket (NS.addrFamily addr) (NS.addrSocketType addr) (NS.addrProtocol addr)
    return (NS.addrAddress addr, sock)
