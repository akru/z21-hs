{-# LANGUAGE GADTs #-}
module System.Hardware.Z21.Types where

import qualified Data.ByteString.Base16.Lazy as B16
import Network.Socket (Socket, SockAddr)
import qualified Network.Socket as NS
import qualified Data.Conduit.List as CL
import Data.Conduit.Serialization.Binary
import Data.Conduit.Network.UDP
import Data.Conduit
import Data.Binary.Get
import Data.Binary.Put
import Data.Binary

data Packet where
    Packet :: Binary a => Word16 -> Word16 -> a -> Packet

instance Binary Packet where
    get = do
        len <- getWord16be
        hdr <- getWord16be
        dat <- getLazyByteString $ fromIntegral (len - 4)
        return (Packet len hdr dat)

    put (Packet len hdr dat) = do
        putWord16le len
        putWord16le hdr
        put dat

instance Show Packet where
    show = show . B16.encode . encode

type Address = Word16

data Action
  = GetSerial
  | Logoff
  | GetLoco Address
  | SetLoco Address Word8
  | GetTurnout Address
  | SetTurnout Address Word8

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

type Z21 = ConduitM Packet Action IO

runZ21 :: String -> Int -> Z21 () -> IO ()
runZ21 host port con = do
    (addr, sock) <- getSocket
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
  where getSocket = do
            let hints = NS.defaultHints { NS.addrFlags = [NS.AI_ADDRCONFIG]
                                        , NS.addrSocketType = NS.Datagram }
            (addr : _) <- NS.getAddrInfo (Just hints) (Just host) (Just $ show port)
            sock <- NS.socket (NS.addrFamily addr)
                              (NS.addrSocketType addr)
                              (NS.addrProtocol addr)
            return (NS.addrAddress addr, sock)
