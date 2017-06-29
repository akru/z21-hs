{-# LANGUAGE GADTs #-}
module System.Hardware.Z21.Types where

import qualified Data.ByteString.Base16.Lazy as B16
import qualified Data.ByteString.Lazy as LBS
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
    Packet :: Binary a => Word16 -> a -> Packet

instance Binary Packet where
    get = do
        len <- getWord16le
        hdr <- getWord16le
        dat <- getLazyByteString $ fromIntegral (len - 4)
        return (Packet hdr dat)

    put (Packet hdr dat) = do
        putWord16le $ fromIntegral (LBS.length $ encode dat)
        putWord16le hdr
        put dat

instance Show Packet where
    show = show . B16.encode . encode

data Address = Address
  { msb :: Word8
  , lsb :: Word8
  } deriving (Show, Eq, Ord)

type Z21 = ConduitM Packet Packet IO

runZ21 :: String -> Int -> Z21 () -> IO ()
runZ21 host port con = do
    (addr, sock) <- getSocket
    runConduit $
        sourceSocket sock 512
        =$= CL.map msgData
        =$= conduitDecode
        =$= con
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
