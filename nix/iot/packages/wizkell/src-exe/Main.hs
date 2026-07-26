{-# LANGUAGE OverloadedStrings #-}

import Data.IP as IP
import System.Exit
import System.Timeout

import Data.Aeson (Object (..), ToJSON (..), decode, encode)
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types
import Data.ByteString.Lazy qualified as LBS
import Data.Maybe
import GHC.Generics
import GHC.Word
import Network.Socket hiding (recv, recvFrom, send, sendTo)
import Network.Socket.ByteString (recv, recvFrom, send, sendTo)
import System.Environment (getArgs)

data Req a = Req {method :: String, params :: a}
    deriving (Generic)
instance (ToJSON a) => ToJSON (Req a)

newtype TempParams = TempParams {temp :: Int}
    deriving (Generic)
instance ToJSON TempParams

newtype StateParams = StateParams {state :: Bool}
    deriving (Generic)
instance ToJSON StateParams

req :: Word32 -> Value -> IO Value
req h a = do
    let serverAddress = SockAddrInet 38899 h

    -- Create a UDP socket
    sock <- socket AF_INET Datagram 0

    _ <- sendTo sock (LBS.toStrict (encode a)) serverAddress

    (receivedData, _) <- recvFrom sock 1024

    let decodedData = decode $ LBS.fromStrict receivedData :: Maybe Value

    -- FIXME: make typesafe
    case decodedData of
        Just myValue -> return myValue
        _ -> return Null

strToHA ip = toHostAddress (read ip :: IPv4)

timeout1s = timeout 1000000

help = do
    putStrLn "usage: toggle bulb-ip   - switch-off bulb on/off"
    putStrLn "       toggle [bulb-ip] - switch-off bulbs if any is on"
    putStrLn "                          switch-on  bulbs if all are off"
    exitFailure

main = do
    -- FIXME: make safer
    theArgs <- getArgs
    case theArgs of
        [] -> help
        (_ : []) -> help
        ("toggle" : ip : []) -> do
            singleToggle <- timeout1s $ toggleState (strToHA ip)
            case singleToggle of
                (Just res) -> print res
                (Nothing) -> do
                    putStrLn "timeout"
                    exitFailure
        ("toggle" : ips) -> do
            let hips = map strToHA ips
            bulbStates <-
                mapM
                    ( \ip -> timeout1s $ do
                        resp <- getPilot ip
                        return $ getState resp
                    )
                    hips
            if any (== (Just True)) bulbStates
                then
                    mapM_ (timeout1s . toggleState . snd) $
                        filter
                            (\x -> (fst x) == Just True)
                            (zip bulbStates hips)
                else mapM_ (timeout1s . toggleState) hips

-- setTemp (tupleToHostAddress (172, 19, 87, 59)) 3000
-- setRgb (tupleToHostAddress (172, 19, 87, 57)) 255 0 0

setTemp :: Word32 -> Int -> IO Value
setTemp h t = do
    req h $ toJSON $ Req "setState" (TempParams t)

toggleState h = do
    resp <- getPilot h
    let toggledState = not $ getState resp
    req h $ toJSON $ Req "setState" (StateParams toggledState)

-- getDevInfo h = req h $ toJSON $ Req "getDevInfo" (object [])
getPilot h = req h $ toJSON $ Req "getPilot" (object [] :: Value)

-- FIXME: lens maybe?
getState :: Value -> Bool
getState (Object o1) = case (KM.lookup "result" o1) of
    Just (Object o2) -> case (KM.lookup "state" o2) of
        Just (Bool True) -> True
        _ -> False
    _ -> False
