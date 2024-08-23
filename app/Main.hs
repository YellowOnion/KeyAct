{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T

import Control.Monad (forever, forM_)
import Control.Concurrent
import Control.Concurrent.STM

import qualified System.Environment as Env
import GHC.Clock (getMonotonicTimeNSec)

import qualified Network.WebSockets as WS

import qualified Evdev as Ev
import qualified Evdev.Codes as EvCodes
import qualified Evdev.Codes as Ev

import Debug.Trace (traceShow)

keys = [ EvCodes.KeyQ, EvCodes.KeyW, EvCodes.KeyE, EvCodes.KeyA, EvCodes.KeyS
       , EvCodes.KeyD, EvCodes.KeyF, EvCodes.KeySpace, EvCodes.BtnLeft, EvCodes.BtnRight, EvCodes.BtnMiddle ]

evToText :: Ev.EventData -> Maybe String
evToText (Ev.KeyEvent key state)
  | key `elem` keys && state /= Ev.Repeated = Just $ show key ++ "::" ++ show state
  | otherwise = Nothing
evToText e = Nothing

parseEvent :: Ev.Event -> Maybe String
parseEvent (Ev.Event evData _) = evToText evData

main :: IO ()
main = do
  args <- Env.getArgs
  kb <- BS.fromFilePath $ args !! 0
  mouse <- BS.fromFilePath $ args !! 1
  BS.putStrLn $ "Opening device at: " <> kb
  BS.putStrLn $ "Opening device at: " <> mouse
  outputChan <- atomically $ newBroadcastTChan :: IO (TChan String)

  forkIO . WS.runServer ("127.0.0.1") 9160
    $ \pending -> do
        conn <- WS.acceptRequest pending

        chan <- atomically $ dupTChan outputChan
        forkIO . forever $ do
          _ :: T.Text <- WS.receiveData conn
          return ()

        forever $ do
          msg <- atomically $ readTChan chan
          msg `seq` WS.sendTextData conn $ T.pack msg

  devKb <- Ev.newDevice kb
  forkIO . forever $  do
    event <- Ev.nextEvent devKb
    forM_ (parseEvent event) (atomically . writeTChan outputChan)

  devMouse <- Ev.newDevice mouse
  forkIO . forever $  do
    event <- Ev.nextEvent devMouse
    forM_ (parseEvent event) (atomically . writeTChan outputChan)

  _ <- getLine
  return ()
