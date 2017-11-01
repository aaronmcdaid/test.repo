module Main where

import Network.Transport.TCP (createTransport, defaultTCPParameters)
import Control.Distributed.Process
import Control.Distributed.Process.Node

import Lib

main :: IO ()
main = do
    Right t <- createTransport "127.0.0.1" "10501" defaultTCPParameters
    node <- newLocalNode t initRemoteTable
    print "hi"
    _ <- runProcess node $ do
        -- get our own process id
        self <- getSelfPid
        send self (3.1415 ::        Double)
        hello <- expect :: Process  Double
        liftIO $ putStrLn (show [hello])
    return ()
