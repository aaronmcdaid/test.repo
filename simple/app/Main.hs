{-# LANGUAGE TemplateHaskell #-}

import System.Environment (getArgs)
import Control.Distributed.Process
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Distributed.Process.Backend.SimpleLocalnet

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node
import Network.Transport.TCP (createTransport, defaultTCPParameters)

import System.IO(hFlush,stdout)

sampleTask :: () -> Process ()
sampleTask _ = do
                        selfPid <- getSelfPid
                        let onemessage message_count total_of_i_mi = do
                                m <- expectTimeout 1000000 :: Process (Maybe Double)
                                case m of
                                    Nothing  -> do    -- all done, print the tuple and end
                                                    say ("bye: " ++ show (message_count, total_of_i_mi))
                                                    liftIO $ do
                                                                print ("Bye: " ++ show (message_count, total_of_i_mi))
                                                                hFlush stdout
                                    Just m_i -> do    -- add to the accumulators and recurse
                                                    -- say $ "Got " ++ show (message_count, total_of_i_mi) ++ " back!"
                                                    onemessage (message_count+1) (total_of_i_mi+m_i)
                        onemessage 0 (0.0::Double)
                        -- liftIO (print "hi" >> threadDelay (t * 1000000))

remotable ['sampleTask]
myRemoteTable :: RemoteTable
myRemoteTable = Main.__remoteTable initRemoteTable

master :: Backend -> [NodeId] -> Process ()
master backend slaves = do
   -- Do something interesting with the slaves
   liftIO . putStrLn $ "Slaves: " ++ show slaves
   pids <- mapM (\slave -> spawn (slaves !! 2) $ $(mkClosure 'sampleTask) ()) slaves

   mapM ( (flip send) (0.001 :: Double) ) pids
   mapM ( (flip send) (0.001 :: Double) ) pids

   -- Terminate the slaves when the master terminates
   liftIO $ threadDelay (5000000)
   terminateAllSlaves backend

main :: IO ()
main = do
   args <- getArgs

   case args of
     ["master", host, port] -> do
       backend <- initializeBackend host port myRemoteTable
       startMaster backend (master backend)
     ["slave", host, port] -> do
       backend <- initializeBackend host port myRemoteTable
       startSlave backend
