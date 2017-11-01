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

sampleTask :: () -> Process ()
sampleTask _ = do
                        let onemessage message_count total_of_i_mi = do
                                m <- expectTimeout 100000 :: Process (Maybe Double)
                                case m of
                                    Nothing  -> do    -- all done, print the tuple and end
                                                    liftIO . print $ ("Bye: " ++ show (message_count, total_of_i_mi))
                                    Just m_i -> do    -- add to the accumulators and recurse
                                                    say $ "Got " ++ show (message_count, total_of_i_mi) ++ " back!"
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
   pid0<- spawn (slaves !! 0) $ $(mkClosure 'sampleTask) ()
   pid1<- spawn (slaves !! 1) $ $(mkClosure 'sampleTask) ()
   pid2<- spawn (slaves !! 2) $ $(mkClosure 'sampleTask) ()
   send pid0 (1.41::Double)
   send pid1 (1.41::Double)
   send pid2 (1.41::Double)
   send pid0 (2.7::Double)
   send pid1 (2.7::Double)
   send pid2 (2.7::Double)
   send pid0 (3.14::Double)
   send pid1 (3.14::Double)
   send pid2 (3.14::Double)

   -- Terminate the slaves when the master terminates
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
