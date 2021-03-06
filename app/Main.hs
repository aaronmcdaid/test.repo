{-# LANGUAGE TemplateHaskell #-}

-- next two for options processing, https://stackoverflow.com/a/39049486/146041
import Options.Applicative
import Data.Monoid ((<>))

import Control.Monad (forever,when,replicateM_)

import Control.Distributed.Process
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Distributed.Process.Backend.SimpleLocalnet

import Control.Concurrent (threadDelay)
import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node
import Network.Transport.TCP (createTransport, defaultTCPParameters)

import System.IO(hFlush,stdout,stderr,hPutStrLn)
import System.Exit(exitSuccess,exitWith, ExitCode(ExitFailure))

import Data.Time.Clock
import System.Random


listenAndAccumulate :: Int -> Process ()
listenAndAccumulate waitfor = do
                        -- define a recursive message-handler to accumulate the counters
                        let onemessage message_count total_of_i_mi = do
                                m <- expectTimeout (waitfor*1000000) :: Process (Maybe Double)
                                case m of
                                    Nothing  -> do  -- all done, print the tuple and end
                                                    -- say $ show (message_count, total_of_i_mi)
                                                    liftIO $ do
                                                                print (message_count, total_of_i_mi)
                                                                hFlush stdout
                                    Just m_i -> do    -- add to the accumulators and recurse
                                                    -- say $ "Got " ++ show (message_count, total_of_i_mi) ++ " back!"
                                                    onemessage (message_count+1) (total_of_i_mi + (message_count+1)*m_i)
                        onemessage 0 (0.0::Double)

remotable ['listenAndAccumulate]
myRemoteTable :: RemoteTable
myRemoteTable = Main.__remoteTable initRemoteTable

master :: Int -> Int -> Int -> Backend -> [NodeId] -> Process ()
master seed sendfor waitfor backend slaves = do
    -- Do something interesting with the slaves
    liftIO $ hPutStrLn stderr ( "Slaves: " ++ show slaves )

    -- spawn the accumulator on each slave
    pids <- mapM (\slave -> spawn slave $ $(mkClosure 'listenAndAccumulate) waitfor) slaves

    startTime <- liftIO getCurrentTime

    -- another recursive action, sending a random number to each
    -- slave, then checking the time, and restarting if 'send-for'
    -- seconds has not yet elapsed
    let send_and_check_time g = do
            let (d,g') = random g -- store the new state to pass recursively below
            mapM ( (flip send) (d :: Double) ) pids

            currentTime <- liftIO getCurrentTime
            when ((diffUTCTime currentTime startTime) < fromIntegral sendfor) $ send_and_check_time g'

    let g = mkStdGen seed
    send_and_check_time g

    -- Terminate the slaves when the master terminates
    liftIO $ threadDelay (1000000*(waitfor+1))
    terminateAllSlaves backend

main :: IO ()
main = do

    -- the masters and the slaves both come in here at first

    -- First, process the command line args
    let opts = info (helper <*> sample)
          ( fullDesc
         <> progDesc " -k INT and -l INT are necessary. -s INT defaults to 1337"
         <> header "???" )
    args_kls <- execParser opts
    let arg_k = k args_kls --send-for
    let arg_l = l args_kls --wait-for
    let arg_s = s args_kls --with-seed
    let arg_mos = master_or_slave args_kls
    let arg_h = host args_kls
    let arg_p = port args_kls

    when  (   arg_k == -1
         || arg_l == -1
      ) $ do
            putStrLn "\n\n you must specify '--send-for SECONDS' and '--wait-for SECONDS'"
            exitWith (ExitFailure 1)

    -- hPutStrLn stderr $ show args_kls

    case arg_mos of
     "master" -> do
       --liftIO
       threadDelay 500000 -- half a second to give the slaves a little time to wake up
       backend <- initializeBackend arg_h arg_p myRemoteTable
       startMaster backend (master arg_s arg_k arg_l backend)
     "slave" -> do
       backend <- initializeBackend arg_h arg_p myRemoteTable
       startSlave backend

data CommandLineArgs = CommandLineArgs
    { master_or_slave :: String
    , host :: String
    , port :: String
    , k :: Int -- send-for
    , l :: Int -- wait-for
    , s :: Int -- with-seed
    } deriving Show

sample :: Parser CommandLineArgs
sample = CommandLineArgs
     <$> argument str
          ( help "is this node the master or slave?"
         <> metavar "[master|slave]" )
     <*> argument str
          ( help "hostname"
         <> metavar "STRING" )
     <*> argument str
          ( help "port"
         <> metavar "STRING" )
     <*> option auto
          ( long "send-for"
         <> short 'k'
         <> help "how many seconds does the system send messages"
         <> showDefault
         <> value (-1)
         <> metavar "INT" )
     <*> option auto
          ( long "wait-for"
         <> short 'l'
         <> help "the length of the grace period in seconds"
         <> showDefault
         <> value (-1)
         <> metavar "INT" )
     <*> option auto
          ( long "with-seed"
         <> short 's'
         <> help "which defines seed for RNGs"
         <> showDefault
         <> value (1337)
         <> metavar "INT" )
