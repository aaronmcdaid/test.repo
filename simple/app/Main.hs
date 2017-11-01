import Control.Concurrent (threadDelay)
import Control.Monad (forever, when)
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Network.Transport.TCP (createTransport, defaultTCPParameters)

-- next two for options processing, https://stackoverflow.com/a/39049486/146041
import Options.Applicative
import Data.Monoid ((<>))

import System.Exit(exitWith, ExitCode(ExitFailure)) -- if args are missing

{-
 - Program should accept the two command line arguments: --send-for k, which
 - denotes how many seconds does the system send messages, and --wait-for l,
 - which denotes the length of the grace period in seconds. s, k, l ∈ N. We also
 - suggest providing --with-seed s argument, which defines seed for RNGs.
 -}
data CommandLineArgs = CommandLineArgs
    { k :: Int -- send-for
    , l :: Int -- wait-for
    , s :: Int -- with-seed
    } deriving Show

sample :: Parser CommandLineArgs
sample = CommandLineArgs
     <$> option auto
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
replyBack :: (ProcessId, String) -> Process ()
replyBack (sender, msg) = send sender ("<" ++ msg ++ ">")

logMessage :: String -> Process ()
logMessage msg = say $ "handling " ++ msg

main :: IO ()
main = do
  let opts = info (helper <*> sample)
          ( fullDesc
         <> progDesc " -k INT and -l INT are necessary. -s INT defaults to 1337"
         <> header "???" )
  args_kls <- execParser opts
  let arg_k = k args_kls
  let arg_l = l args_kls
  let arg_s = s args_kls
  when  (   arg_k == -1
         || arg_l == -1
      ) $ do
            putStrLn "\nUsage: simple-exe -k|--send-for INT -l|--wait-for INT [-s|--with-seed INT]"
            exitWith (ExitFailure 1)
  print args_kls
  Right t <- createTransport "127.0.0.1" "10501" defaultTCPParameters
  node <- newLocalNode t initRemoteTable
  runProcess node $ do
    -- Spawn another worker on the local node
    echoPid <- spawnLocal $ forever $ do
      -- Test our matches in order against each message in the queue
      receiveWait [match logMessage, match replyBack]

    -- The `say` function sends a message to a process registered as "logger".
    -- By default, this process simply loops through its mailbox and sends
    -- any received log message strings it finds to stderr.

    say "send some messages!"
    send echoPid "hello"
    send echoPid "Hello"
    self <- getSelfPid
    send echoPid (self, "hello")
    send echoPid (self, "Hello")
    send echoPid (self, "Hello")

    -- `expectTimeout` waits for a message or times out after "delay"
    forever $ do
        m <- expectTimeout 1000000 -- one second == 1million ms
        case m of
          -- Die immediately - throws a ProcessExitException with the given reason.
          Nothing  -> die "nothing came back!"
          Just s -> say $ "got " ++ s ++ " back!"

    -- Without the following delay, the process sometimes exits before the messages are exchanged.
    liftIO $ threadDelay 2000000
