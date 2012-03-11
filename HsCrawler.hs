import System.Environment (getArgs)
import System.Posix.Signals
import System.IO
import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.Chan



data CrawlerRequest = CrawlerRequest
  {
    maxDepth :: Integer
  , depth    :: Integer
  , url      :: String
  } deriving (Ord, Eq)

instance Show CrawlerRequest where
    show (CrawlerRequest mx d u)  = "(" ++ (show mx)
                                        ++ "/" ++ (show d)
                                        ++ ")" ++ " -- "
                                        ++ u


-- Basic crawler
crawler ch = do
    c@(CrawlerRequest maxD d page) <- readChan ch
    id <- myThreadId
    putStrLn $ show c ++ " -- " ++ show id
    case (d < maxD) of
        True -> do
            writeChan ch $ CrawlerRequest maxD (d+1) page
            crawler ch
        False -> do crawler ch


-- Thread to monitor other threads, write to disk, etc.
idle mv = do
    threadDelay 1000000 --Microseconds
    -- Do stuff here, perhaps?
    idle mv


-- Unblock exit
sigHandler mv = do putMVar mv True


main = do
    th:mx:startPage:_ <- getArgs
    let threads = read th :: Int
        maxD = read mx :: Integer

    -- Set up signal handler
    stopMv <- newEmptyMVar
    installHandler sigINT (Catch $ sigHandler stopMv) Nothing

    -- Launch the worker threads
    ch <- newChan
    replicateM_ threads $ forkIO $ crawler ch
    forkIO $ idle stopMv


    -- Kickoff the first thread
    writeChan ch $ CrawlerRequest maxD 0 startPage

    -- Block until signal is received or threads are done
    takeMVar stopMv
    putStrLn "everything is super"



