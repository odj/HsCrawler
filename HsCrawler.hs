{-# LANGUAGE ScopedTypeVariables #-}
import Prelude hiding (catch)
import qualified Data.Set as S
import Data.Maybe
import System.Environment (getArgs)
import System.Posix.Signals
import System.IO
import System.Exit
import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Control.Exception
import Network.HTTP
import Network.Browser (uriDefaultTo)
import Network.URI
import Text.HTML.TagSoup


data CrawlerRequest = CrawlerRequest
  {
    maxDepth :: Integer
  , depth    :: Integer
  , uri      :: URI
  } deriving (Eq)


instance Show CrawlerRequest where
    show (CrawlerRequest mx d u)  = "(" ++ (show d)
                                        ++ "/" ++ (show mx)
                                        ++ ")" ++ " -- "
                                        ++ (show u)

httpGet u = do
    res <- (try $ simpleHTTP $ getRequest u)
    body <- case res of
        Left (SomeException e) -> do return Nothing  -- No soup for you!
        Right b -> do liftM Just $ getResponseBody b
    return body


pageLinks ts =  S.toList attributes where
    attributes = foldr getHrefs S.empty $ concat $ map getAtts ts
    getAtts (TagOpen s atts) = atts
    getAtts _ = []
    getHrefs (att, val) acc = case att of
            "href" -> S.insert val acc
            _      -> acc 
                             


-- Basic crawler
crawler ch = do
    c@(CrawlerRequest maxD d page) <- readChan ch
    id <- myThreadId
    case (d < maxD) of
        True -> do
            pageText <- httpGet (show page)
            case pageText of
                Nothing -> crawler ch
                Just s -> taskWorkers ch c s
            putStrLn $ show id ++ " -- " ++ show c
            crawler ch
        False -> do 
            putStrLn $ show id ++ " -- " ++ show c ++ " -- Dead Link"
            crawler ch


taskWorkers ch base page = do
    let rLinks = catMaybes $ map parseURI $ pageLinks $ parseTags page
        allLinks = map (\u -> uriDefaultTo u (uri base)) rLinks
        domainLinks = allLinks-- filter (\l -> ((==) (auth l) $  auth $ uri base)) allLinks
        depth' = 1 + depth base
        auth = liftM uriRegName . uriAuthority
        reqs = map (\u -> base {uri = u, depth = depth'}) domainLinks
    mapM_ (writeChan ch) reqs
    putStrLn $ "\t--> Number of links: " ++ (show $ length reqs)


-- Thread to monitor other threads, write to disk, etc.
idle mv = do
    threadDelay 1000000 --Microseconds
    -- Do stuff here, perhaps?
    idle mv


-- Unblock exit
sigHandler mv = do putMVar mv True


readOrDie v = do
    let maybeRead = fmap fst . listToMaybe . reads
    case maybeRead v of
        Nothing -> usage
        Just v -> return v


usage = do
    putStrLn "crawl NUM_THREADS  MAX_DEPTH  START_URL"
    exitWith $ ExitFailure 1


main = do
    th:mx:startPage:_ <- getArgs
    threads <- readOrDie th
    maxD <- readOrDie mx

    -- Set up signal handler
    stopMv <- newEmptyMVar
    installHandler sigINT (Catch $ sigHandler stopMv) Nothing

    -- Launch the worker threads
    ch <- newChan
    replicateM_ threads $ forkIO $ crawler ch
    forkIO $ idle stopMv

    -- Kickoff the first thread with a task
    case parseURI startPage of
        Nothing -> usage
        Just uri -> writeChan ch $ CrawlerRequest (fromIntegral maxD) 0 uri

    -- Block until exit signal is received or threads are done
    takeMVar stopMv
    putStrLn "everything is super"
    exitWith ExitSuccess  --Threads not done?  Too bad.


