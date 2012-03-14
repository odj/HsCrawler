{-# LANGUAGE ScopedTypeVariables #-}
import Prelude hiding (catch)
import qualified Data.Set as S
import Data.Maybe
import System.Environment (getArgs)
import System.Posix.Signals
import System.IO
import System.Exit
import Control.Monad
import Control.Applicative
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
  , found    :: MVar (S.Set String)
  } deriving (Eq)


instance Show CrawlerRequest where
    show (CrawlerRequest mx d u found)  = "(" ++ (show d)
                                        ++ "/" ++ (show mx)
                                        ++ ")" ++ " -- "
                                        ++ (show u)


httpGet u = do
    res <- (try $ simpleHTTP $ getRequest u)
    body <- case res of
        Left (SomeException e) -> do 
            return Nothing  -- No soup for you!
        Right b -> do liftM Just $ getResponseBody b
    return body


pageLinks ts =  S.toList attributes where
    attributes = foldr getHrefs S.empty $ concat $ map getAtts ts
    getAtts t = case t of
        (TagOpen s ats) -> ats
        _               -> []
    getHrefs (att, val) acc = case att of
            "href" -> S.insert val acc
            _      -> acc 
                             
reportFound found page= do
    s <- takeMVar found
    let s' = S.insert (show page) s
    putMVar found s'
    putStrLn $ "\t--> Total Found: " ++ (show $ S.size s')


isVisited found page = do
    s <- takeMVar found
    putMVar found s
    let visited = S.member (show page) s
    return visited
    

-- Basic crawler
crawler ch = do
    c@(CrawlerRequest maxD d page found) <- readChan ch
    id <- myThreadId
    visited <- isVisited found page
    case (visited) of
        True  -> return ()
        False ->  case (d < maxD) of
            True -> do
                pageText <- httpGet (show page)
                case pageText of
                    Nothing -> do putStrLn $ show id ++ " -- " ++ show c ++ " -- Dead link"
                    Just s -> do reportFound found page
                                 taskWorkers ch c s
                                 putStrLn $ show id ++ " -- " ++ show c
            False -> return ()
    crawler ch


Just pharmash = parseURI "http://www.pharmash.com"

parseTag base t = u >>= (rel base) where
  rel a b = nonStrictRelativeTo b a
  u = parseURI t <|> parseRelativeReference t


taskWorkers :: (Chan CrawlerRequest) -> CrawlerRequest -> String -> IO ()
taskWorkers ch base page = do
    let rLinks = catMaybes $ map (parseTag $ uri base) $ pageLinks $ parseTags page
        domainLinks = filter (\l -> ((==) (auth l) $  auth $ uri base)) rLinks
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
    outputSet <- newEmptyMVar
    putMVar outputSet S.empty
    replicateM_ threads $ forkIO $ crawler ch
    forkIO $ idle stopMv

    -- Kickoff the first thread with a task
    case parseURI startPage of
        Nothing -> usage
        Just uri -> writeChan ch $ CrawlerRequest (fromIntegral maxD) 0 uri outputSet

    -- Block until exit signal is received or threads are done
    takeMVar stopMv
    putStrLn "everything is super"
    grandTotal <- takeMVar outputSet
    putStrLn $ "#### TOTAL FOUND: " ++ (show $ S.size grandTotal) ++ "\n\n"
    putStrLn $ show grandTotal
    exitWith ExitSuccess  --Threads not done?  Too bad.


