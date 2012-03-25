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
import Text.HTML.TagSoup.Tree
import Text.HTML.TagSoup.Match


-- Unblock exit
sigHandler mv = do putMVar mv True


usage (_::SomeException) = do
    putStrLn "phil START_URL"
    exitWith $ ExitFailure 1

nextPage page depth stop  = do
    res <- (try $ simpleHTTP $ getRequest $ show page)
    case res of
        Left (SomeException e) -> do 
            putStrLn $ "Failed to load page : " ++ (show page)
            putMVar stop True
        Right b -> do 
            putStrLn $ (show depth) ++ " -- " ++
                       (show page)
            body <- getResponseBody b
            nextPage (topLink body) (depth + 1) stop


topLink body = uri where
    tags = parseTags body
    tree = tagTree tags
    Just uri = parseURI "http://www.pharmash.com"


isPhilosphy uri = (show uri) == "http://en.wikipedia.org/wiki/Philosophy"
getPhilosophy = simpleHTTP $ getRequest "http://en.wikipedia.org/wiki/Philosophy" 
isMainBody = anyAttrValue ((==) "mw-content-text") --A wikipedia specificy ID attribute
mainBodyTree ts = [t | t@(TagBranch "div" a _) <- universeTree ts , isMainBody a]
treeParagraph ts = [t | t@(TagBranch "p" _ _) <- ts]
firstLink ts = [t | t@(TagBranch "a" _ _) <- universeTree ts]




main = do
    startPage <- handle usage $ do 
        startPage:_ <- getArgs
        return startPage


    -- Set up signal handler
    stopMv <- newEmptyMVar
    installHandler sigINT (Catch $ sigHandler stopMv) Nothing

    -- Kickoff the first thread with a task
    case parseURI startPage of
        Nothing ->  return ()
        Just uri -> do forkIO $ nextPage uri 0 stopMv
                       return ()

    -- Block until exit signal is received or threads are done
    takeMVar stopMv
    putStrLn "everything is super"
    exitWith ExitSuccess  --Threads not done?  Too bad.


