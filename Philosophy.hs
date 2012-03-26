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
import Text.Printf
import Data.Time.Clock


-- Unblock exit
sigHandler mv = do putMVar mv True



showMaybe (Just a) = show a
showMaybe Nothing = "--Nothing--"

usage (_::SomeException) = do
    putStrLn "phil START_URL"
    exitWith $ ExitFailure 1

nextPage page depth stop found = do
    found' <- isFound stop page found
    case page of
        Nothing -> do
            putStrLn $ "***\tNo link to follow"
            putMVar stop True
        Just url -> do
            startTime <- getCurrentTime
            res <- (try $ simpleHTTP $ getRequest $ show url)
            endTime <- getCurrentTime
            case res of
                Left (SomeException e) -> do 
                    putStrLn $ "***\tFailed to load page : " ++ (showMaybe page)
                    putMVar stop True
                Right b -> do 
                    let d = (printf "%3d" depth :: String)
                    let t = printf "  %-10s  " (show $ diffUTCTime endTime startTime)
                    putStrLn $ d ++ " -- " ++ t ++
                               (showMaybe page)
                    body <- getResponseBody b
                    nextPage (topLink body) (depth + 1) stop found'


isFound stop page found = do 
    case S.member (show page) found of
        True -> do 
            putStrLn $ "***\tCycle detected returning to page: " ++ (showMaybe page)
            putMVar stop True
            return found
        False -> return $ S.insert (show page) found

-- Provide an absolute URL based on a URL base and relative path
-- If the relative path is actually a full URL, provides nothing
parseTag base t = u >>= (rel base) where
  rel a b = nonStrictRelativeTo b a
  u = (t >>= parseURI)  <|> (t >>= parseRelativeReference)


topLink body = top where
    tags = parseTags body
    tree = tagTree tags
    (TagBranch _ _ mainBody) = head $ mainBodyTree tree
    tag = (findFirstParagraph mainBody) >>= firstLink
    ref = case tag of 
        Just (TagBranch _ atts _ ) -> getRef atts
        Nothing -> Nothing
    top = parseTag (fromJust $ parseURI "http://en.wikipedia.org") ref
    dummy = parseURI "http://www.pharmash.com"


getVal key atts = case filter (\(k, _) -> k == key) atts of
    [] -> Nothing
    (v:_) -> Just $ snd v

getRef = getVal "href"



isPhilosphy uri = (show uri) == "http://en.wikipedia.org/wiki/Philosophy"
getPhilosophy = simpleHTTP $ getRequest "http://en.wikipedia.org/wiki/Philosophy" 
isMainBody = anyAttrValue ((==) "mw-content-text") --A wikipedia specificy ID attribute
mainBodyTree ts = [t | t@(TagBranch "div" a _) <- universeTree ts , isMainBody a]
treeParagraph ts = [t | t@(TagBranch "p" _ _) <- ts]
linkTrees ts = [t | t@(TagBranch "a" _ _) <- universeTree ts]
firstLinks ts = [t | t@(TagBranch "a" _ _) <- ts]

firstLink ts = case firstLinks ts of
    [] -> Nothing
    l:_ -> Just l

hasVisibleLink ts = case firstLink ts of
    Nothing -> False
    Just _  -> True


-- Searches top level branches only.  Does not descend.
findFirstParagraph [] = Nothing
findFirstParagraph ((TagBranch "p" _ t):ts) | hasVisibleLink t = Just t
                                            | otherwise = findFirstParagraph ts
findFirstParagraph (_:ts) = findFirstParagraph ts
    


testSetup = do
    phil <- getPhilosophy
    pBody <- getResponseBody phil
    let pTags = parseTags pBody
    let pTree = tagTree pTags
    let (TagBranch _ _ t) = head $ mainBodyTree pTree
    return t


main = do
    startPage <- handle usage $ do 
        startPage:_ <- getArgs
        return startPage


    -- Set up signal handler
    stopMv <- newEmptyMVar
    installHandler sigINT (Catch $ sigHandler stopMv) Nothing

    -- Kickoff the first thread with a task
    let uri = parseURI startPage
    let foundPages = S.empty
    forkIO $ nextPage uri (0::Int) stopMv foundPages

    -- Block until exit signal is received or threads are done
    takeMVar stopMv
    putStrLn "everything is super"
    exitWith ExitSuccess  --Threads not done?  Too bad.


