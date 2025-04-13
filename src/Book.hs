module Book (writeGreetingFile, howManyHandles, printFileContentsUpperCase) where

import Relude
import Prelude ()

import Control.Monad.Trans.Resource (ReleaseKey, ResourceT, allocate, runResourceT)
import qualified System.Directory as Dir
import System.FilePath ((</>))
import qualified System.IO as IO

import qualified Control.Exception.Safe as Ex
import Data.Maybe (fromJust)

import qualified Data.Char as Char
import qualified Data.Text as T
import qualified Data.Text.IO as T

getDataDir :: IO FilePath
getDataDir = do
    dir <- Dir.getXdgDirectory Dir.XdgData "sockets-and-pipes"
    Dir.createDirectoryIfMissing True dir
    return dir

writeGreetingFile :: IO ()
writeGreetingFile = runResourceT @IO do
    dir <- liftIO getDataDir
    (_releaseKey, h) <- fileResource (dir </> "greeting.txt") WriteMode
    liftIO $ handlePrintTest h
    liftIO $ IO.hPutStrLn h "hello"
    liftIO $ IO.hPutStrLn h "world"

-- 1.5 Exercise 1
fileResource :: FilePath -> IOMode -> ResourceT IO (ReleaseKey, Handle)
fileResource filePath mode = allocate (IO.openFile filePath mode) IO.hClose

-- 1.5 Exercise 2
handlePrintTest :: Handle -> IO ()
handlePrintTest h = runResourceT @IO do
    s <- liftIO $ IO.hShow h
    putStrLn s

-- 1.5 Exercise 3
howManyHandles :: IO ()
howManyHandles = runResourceT @IO do
    hs <- openManyHandles
    putStrLn ("Opened " <> show (length hs) <> " handles")

fileResourceMaybe :: ResourceT IO (Maybe Handle)
fileResourceMaybe = do
    dir <- liftIO getDataDir
    result <- Ex.tryIO do
        liftIO (IO.openFile (dir </> "greeting.txt") ReadMode)
    case result of
        Right x -> return $ Just x
        Left e -> do
            print (displayException e)
            return Nothing

openManyHandles :: ResourceT IO [Handle]
openManyHandles = do
    result <- fileResourceMaybe
    if isNothing result
        then return []
        else do (fromJust result :) <$> openManyHandles

helloText :: IO ()
helloText = T.hPutStrLn stdout (T.pack "hello world!")

helloTextFile :: IO ()
helloTextFile = runResourceT @IO do
    dir <- liftIO getDataDir
    (_, h) <- fileResource (dir </> "hello.txt") WriteMode
    liftIO do
        T.hPutStrLn h (T.pack "hello")
        T.hPutStrLn h (T.pack "world")

printFileContentsUpperCase :: IO ()
printFileContentsUpperCase = runResourceT @IO do
    dir <- liftIO getDataDir
    (_, h) <- fileResource (dir </> "greeting.txt") ReadMode
    liftIO $ repeatUntilIO (T.hGetChunk h) T.null (T.putStrLn . T.toUpper)

-- Function to repeatedly perform IO action until condition is met
repeatUntilIO :: IO c -> (c -> Bool) -> (c -> IO a) -> IO ()
repeatUntilIO = repeatUntil

-- 2.3 Exercise 4
digitsOnly :: Text -> Text
digitsOnly = T.filter Char.isDigit

-- 2.3 Exercise 5
capitalizeLast :: Text -> Text
-- capitalizeLast = T.cons <$> (Char.toUpper . T.head) <*> T.tail
capitalizeLast = do
    firstChar <- Char.toUpper . T.head
    rest <- T.tail
    return $ firstChar `T.cons` rest

-- >>> unParen (T.pack "foo )h(el)lo)( world")
-- Just "el)lo"
unParen :: Text -> Maybe Text
unParen t = do
    let t' = T.dropWhileEnd (/= ')') . T.dropWhile (/= '(') $ t
    (_, t'') <- T.uncons t'
    (t''', _) <- T.unsnoc t''
    return t'''

-- 2.3 Exercise 5
-- >>> characterCount "greeting.txt"
characterCount fp = runResourceT @IO do
    dir <- liftIO getDataDir
    (_, h) <- fileResource (dir </> fp) ReadMode
    liftIO $ go h 0
  where
    go :: Handle -> Int -> IO Int
    go h acc = do
        chunk <- T.hGetChunk h
        if T.null chunk
            then return acc
            else go h (acc + T.length chunk)

repeatUntil :: (Monad m) => m c -> (c -> Bool) -> (c -> m a) -> m ()
repeatUntil m condition action = do
    chunk <- m
    unless (condition chunk) (action chunk >> repeatUntil m condition action)
