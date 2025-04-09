module Book (writeGreetingFile, howManyHandles) where

import Relude
import Prelude ()

import Control.Monad.Trans.Resource (ReleaseKey, ResourceT, allocate, runResourceT)
import qualified System.Directory as Dir
import System.FilePath ((</>))
import qualified System.IO as IO

import qualified Control.Exception.Safe as Ex
import Data.Maybe (fromJust)

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
