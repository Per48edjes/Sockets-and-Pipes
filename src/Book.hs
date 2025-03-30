module Book (writeGreetingFile) where

import Relude
import Prelude ()

import Control.Monad.Trans.Resource (allocate, runResourceT)
import qualified System.Directory as Dir
import System.FilePath ((</>))
import qualified System.IO as IO

getDataDir :: IO FilePath
getDataDir = do
    dir <- Dir.getXdgDirectory Dir.XdgData "sockets-and-pipes"
    Dir.createDirectoryIfMissing True dir
    return dir

writeGreetingFile :: IO ()
writeGreetingFile = runResourceT @IO do
    dir <- liftIO getDataDir
    (_releaseKey, h) <- allocate (IO.openFile (dir </> "greeting.txt") WriteMode) IO.hClose
    liftIO $ IO.hPutStrLn h "hello"
    liftIO $ IO.hPutStrLn h "world"
