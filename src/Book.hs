{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Book (makeHTTPRequest) where

import Relude
import Prelude ()

import Control.Monad.Trans.Resource (ReleaseKey, ResourceT, allocate, runResourceT)
import qualified System.Directory as Dir
import System.FilePath ((</>))
import qualified System.IO as IO

import qualified Control.Exception.Safe as Ex
import Data.Maybe (fromJust)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import qualified Data.Char as Char
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T

import qualified Network.Socket as S
import qualified Network.Socket.ByteString as S

import Network.Simple.TCP (HostPreference (..), serve)
import qualified Network.Simple.TCP as Net

import qualified ASCII as A
import qualified ASCII.Char as A
import ASCII.Decimal (Digit (..))

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

copyGreetingFile = runResourceT @IO do
    dir <- liftIO getDataDir
    (_, h1) <- binaryFileResource (dir </> "greeting.txt") ReadMode
    (_, h2) <- binaryFileResource (dir </> "greeting2.txt") WriteMode
    liftIO $ repeatUntil (BS.hGetSome h1 1024) BS.null (BS.hPut h2)

binaryFileResource :: FilePath -> IOMode -> ResourceT IO (ReleaseKey, Handle)
binaryFileResource path mode = allocate (IO.openBinaryFile path mode) IO.hClose

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
characterCount :: FilePath -> IO Int
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

-- 3.6 Exercise 1
-- >>> greet $ fromString "李某 "
greet :: BS.ByteString -> IO ()
greet nameBS = case T.decodeUtf8' nameBS of
    Left _ -> putStrLn "Invalid byte string"
    Right nameText -> T.putStrLn (T.pack "Hello, " <> nameText)

-- 3.6 Exercise 2
asciiUpper :: BS.ByteString -> BS.ByteString
asciiUpper = BS.map f
  where
    f c
        | 97 <= c && c <= 122 = c - 32
        | otherwise = c

makeFriendSafely :: S.AddrInfo -> IO ()
makeFriendSafely addressInfo = runResourceT @IO do
    (_, s) <- allocate (S.openSocket addressInfo) S.close
    liftIO do
        S.connect s $ S.addrAddress addressInfo
        S.sendAll s $ T.encodeUtf8 $ T.pack "\r\n"
        -- Construct a simple HTTP GET request
        -- let request = T.encodeUtf8 $ T.pack "GET / HTTP/1.1\r\nHost: google.com\r\n\r\n"
        -- S.sendAll s request
        repeatUntil (S.recv s 1024) BS.null BS.putStr
        S.gracefulClose s 1000

findHaskellWebsite :: IO S.AddrInfo
findHaskellWebsite = do
    addrInfos <-
        S.getAddrInfo
            (Just S.defaultHints{S.addrSocketType = S.Stream})
            (Just "www.haskell.org")
            (Just "http")
    case addrInfos of
        [] -> fail "getAddrInfo returned []"
        x : _ -> return x

-- 4.5 Exercise 10
openAndConnect :: S.AddrInfo -> ResourceT IO (ReleaseKey, S.Socket)
openAndConnect addressInfo = do
    (releaseKey, s) <- allocate (S.openSocket addressInfo) S.close
    liftIO do
        -- S.setSocketOption s S.UserTimeout 1000
        S.connect s (S.addrAddress addressInfo)
    return (releaseKey, s)

-- 4.5 Exercise 11
findGopherWebsite :: IO ()
findGopherWebsite = do
    addrInfos <-
        S.getAddrInfo
            (Just S.defaultHints{S.addrSocketType = S.Stream})
            (Just "www.quux.org")
            (Just "gopher")
    case addrInfos of
        [] -> fail "getAddrInfo returned []"
        x : _ -> makeFriendSafely x

-- 4.5 Exercise 12
resolve :: S.ServiceName -> S.HostName -> IO S.AddrInfo
resolve sn hn = do
    addrInfos <-
        S.getAddrInfo
            (Just S.defaultHints{S.addrSocketType = S.Stream})
            (Just hn)
            (Just sn)
    case addrInfos of
        [] -> fail "getAddrInfo returned []"
        x : _ -> return x

crlf :: ByteString
crlf = A.charListToByteString [A.CarriageReturn, A.LineFeed]

line :: ByteString -> ByteString
line x = x <> crlf

helloRequestString :: ByteString
helloRequestString =
    line [A.string|GET /hello.txt HTTP/1.1|]
        <> line [A.string|User-Agent: curl/7.16.3|]
        <> line [A.string|Accept-Language: en, mi|]
        <> line [A.string||]

helloResponseString :: ByteString
helloResponseString =
    line [A.string|HTTP/1.1 200 OK|]
        <> line [A.string|Content-Type: text/plain; charset=us-ascii|]
        <> line [A.string|Content-Length: 6|]
        <> line [A.string||]
        <> [A.string|Hello!|]

ourFirstServer = serve @IO HostAny "8000" \(s, a) -> do
    putStrLn ("New connection from " <> show a)
    Net.send s helloResponseString

-- 5.6 Exercise 13
repeatUntilNothing :: (Monad m) => m (Maybe chunk) -> (chunk -> m x) -> m ()
repeatUntilNothing m action = repeatUntil m isNothing (action . fromJust)

-- 5.6 Exercise 14
makeHTTPRequest :: Net.ServiceName -> Net.HostName -> IO ()
makeHTTPRequest sn hn = runResourceT @IO do
    addrInfo <- lift $ resolve sn hn
    (_, s) <- openAndConnect addrInfo
    Net.send s request
    lift $ repeatUntilNothing (Net.recv s 1024) BS.putStr
    lift $ S.gracefulClose s 1000
  where
    request = helloRequestString

data Request = Request RequestLine [HeaderField] (Maybe MessageBody)
data Response = Response StatusLine [HeaderField] (Maybe MessageBody)

data RequestLine = RequestLine Method RequestTarget HttpVersion
data StatusLine = StatusLine HttpVersion StatusCode ReasonPhrase

data HeaderField = HeaderField FieldName FieldValue
newtype FieldName = FieldName BS.ByteString
newtype FieldValue = FieldValue BS.ByteString

newtype MessageBody = MessageBody LBS.ByteString

data HttpVersion = HttpVersion A.Digit A.Digit

newtype RequestTarget = RequestTarget BS.ByteString
newtype Method = Method BS.ByteString

data StatusCode = StatusCode A.Digit A.Digit A.Digit
newtype ReasonPhrase = ReasonPhrase BS.ByteString

-- 6.7 Exercise 16
helloRequest :: Request
helloRequest = Request start [host, lang] Nothing
  where
    start =
        RequestLine
            (Method [A.string|GET|])
            ( RequestTarget
                [A.string|./hello.txt|]
            )
            (HttpVersion Digit1 Digit1)
    host =
        HeaderField
            ( FieldName [A.string|User-Agent|]
            )
            (FieldValue [A.string|curl/7.16.3|])
    lang = HeaderField (FieldName [A.string|Accept-Language|]) (FieldValue [A.string|en, mi|])

helloResponse :: Response
helloResponse = Response (StatusLine (HttpVersion Digit1 Digit1) (StatusCode Digit2 Digit0 Digit0) (ReasonPhrase [A.string|OK|])) [HeaderField (FieldName [A.string|Content-Type|]) (FieldValue [A.string|text/plain; charset=us-ascii|]), HeaderField (FieldName [A.string|Content-Length|]) (FieldValue [A.string|6|])] (Just (MessageBody [A.string|Hello!|]))
