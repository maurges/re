{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedLists #-}

module Main where

import qualified Data.Map.Strict as Map
import qualified Network.Wai.Handler.Warp as Warp

import Data.Function ((&))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Ron.Class (FromRon, ToRon)
import Data.Ron.Deserialize (decodeFile)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Network.HTTP.Types.Status (seeOther303, status404)
import Network.Wai (Application, rawPathInfo, responseBuilder, responseLBS)
import Network.Wai.Handler.Warp (runSettings)
import System.Directory (XdgDirectory(XdgConfig), getXdgDirectory, makeAbsolute)
import System.Environment (getArgs, lookupEnv)
import System.FSNotify (Event(Added, Modified), startManager, watchDir)
import System.FilePath ((</>), splitFileName)

data Settings = Settings
    { port :: !Int
    , redirectsPath :: !FilePath
    } deriving (Eq, Show)

envSettings :: IO Settings
envSettings = do
    redirectsPathDef <- getXdgDirectory XdgConfig "re-server/redirects.ron"
    let portDef = 8040
    port <- fromMaybe portDef . fmap read <$> lookupEnv "PORT"
    redirectsPath <- fromMaybe redirectsPathDef <$> lookupEnv "REDIRECTS_FILE"
    pure Settings {port, redirectsPath}

parseCommandLine :: [String] -> Settings -> Settings
parseCommandLine = go where
    go [] !x = x
    go ("--port" : port : rest) !x =
        go rest $! x {port = read port}
    go ( ('-':'-':'p':'o':'r':'t':'=':port) : rest ) !x =
        go rest $! x {port = read port}
    go ("--config" : path : rest) !x =
        go rest $! x {redirectsPath = path}
    go ( ('-':'-':'c':'o':'n':'f':'i':'g':'=':path) : rest ) !x =
        go rest $! x {redirectsPath = path}
    go (_noMatch : rest) !x = go rest x

newtype Redirects = Redirects (Map Text Text)
    deriving (Eq, Show)
    deriving (ToRon, FromRon) via (Map Text Text)

app :: IORef Redirects -> Application
app redirects req respond = do
    putStrLn ("Request: " <> show (rawPathInfo req))
    Redirects redirMap <- readIORef redirects
    let path = decodeUtf8 $ rawPathInfo req
    case Map.lookup path redirMap of
        Just redir ->
            let redir' = encodeUtf8 redir
            in respond $ responseBuilder seeOther303 [("Location", redir')] mempty
        Nothing -> respond $ responseLBS status404 [] "Not Found"

main :: IO ()
main = do
    appSettings@Settings {port, redirectsPath} <-
        parseCommandLine <$> getArgs <*> envSettings
    putStrLn $ "App settings: " <> show appSettings

    redirects <- decodeFile redirectsPath
    let (dir', file) = splitFileName redirectsPath
    dir <- makeAbsolute dir'
    redirectsRef <- newIORef $ Redirects redirects
    let reloadRedirects = do
            content <- Redirects <$> decodeFile redirectsPath
            writeIORef redirectsRef content
            putStrLn "Reloaded redirects"

    fsManager <- startManager
    _stop <- watchDir fsManager dir (const True) $ \case
        Added path _ _ | path == file || path == dir </> file -> reloadRedirects
        Modified path _ _ | path == file || path == dir </> file -> reloadRedirects
        _ -> pure ()

    let settings = Warp.defaultSettings
            & Warp.setPort port
            & Warp.setHost "127.0.0.1"

    putStrLn "Server starting..."
    runSettings settings $ app redirectsRef
