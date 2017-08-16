{-# LANGUAGE ExtendedDefaultRules, OverloadedStrings, QuasiQuotes #-}
module Nirum.Quickstart (main) where

import System.IO ( BufferMode (NoBuffering)
                 , hSetBuffering
                 , stdout
                 )

import Data.Monoid ((<>))
import qualified Data.Text as T

import qualified Data.Text.IO as TIO
import qualified System.Process as P
import qualified Control.Exception as E

defaultVersion :: T.Text
defaultVersion = "0.0.1"

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    TIO.putStr $ "version: (" <> defaultVersion <> ") "
    inputVersionText <- TIO.getLine
    let versionText = if T.null inputVersionText
        then defaultVersion
        else inputVersionText
    TIO.putStrLn versionText

    gitName <- readConfig "user.name"
    name <- inputConfig "author.name: " gitName
    TIO.putStrLn name

    gitEmail <- readConfig "user.email"
    email <- inputConfig "author.email: " gitEmail
    TIO.putStrLn email

readConfig :: T.Text -> IO (Maybe T.Text)
readConfig cfg = do
    let proc = (P.proc "git" ["config", T.unpack cfg]) { P.std_out = P.CreatePipe }
    stream <- E.catch (do (_, s, _, _) <- P.createProcess proc
                          return s)
                      ((\_ -> return Nothing) :: E.IOException -> IO (Maybe a))
    case stream of
        Just s -> do
            c <- TIO.hGetLine s
            return $ Just c
        Nothing -> return Nothing

inputConfig :: T.Text -> Maybe T.Text -> IO T.Text
inputConfig prompt gitName = do
    let name = maybe T.empty (\n -> n) gitName
    TIO.putStr $ prompt <>  maybe "" (\n -> "(" <> n <> ") ") gitName
    authorName <- TIO.getLine
    return $ if T.null authorName then name else authorName
