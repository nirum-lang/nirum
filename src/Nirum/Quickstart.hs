{-# LANGUAGE ExtendedDefaultRules, TypeFamilies #-}
module Nirum.Quickstart (main) where

import qualified System.Exit as SE
import System.IO ( BufferMode (NoBuffering)
                 , hSetBuffering
                 , stdout
                 )

import qualified Control.Exception as E
import qualified Data.Map.Strict as MS
import qualified Data.Maybe as M
import Data.Monoid ((<>))
import qualified Data.SemVer as SV
import qualified Data.Text as T
import qualified Data.Text.Encoding as ENC
import qualified Data.Text.IO as TIO
import qualified System.Process as P
import qualified Text.Email.Validate as EV

import Nirum.Package.Metadata ( Author ( Author
                                       , email
                                       , name
                                       , uri
                                       )
                              , Package
                              , Target ( CompileError
                                       , CompileResult
                                       , compilePackage
                                       , parseTarget
                                       , showCompileError
                                       , targetName
                                       , toByteString
                                       )
                              , Metadata ( Metadata
                                          , authors
                                          , description
                                          , keywords
                                          , license
                                          , target
                                          , version
                                          )
                              )
import qualified Nirum.Package.Metadata.Writer as MW

-- Dummy target to create Metadata
data Quickstart = Quickstart deriving (Eq, Ord, Show)

compilePackage' :: Package Quickstart -> MS.Map FilePath (Either () ())
compilePackage' _ = MS.empty

instance Target Quickstart where
    type CompileResult Quickstart = ()
    type CompileError Quickstart = ()
    targetName _ = "quickstart"
    parseTarget _ = return Quickstart
    compilePackage = compilePackage'
    showCompileError _ = const ""
    toByteString _ = const ""

defaultVersion :: T.Text
defaultVersion = "0.0.1"

readConfig :: T.Text -> IO (Maybe T.Text)
readConfig cfg = do
    let proc = (P.proc "git" ["config", T.unpack cfg]) {
                   P.std_out = P.CreatePipe
               }
        catchFn _ = return Nothing
    stream <- (`E.catch` (catchFn :: E.IOException -> IO (Maybe a))) $ do
        (_, s, _, _) <- P.createProcess proc
        return s
    case stream of
        Just s -> do
            c <- TIO.hGetLine s
            return $ Just c
        Nothing -> return Nothing

inputConfig :: T.Text -> Maybe T.Text -> IO T.Text
inputConfig prompt defaultValue = do
    let emptyValue = M.fromMaybe T.empty defaultValue
    TIO.putStr $ prompt <> maybe "" (\ n -> "(" <> n <> ") ") defaultValue
    configValue <- TIO.getLine
    return $ if T.null configValue then emptyValue else configValue

parseSemVer :: T.Text -> IO SV.Version
parseSemVer v =
    case SV.fromText v of
        Right r -> return r
        Left _ -> SE.die $ T.unpack $ completeMessage v
  where
    errorMessage :: T.Text
    errorMessage = "version expected to be fromatted as Semantic Versioning: "
    completeMessage :: T.Text -> T.Text
    completeMessage e' = errorMessage <> e'

parseAuthor :: T.Text -> T.Text -> Author
parseAuthor name' email' =
    Author { name = name'
           , email = case EV.validate (ENC.encodeUtf8 email') of
                         Right e -> Just e
                         Left _ -> Nothing
           , uri = Nothing
           }

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    semVer <- inputConfig "verison: " $ Just defaultVersion
    gitName <- readConfig "user.name"
    name' <- inputConfig "author.name: " gitName
    gitEmail <- readConfig "user.email"
    email' <- inputConfig "author.email: " gitEmail
    description' <- inputConfig "description: " $ Just ""
    license' <- inputConfig "license: " $ Nothing
    version' <- parseSemVer semVer
    let author = parseAuthor name' email'
        text = MW.encode Metadata { target = Quickstart
                                  , authors = [author]
                                  , version = version'
                                  , description = textToMaybe description'
                                  , license = textToMaybe license'
                                  , keywords = []
                                  }
    writeFile "package.toml" $ T.unpack text
  where
    textToMaybe :: T.Text -> Maybe T.Text
    textToMaybe t = if T.null t then Nothing else Just t
