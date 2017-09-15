{-# LANGUAGE OverloadedStrings #-}
module Nirum.Package.Metadata.Writer ( encode
                                     , metadataToToml
                                     , renderTable
                                     ) where

import qualified Data.HashMap.Lazy as HM
import qualified Data.SemVer as SV
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Maybe as M
import qualified Data.Monoid as MO
import qualified Data.Text as T
import qualified Data.Text.Encoding as ENC
import qualified Data.Time.Clock as TC
import qualified Data.Vector as V
import qualified Text.Email.Validate as EV
import qualified Text.Toml.Types as TM

import qualified Nirum.Package.Metadata as M

renderTable :: HM.HashMap T.Text TM.Node -> T.Text
renderTable t = T.intercalate "\n" rendered
  where
    rendered :: [T.Text]
    rendered = [ renderKey k n MO.<> renderNode n
               | (k, n) <- L.sortOn nodePriority $ HM.toList t
               ]
    renderKey :: T.Text -> TM.Node -> T.Text
    renderKey k (TM.VTable _) = "\n[" MO.<> k MO.<> "]\n"
    renderKey k (TM.VTArray _) = "\n[[" MO.<> k MO.<> "]]\n"
    renderKey k _ = k MO.<> " = "
    nodePriority :: (T.Text, TM.Node) -> Int
    nodePriority (_, TM.VTable _) = 1
    nodePriority (_, TM.VTArray _) = 2
    nodePriority (_, _) = 0

renderText :: T.Text -> T.Text
renderText t = "\"" MO.<> t MO.<> "\""

renderTableArray :: V.Vector TM.Table -> T.Text
renderTableArray v = T.intercalate "\n" [ renderTable t | t <- V.toList v ]

-- TODO
renderInteger :: I.Int64 -> T.Text
renderInteger _ = ""

-- TODO
renderFloat :: Double -> T.Text
renderFloat _ = ""

-- TODO
renderBoolean :: Bool -> T.Text
renderBoolean _ = ""

-- TODO
renderDatetime :: TC.UTCTime -> T.Text
renderDatetime _ = ""

-- TODO
renderArray :: V.Vector TM.Node -> T.Text
renderArray _ = ""

renderNode :: TM.Node -> T.Text
renderNode (TM.VString s) = renderText s
renderNode (TM.VTable t) = renderTable t
renderNode (TM.VTArray vta) = renderTableArray vta
renderNode (TM.VInteger i) = renderInteger i
renderNode (TM.VFloat f) = renderFloat f
renderNode (TM.VBoolean b) = renderBoolean b
renderNode (TM.VDatetime dt) = renderDatetime dt
renderNode (TM.VArray a) = renderArray a

encode :: M.Target t => M.Metadata t -> T.Text
encode metadata = renderTable $ metadataToToml metadata

metadataToToml :: M.Target t => M.Metadata t -> TM.Table
metadataToToml M.Metadata { M.version = version'
                          , M.authors = authors'
                          , M.description = description'
                          , M.license = license'
                          } =
    HM.fromList [(name, value) | (name, Just value) <- fields]
  where
    authors'' :: [TM.Table]
    authors'' = [ HM.fromList [ ("name", TM.VString name')
                              , ( "email"
                                , TM.VString $ maybe "" toEmailText email'
                                )
                              ]
                | M.Author { M.name = name', M.email = email' } <- authors'
                ]
      where
        toEmailText :: EV.EmailAddress -> T.Text
        toEmailText e = ENC.decodeUtf8 $ EV.toByteString e
    fields :: [(T.Text, Maybe TM.Node)]
    fields =
        [ ("authors", Just $ TM.VTArray $ V.fromList authors'')
        , ("version", Just $ TM.VString $ SV.toText version')
        , ("description", Just $ TM.VString $ M.fromMaybe "" description')
        , ("license", fmap TM.VString $ license')
        ]
