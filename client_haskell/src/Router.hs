{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}

module Router where
import           Protolude
import qualified Web.Scotty.Trans as S
import qualified Data.Text.Lazy as Tl
import           Control.Monad.IO.Unlift
import qualified Network.Wai.Middleware.StaticEmbedded as Wm
import qualified Data.FileEmbed as Fe
import qualified Data.Map.Strict as Map
import qualified Data.Aeson as Ae

import qualified Env
import qualified Html
import qualified Graph
import qualified Gremlin as Gr
import qualified Types.Cytoscape as C
import qualified Adapters.Simple as A

newtype RouterCmp m = RouterCmp
  { rtRun :: m ()
  }

newRouterCmp :: MonadUnliftIO m => Env.EnvCmp m -> Gr.GremlinCmp m -> Html.HtmlCmp -> RouterCmp m
newRouterCmp env grm html = RouterCmp
  { rtRun = runRouter env grm html
  }


runRouter :: MonadUnliftIO m => Env.EnvCmp m -> Gr.GremlinCmp m -> Html.HtmlCmp -> m ()
runRouter env@Env.EnvCmp{..} grm html = do
  ev <- evGet
  let
    router = do
      S.middleware $ Wm.static $(Fe.embedDir "./static/")
      newRouter env grm html
  withRunInIO $ \fUnliftIO -> do
    S.scottyT (Env.envPort ev) fUnliftIO router

newRouter :: MonadIO m => Env.EnvCmp m -> Gr.GremlinCmp m -> Html.HtmlCmp -> S.ScottyT Tl.Text m ()
newRouter env@Env.EnvCmp{..} Gr.GremlinCmp{..} Html.HtmlCmp{..} = do
  S.get "/" $ do
    let
      content = Html.queryForm
      results
          = Html.queryRaw
          <> Html.queryGraph
          <> Html.queryTable
      scripts = Html.queryJs
      html = htmlBase (Just content) (Just results) (Just scripts)
    S.html html

  S.post "/" $ do
    g <- S.param "query"
    (raw, res) <- lift $ grSubmit A.adapter g
    case res of
      Just (tableData, cyto) -> S.json $ ResponseJson (GraphJson cyto tableData) raw
      Nothing -> S.json (Map.fromList [("raw" :: Text, raw)])

data GraphJson = GraphJson
  { graphData :: !C.CytoElements
  , tableData :: ![C.TableDatum]
  } deriving (Eq, Show, Generic)

instance Ae.ToJSON GraphJson where
  toJSON = Ae.genericToJSON Ae.defaultOptions { Ae.fieldLabelModifier = Ae.camelTo2 '-' }

data ResponseJson = ResponseJson
  { cyto :: !GraphJson
  , raw :: !Text
  } deriving (Eq, Show, Generic)

instance Ae.ToJSON ResponseJson where
  toJSON = Ae.genericToJSON Ae.defaultOptions { Ae.fieldLabelModifier = Ae.camelTo2 '-' }


