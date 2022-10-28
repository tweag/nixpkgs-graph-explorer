{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Gremlin where

import           Protolude
import           Control.Monad.IO.Unlift
import qualified Network.Greskell.WebSocket as Gw
import qualified Data.Greskell.GraphSON as Gson
import qualified Data.Aeson as Ae

import qualified Env
import qualified Html
import qualified Graph

newtype GremlinCmp m = GremlinCmp
  { grSubmit :: forall a b c. (MonadIO m, Gson.FromGraphSON a, Ae.ToJSON b, Ae.ToJSON c) => ([a] -> Either Text (b, c)) -> Text -> m (Text, Maybe (b, c))
  }

newGremlinCmp :: MonadUnliftIO m => Env.EnvCmp m -> Graph.GraphCmp m -> GremlinCmp m
newGremlinCmp env gr = GremlinCmp
  { grSubmit = submitGremlin env gr
  }

submitGremlin
  :: (MonadIO m, Gson.FromGraphSON a, Ae.ToJSON b, Ae.ToJSON c)
  => Env.EnvCmp m -> Graph.GraphCmp m
  -> ([a] -> Either Text (b, c))
  -> Text
  -> m (Text, Maybe (b, c))
submitGremlin env@Env.EnvCmp{..} Graph.GraphCmp{..} adapter g = do
  env <- evGet
  res <-
    liftIO $ do
      let gremlinClient = Gw.connect (Env.envGremlinServerHost env) (Env.envGremlinServerPort env)
      bracket gremlinClient Gw.close $ \client -> do
        result_handle <- Gw.submitRaw client g Nothing
        toList <$> Gw.slurpResults result_handle
  raw <- grToRaw res
  cyto <- grToCytoWith adapter res
  pure (raw, rightToMaybe cyto)


{-
g.V()
.filter{it.get().value('outputPath').matches(".*auto-multiple-choice-1.5.2.*")}
.repeat(outE().otherV().simplePath())
.until(__.not(outE().simplePath()))
.path()
.by('outputPath')
.by(label)

g.V()
.has('outputPath', '/nix/store/5wj3nq4gdwyih4j7vl58hglvgw4ls9zg-auto-multiple-choice-1.5.2')
.repeat(outE().otherV().simplePath())
.until(__.not(outE().simplePath()))
.path()
.by('outputPath')
.by(label)
-}
