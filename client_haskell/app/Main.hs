{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Protolude
import qualified Control.Concurrent.STM as Stm

import qualified Args
import qualified Env
import qualified Graph
import qualified Gremlin
import qualified Router
import qualified Html

newProg :: Applicative m => Router.RouterCmp m -> m ()
newProg Router.RouterCmp{..} = rtRun

main :: IO ()
main = do
  -- let
  --   env = Env.Env
  --     { Env.envPort = 5002
  --     , Env.envGremlinServerHost = "localhost"
  --     , Env.envGremlinServerPort = 8182
  --     }
  env <- Args.getArgs
  envTV <- Stm.newTVarIO env
  let envC = Env.newEnvCmp
  let graphC = Graph.newGraphCmp
  let gremlinC = Gremlin.newGremlinCmp envC graphC
  let htmlC = Html.newHtmlCmp
  let routerC = Router.newRouterCmp envC gremlinC htmlC
  runReaderT (newProg routerC) envTV
