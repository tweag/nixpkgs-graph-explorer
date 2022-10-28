{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Env where

import           Protolude
import qualified Control.Concurrent.STM as Stm


data Env = Env
  { envHost :: ![Char]
  , envPort :: !Int
  , envGremlinServerHost :: ![Char]
  , envGremlinServerPort :: !Int
  }

data EnvCmp m = EnvCmp
  { evGet :: !(m Env)
  , evPut :: !(Env -> m ())
  }

newEnvCmp :: (MonadIO m, MonadReader (Stm.TVar Env) m) => EnvCmp m
newEnvCmp = EnvCmp
  { evGet = ask >>= (liftIO . Stm.readTVarIO)
  , evPut = \env -> do
      tv <- ask
      liftIO . Stm.atomically. Stm.writeTVar tv $ env
  }
