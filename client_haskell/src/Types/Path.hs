{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}

module Types.Path where

import           Protolude
import qualified Control.Concurrent.STM as Stm
import qualified Data.Graph as Gr
import qualified Data.Aeson as Ae
import qualified Data.Tree as Tr
import qualified Data.Greskell.GraphSON as Gson
import           Data.Greskell.GraphSON.GValue (unwrapAll)
import           Control.Lens as L
import qualified Data.Set as Set
import Text.Pretty.Simple (pPrint)
import qualified Data.Map.Strict as Map


data Path = Path
  { _pthLabels :: ![[Text]]
  , _pthObjects :: ![Text]
  } deriving (Eq, Show, Generic)

makeLenses ''Path

instance Ae.ToJSON Path where
  toJSON = Ae.genericToJSON Ae.defaultOptions { Ae.fieldLabelModifier = Ae.camelTo2 '_' . drop 4 }

instance Ae.FromJSON Path where
  parseJSON = Ae.genericParseJSON Ae.defaultOptions { Ae.fieldLabelModifier = Ae.camelTo2 '_' . drop 4 }

instance Gson.FromGraphSON Path where
  parseGraphSON = Ae.parseJSON . unwrapAll

