{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.Cytoscape where

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

data CytoNode = CytoNode
  { _cynId :: !Int
  , _cynName :: !(Maybe Text)
  , _cynValue :: !(Maybe Text)
  } deriving (Eq, Show, Generic)

data CytoEdge = CytoEdge
  { _cyeId :: !Int
  , _cyeSource :: !Int
  , _cyeTarget :: !Int
  , _cyeLabel :: !(Maybe Text)
  } deriving (Eq, Show, Generic)

data CytoElements = CytoElements
  { _cytNodes :: !(Map Int CytoNode)
  , _cytEdges :: ![CytoEdge]
  } deriving (Eq, Show, Generic)

makeLenses ''CytoNode
makeLenses ''CytoEdge
makeLenses ''CytoElements

instance Ae.ToJSON CytoNode where
  toJSON = Ae.genericToJSON Ae.defaultOptions { Ae.fieldLabelModifier = Ae.camelTo2 '_' . drop 4 }

instance Ae.ToJSON CytoEdge where
  toJSON = Ae.genericToJSON Ae.defaultOptions { Ae.fieldLabelModifier = Ae.camelTo2 '_' . drop 4 }

instance Ae.ToJSON CytoElements where
  toJSON c = do
    let dataNodes = c ^.. cytNodes . traversed . L.to (\n -> Ae.object ["data" Ae..= n])
    let dataEdges = c ^.. cytEdges . traversed . L.to (\e -> Ae.object ["data" Ae..= e])
    Ae.object ["nodes" Ae..= dataNodes, "edges" Ae..= dataEdges]


data TableDatum = TableDatum
  { _tdName :: !Text
  , _tdNeighbours :: ![Text]
  } deriving (Eq, Show, Generic)

makeLenses ''TableDatum

instance Ae.ToJSON TableDatum where
  toJSON = Ae.genericToJSON Ae.defaultOptions { Ae.fieldLabelModifier = Ae.camelTo2 '_' . drop 3 }

