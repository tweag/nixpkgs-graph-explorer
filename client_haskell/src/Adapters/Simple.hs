{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Adapters.Simple where

import           Protolude
import qualified Data.Greskell.GraphSON as Gson
import           Control.Lens as L
import qualified Data.Map.Strict as Map

import           Types.Cytoscape
import           Types.Path

----------------------------------------------------------------------------------------------------
  -- Simple adapter for paths of nodes interleaved with edges
----------------------------------------------------------------------------------------------------

adapter :: [Gson.GValue] -> Either Text ([TableDatum], CytoElements)
adapter gs =
  case traverse Gson.parseEither gs of
    Right paths -> do
      cyto <- pathsToCyto paths
      tableData <- maybeToRight "Could not generate table data" $ cytoToTableData cyto
      Right (tableData, cyto)
    Left e -> Left "Could not parse Gremlin result"

----------------------------------------------------------------------------------------------------

ratchet3 :: [Text] -> Either Text [(Text, Text, Text)]
ratchet3 [] = Right []
ratchet3 (n0:e:n1:tl) = Right [(n0, e, n1)] <> ratchet3 (n1 : tl)
ratchet3 _ = Left "Could not create triplets"

nensToCyto :: [(Text, Text, Text)] -> CytoElements
nensToCyto nens =
  let
    (nm, em) = f 0 mempty mempty nens
    nodes = nm ^@.. itraversed ^.. traversed . L.to (\(val, id_) -> (id_, CytoNode id_ (Just val) (Just val))) & Map.fromList
    edges = em ^@.. itraversed ^.. traversed . L.to (\((f, t), (id_, val)) -> CytoEdge id_ f t (Just val))
  in
    CytoElements nodes edges
  where
    f :: Int -> Map Text Int -> Map (Int, Int) (Int, Text) -> [(Text, Text, Text)] -> (Map Text Int, Map (Int, Int) (Int, Text))
    f _ nMap eMap [] = (nMap, eMap)
    f i nMap eMap ((n0, e0, n1):tl) = do
      let
        (i0, nMap') =
          case Map.lookup n0 nMap of
            Just n -> (n, nMap)
            Nothing -> (i, Map.insert n0 i nMap)
        (i1, nMap'') =
          case Map.lookup n1 nMap' of
            Just n -> (n, nMap')
            Nothing -> (i + 1, Map.insert n1 (i + 1) nMap')
        (i2, eMap') =
          case Map.lookup (i0, i1) eMap of
            Just _ -> (i + 2, eMap)
            Nothing -> (i + 2, Map.insert (i0, i1) (i + 2, e0) eMap)
      f (i + 3) nMap'' eMap' tl


pathsToCyto :: [Path] -> Either Text CytoElements
pathsToCyto paths = do
  nens <- paths
      ^.. traversed
      . pthObjects
      . L.to ratchet3
      & sequenceA
  pure . nensToCyto . join $ nens

cytoToAdjacencyList :: CytoElements -> Maybe (Map Int [Int])
cytoToAdjacencyList cyto = do
  f (Just mempty) $ cyto ^. cytEdges
  where
    f :: Maybe (Map Int [Int]) -> [CytoEdge] -> Maybe (Map Int [Int])
    f map [] = map
    f Nothing _ = Nothing
    f (Just map) (CytoEdge _ s t _:tl) = do
      ms <- cyto ^? cytNodes . ix s
      mt <- cyto ^? cytNodes . ix t
      let
        map' = map
          & Map.insertWith (<>) (ms ^. cynId) [mt ^. cynId]
          & Map.insertWith (<>) (mt ^. cynId) [ms ^. cynId]
      f (Just map') tl

cytoToTableData :: CytoElements -> Maybe [TableDatum]
cytoToTableData cyto = do
  al <- Map.toList <$> cytoToAdjacencyList cyto
  f al
  where
    f :: [(Int, [Int])] -> Maybe [TableDatum]
    f [] = Just mempty
    f ((n0, nn):tl) = do
      n0' <- cyto ^? cytNodes . ix n0 . cynName . _Just
      nn' <- sequenceA $ nn ^.. traversed . L.to (\i -> cyto ^? cytNodes . ix i . cynName . _Just)
      pure [TableDatum n0' nn'] <> f tl
