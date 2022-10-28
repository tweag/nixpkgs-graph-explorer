{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Graph where

import           Protolude
import qualified Data.Aeson as Ae
import qualified Data.Greskell.GraphSON as Gson


data GraphCmp m = GraphCmp
  { grToRaw :: !([Gson.GValue] -> m Text)
  , grToCytoWith :: !(forall a b c. (Gson.FromGraphSON a, Ae.ToJSON b, Ae.ToJSON c) => ([a] -> Either Text (b, c)) -> [Gson.GValue] -> m (Either Text (b, c)))
  }

newGraphCmp :: Monad m => GraphCmp m
newGraphCmp = GraphCmp
  { grToRaw = pure . show
  , grToCytoWith = \adapter gs -> pure $
      case traverse Gson.parseEither gs of
        Right a -> adapter a
        Left e -> Left "Adapter failed to parse Gremlin result"
  }

