{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Args where

import           Protolude hiding ( option )
import           Options.Applicative

import           Env

getArgs :: IO Env
getArgs = execParser opts

pEnv :: Parser Env
pEnv = Env
  <$> argument str (metavar "HOST" <> help "host for server")
  <*> argument auto (metavar "PORT" <> help "port for server")
  <*> argument str (metavar "GREMLIN_HOST" <> help "host for gremlin server")
  <*> argument auto (metavar "GREMLIN_PORT" <> help "port for gremlin server")
  -- <*> optional
  --   ( strOption
  --   ( long "key"
  --   <> short 'k'
  --   <> metavar "KEY"
  --   <> help "Key file for HTTPS/TLS"
  --   ))
  -- <*> optional
  --   ( strOption
  --   ( long "cert"
  --   <> short 'c'
  --   <> metavar "CERT"
  --   <> help "Certificate file for HTTPS/TLS"
  --   ))

opts :: ParserInfo Env
opts =
  info (pEnv <**> helper)
     ( fullDesc
    <> progDesc "..."
    <> header "Nixpkgs graph explorer" )

