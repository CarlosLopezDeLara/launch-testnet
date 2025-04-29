-- app/CLI.hs
{-# LANGUAGE OverloadedStrings #-}

module CLI
  ( Command(..)
  , optsParser
  ) where

import           Options.Applicative
import           Data.Semigroup                ((<>))

-- | Available subcommands
data Command
  = Default   FilePath
  | DumpSpecs FilePath
  | Custom    FilePath  FilePath  FilePath  FilePath  FilePath  FilePath
    -- ^ shelley.json, alonzo.json, conway.json, config.json, topology.json, out-dir
  deriving Show

optsParser :: ParserInfo Command
optsParser = info (parser <**> helper) $
     fullDesc
  <> header   "launch-testnet - spin up or dump spec files for a local Cardano testnet"
  <> progDesc "Commands: default, dump-spec-files, custom"

parser :: Parser Command
parser = hsubparser $
     commandGroup "Available commands"
  <> command "default"
       ( info defaultOpts
         ( progDesc "Launch testnet with default embedded specs" ) )
  <> command "dump-spec-files"
       ( info dumpSpecsOpts
         ( progDesc "Dump embedded specs + config/topology into DIR" ) )
  <> command "custom"
       ( info customOpts
         ( progDesc "Use your own genesis specs & node config + topology" ) )

defaultOpts :: Parser Command
defaultOpts = Default
  <$> strOption
      ( long "out-dir"
     <> metavar "DIR"
     <> help "Directory to write testnet data + config files" )

dumpSpecsOpts :: Parser Command
dumpSpecsOpts = DumpSpecs
  <$> strOption
      ( long "out-dir"
     <> metavar "DIR"
     <> help "Directory in which to dump embedded specs + config/topology" )

customOpts :: Parser Command
customOpts = Custom
  <$> strOption
      ( long "shelley-spec"
     <> metavar "FILE"
     <> help "Path to your custom shelley.json" )
  <*> strOption
      ( long "alonzo-spec"
     <> metavar "FILE"
     <> help "Path to your custom alonzo.json" )
  <*> strOption
      ( long "conway-spec"
     <> metavar "FILE"
     <> help "Path to your custom conway.json" )
  <*> strOption
      ( long "config"
     <> metavar "FILE"
     <> help "Path to your custom config.json" )
  <*> strOption
      ( long "topology"
     <> metavar "FILE"
     <> help "Path to your custom topology.json" )
  <*> strOption
      ( long "out-dir"
     <> metavar "DIR"
     <> help "Directory to write testnet data + config files" )
