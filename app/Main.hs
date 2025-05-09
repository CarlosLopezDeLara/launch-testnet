{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import CLI (Command (..), PoolCount (PoolCount), optsParser)
import Control.Concurrent.Async (forConcurrently_)
import Control.Monad (forM_, unless)
import qualified Data.ByteString as BS
import Data.FileEmbed (embedFile)
import Data.List (intercalate)
import Data.Semigroup ((<>))
import Options.Applicative (customExecParser, prefs, showHelpOnEmpty, showHelpOnError)
import System.Directory (copyFile, createDirectoryIfMissing, doesFileExist)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (IOMode (AppendMode), hPutStrLn, openFile, stderr)
import System.Process (CreateProcess (..), StdStream (..), callCommand, callProcess, createProcess, proc)

-- | Embed the default spec/config/topology files
specShelley, specAlonzo, specConway, specTopology, specConfig :: BS.ByteString
specShelley = $(embedFile "data/specs/shelley.json")
specAlonzo = $(embedFile "data/specs/alonzo.json")
specConway = $(embedFile "data/specs/conway.json")
specTopology = $(embedFile "data/specs/topology.json")
specConfig = $(embedFile "data/specs/config.json")

main :: IO ()
main = do
    let parserPrefs = prefs (showHelpOnEmpty <> showHelpOnError)
    cmd <- customExecParser parserPrefs optsParser
    case cmd of
        Default dir nPools -> runDefault dir nPools
        DumpSpecs dir -> runDumpSpecs dir >> putStrLn "DumpSpecs done."
        Custom sh al cw cfg topo dir ->
            runCustom sh al cw cfg topo dir

-- | Dump default spec files and config/topology into a directory
runDumpSpecs :: FilePath -> IO ()
runDumpSpecs dir = do
    let specsDir = dir </> "specs"
        configPath = dir </> "config.json"
        topologyPath = dir </> "topology.json"

    createDirectoryIfMissing True specsDir

    BS.writeFile (specsDir </> "shelley.json") specShelley
    BS.writeFile (specsDir </> "alonzo.json") specAlonzo
    BS.writeFile (specsDir </> "conway.json") specConway
    BS.writeFile configPath specConfig
    BS.writeFile topologyPath specTopology

    putStrLn $ "Specs, config & topology dumped to: " ++ dir

-- | Launch N cardano-node processes, each in its own pool directory
spawnNodes ::
    -- | out-dir
    FilePath ->
    -- | config.json path
    FilePath ->
    -- | topology.json path
    FilePath ->
    PoolCount ->
    IO ()
spawnNodes dir configPath topoPath (PoolCount n) =
    forConcurrently_ [1 .. n] $ \i -> do
        let poolDir = dir </> "pools-keys" </> ("pool" <> show i)
            dbPath = dir </> "db" </> ("pool" <> show i)
            sockPath = dir </> ("node" <> show i <> ".sock")
            logFile = poolDir </> "node.log"
            port = 6000 + (i - 1)

            args =
                [ "run"
                , "--config"
                , configPath
                , "--topology"
                , topoPath
                , "--database-path"
                , dbPath
                , "--socket-path"
                , sockPath
                , "--shelley-kes-key"
                , poolDir </> "kes.skey"
                , "--shelley-vrf-key"
                , poolDir </> "vrf.skey"
                , "--byron-delegation-certificate"
                , poolDir </> "byron-delegation.cert"
                , "--byron-signing-key"
                , poolDir </> "byron-delegate.key"
                , "--shelley-operational-certificate"
                , poolDir </> "opcert.cert"
                , "--host-addr"
                , "0.0.0.0"
                , "--port"
                , show port
                ]

        h <- openFile logFile AppendMode
        _ <-
            createProcess
                (proc "cardano-node" args)
                    { std_out = UseHandle h
                    , std_err = Inherit
                    }
        putStrLn $ "✓ started pool " <> show i <> " on port " <> show port

-- | Default journey. Creates a testnet with the embedded specs and custom number of pools
runDefault :: FilePath -> PoolCount -> IO ()
runDefault dir (PoolCount nPools) = do
    runDumpSpecs dir

    let relaysPath = dir </> "pool-relays.json"
        entries =
            [ "\""
                <> show (i - 1)
                <> "\": [ \
                   \{ \"single host address\": { \
                   \\"IPv4\": \"127.0.0.1\", \
                   \\"IPv6\": null, \
                   \\"port\": "
                <> show (6000 + (i - 1))
                <> " } } ]"
            | i <- [1 .. nPools]
            ]
        relaysContent = "{\n" <> intercalate ",\n" entries <> "\n}\n"
    writeFile relaysPath relaysContent
    putStrLn $ "→ wrote relays config to: " <> relaysPath

    callProcess
        "cardano-cli"
        [ "conway"
        , "genesis"
        , "create-testnet-data"
        , "--spec-shelley"
        , dir </> "specs/shelley.json"
        , "--spec-alonzo"
        , dir </> "specs/alonzo.json"
        , "--spec-conway"
        , dir </> "specs/conway.json"
        , "--pools"
        , show nPools
        , "--relays"
        , relaysPath
        , "--stake-delegators"
        , "3"
        , "--total-supply"
        , "43000000000000"
        , "--delegated-supply"
        , "9000000000000"
        , "--drep-keys"
        , "3"
        , "--testnet-magic"
        , "42"
        , "--out-dir"
        , dir
        ]

    -- 3) Launch one cardano-node per pool, in the background
    spawnNodes dir (dir </> "config.json") (dir </> "topology.json") (PoolCount nPools)

-- | Launch a custom testnet with user-provided specs/config/topology
runCustom ::
    -- | shelleySpec
    FilePath ->
    -- | alonzoSpec
    FilePath ->
    -- | conwaySpec
    FilePath ->
    -- | configFile
    FilePath ->
    -- | topologyFile
    FilePath ->
    -- | out-dir
    FilePath ->
    IO ()
runCustom shelleySpec alonzoSpec conwaySpec configFile topologyFile dir = do
    putStrLn $ "Running custom testnet in: " ++ dir

    -- Validate inputs
    forM_
        [ (shelleySpec, "Shelley spec")
        , (alonzoSpec, "Alonzo spec")
        , (conwaySpec, "Conway spec")
        , (configFile, "Config file")
        , (topologyFile, "Topology file")
        ]
        $ \(path, label) -> do
            exists <- doesFileExist path
            unless exists $ do
                hPutStrLn stderr $ label ++ " not found: " ++ path
                exitFailure

    let specsDir = dir </> "specs"
        destShelley = specsDir </> "shelley.json"
        destAlonzo = specsDir </> "alonzo.json"
        destConway = specsDir </> "conway.json"
        configDest = dir </> "config.json"
        topologyDest = dir </> "topology.json"

    createDirectoryIfMissing True specsDir

    copyFile shelleySpec destShelley
    copyFile alonzoSpec destAlonzo
    copyFile conwaySpec destConway
    copyFile configFile configDest
    copyFile topologyFile topologyDest

    callProcess
        "cardano-cli"
        [ "conway"
        , "genesis"
        , "create-testnet-data"
        , "--spec-shelley"
        , destShelley
        , "--spec-alonzo"
        , destAlonzo
        , "--spec-conway"
        , destConway
        , "--pools"
        , "1"
        , "--stake-delegators"
        , "3"
        , "--total-supply"
        , "43000000000000"
        , "--delegated-supply"
        , "9000000000000"
        , "--drep-keys"
        , "3"
        , "--testnet-magic"
        , "42"
        , "--out-dir"
        , dir
        ]

    let quoted p = '"' : p ++ "\""
        dbPath = dir </> "db"
        sockPath = dir </> "node.sock"
        poolDir = dir </> "pools-keys/pool1"
        logFile = poolDir </> "node.log"
        cmd =
            intercalate
                " "
                [ "cardano-node run"
                , "--config"
                , quoted configDest
                , "--topology"
                , quoted topologyDest
                , "--database-path"
                , quoted dbPath
                , "--socket-path"
                , quoted sockPath
                , "--shelley-kes-key"
                , quoted (poolDir </> "kes.skey")
                , "--shelley-vrf-key"
                , quoted (poolDir </> "vrf.skey")
                , "--byron-delegation-certificate"
                , quoted (poolDir </> "byron-delegation.cert")
                , "--byron-signing-key"
                , quoted (poolDir </> "byron-delegate.key")
                , "--shelley-operational-certificate"
                , quoted (poolDir </> "opcert.cert")
                , "--host-addr"
                , "0.0.0.0"
                , "--port"
                , "6000"
                , "| tee -a"
                , quoted logFile
                ]
    callCommand cmd
