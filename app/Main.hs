{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import CLI (Command (..), CustomPaths (..), PoolCount (..), optsParser)
import Control.Concurrent (threadDelay) 
import Control.Concurrent.Async (forConcurrently_)
import Control.Exception (IOException, try)
import Control.Monad (filterM, forM_, unless, void)
import qualified Data.ByteString as BS
import Data.FileEmbed (embedFile)
import Data.List (intercalate)
import Data.Semigroup ((<>))
import Options.Applicative (customExecParser, prefs, showHelpOnEmpty, showHelpOnError)
import System.Directory (copyFile, createDirectoryIfMissing, doesFileExist, makeAbsolute)
import System.Exit (ExitCode (ExitSuccess), exitFailure, exitWith)
import System.FilePath (takeFileName, (</>))
import System.IO (Handle, IOMode (AppendMode), hClose, hPutStrLn, openFile, stderr, stdout)
import System.Process (CreateProcess (..), ProcessHandle, StdStream (..), callProcess, createProcess, proc, waitForProcess)

-- | Embed the default spec/config/topology files
specShelley, specAlonzo, specConway, specTopology, specConfig :: BS.ByteString
specShelley = $(embedFile "data/specs/shelley.json")
specAlonzo = $(embedFile "data/specs/alonzo.json")
specConway = $(embedFile "data/specs/conway.json")
specTopology = $(embedFile "data/specs/topology.json")
specConfig = $(embedFile "data/specs/config.json")

-- | Arguments for creating testnet data
data CreateTestnetDataArgs = CreateTestnetDataArgs
    { ctaSpecShelley :: FilePath
    , ctaSpecAlonzo :: FilePath
    , ctaSpecConway :: FilePath
    , ctaOutDir :: FilePath
    , ctaPoolCount :: PoolCount
    , ctaRelaysFile :: Maybe FilePath -- Optional relays file
    , ctaTestnetMagic :: Word
    , ctaTotalSupply :: Word
    , ctaDelegatedSupply :: Word
    , ctaDrepKeys :: Int
    , ctaStakeDelegators :: Int
    }

-- | Common arguments for spawning cardano-node instances
data SpawnNodesArgs = SpawnNodesArgs
    { snaConfigPath :: FilePath
    , snaTopologyPath :: FilePath
    , snaOutDir :: FilePath -- Base directory for db, logs, pool-keys
    , snaPoolCount :: PoolCount
    , snaBasePort :: Int
    }

main :: IO ()
main = do
    let parserPrefs = prefs (showHelpOnEmpty <> showHelpOnError)
    cmd <- customExecParser parserPrefs optsParser
    case cmd of
        Default outDir poolCount -> runDefault outDir poolCount
        DumpSpecs dir -> runDumpSpecs dir >>= \_ -> putStrLn "Default spec files dumped successfully." -- Adjusted
        Custom paths poolCount -> runCustom paths poolCount

-- | Helper to run a process and wait for it, exiting on failure
runCliProcess :: String -> [String] -> String -> IO ()
runCliProcess cmd args desc = do
    putStrLn $ "Running: " ++ cmd ++ " " ++ unwords args
    (_, _, _, pHandle) <- createProcess (proc cmd args)
    exitCode <- waitForProcess pHandle
    unless (exitCode == ExitSuccess) $ do
        hPutStrLn stderr $ "Error: " ++ desc ++ " failed with exit code: " ++ show exitCode
        exitWith exitCode
    putStrLn $ "✓ " ++ desc ++ " completed successfully."

-- | Create testnet data using cardano-cli
createTestnetData :: CreateTestnetDataArgs -> IO ()
createTestnetData args = do
    let (PoolCount nPools) = ctaPoolCount args
    let commonArgs =
            [ "conway"
            , "genesis"
            , "create-testnet-data"
            , "--spec-shelley"
            , ctaSpecShelley args
            , "--spec-alonzo"
            , ctaSpecAlonzo args
            , "--spec-conway"
            , ctaSpecConway args
            , "--pools"
            , show nPools
            , "--stake-delegators"
            , show (ctaStakeDelegators args)
            , "--total-supply"
            , show (ctaTotalSupply args)
            , "--delegated-supply"
            , show (ctaDelegatedSupply args)
            , "--drep-keys"
            , show (ctaDrepKeys args)
            , "--testnet-magic"
            , show (ctaTestnetMagic args)
            , "--out-dir"
            , ctaOutDir args
            ]
        relayArgs = case ctaRelaysFile args of
            Just relays -> ["--relays", relays]
            Nothing -> []
    runCliProcess "cardano-cli" (commonArgs ++ relayArgs) "Create Testnet Data"

-- | Spawn N cardano-node processes
spawnNodes :: SpawnNodesArgs -> IO ()
spawnNodes args = do
    let (PoolCount nPools) = snaPoolCount args
    let baseOutDir = snaOutDir args -- This is, e.g., "example"
    putStrLn $ "Attempting to spawn " ++ show nPools ++ " node(s) using base output directory: " ++ baseOutDir
    putStrLn $ "Node Config file to be used by all nodes: " ++ snaConfigPath args
    putStrLn $ "Node Topology file to be used by all nodes: " ++ snaTopologyPath args

    forConcurrently_ [1 .. nPools] $ \i -> do
        let fullPoolSpecificKeyDir = baseOutDir </> "pools-keys" </> ("pool" <> show i)
            fullDbPathFromLaunchTestnet = baseOutDir </> "db" </> ("pool" <> show i) 
            fullSockPathFromLaunchTestnet = baseOutDir </> ("node" <> show i <> ".sock") 
            fullLogFilePath = fullPoolSpecificKeyDir </> "node.log"

        -- Paths to be passed to cardano-node, RELATIVE to baseOutDir (its future CWD)
        let relPoolDirName = "pool" <> show i
        let relPoolKeysPath = "pools-keys" </> relPoolDirName
        let relDbPathForNode = "db" </> relPoolDirName
        let relSockPathForNode = "node" <> show i <> ".sock"

        let nodePort = snaBasePort args + (i - 1)

        putStrLn $ "\n[Pool " ++ show i ++ "] Preparing to launch..."
        putStrLn $ "[Pool " ++ show i ++ "] Full path to Key/OpCert dir (for checks): " ++ fullPoolSpecificKeyDir
        putStrLn $ "[Pool " ++ show i ++ "] Full path to DB (for dir creation): " ++ fullDbPathFromLaunchTestnet
        putStrLn $ "[Pool " ++ show i ++ "] Full path to Socket (for info): " ++ fullSockPathFromLaunchTestnet
        putStrLn $ "[Pool " ++ show i ++ "] Full path to Log (for writing): " ++ fullLogFilePath
        putStrLn $ "[Pool " ++ show i ++ "] Node port: " ++ show nodePort

        putStrLn $ "[Pool " ++ show i ++ "] Ensuring directories exist (using full paths)..."
        createDirectoryIfMissing True fullPoolSpecificKeyDir
        createDirectoryIfMissing True fullDbPathFromLaunchTestnet -- Use full path for creation
        putStrLn $ "[Pool " ++ show i ++ "] Directories ensured."

        -- Critical file paths (full paths for doesFileExist)
        let fullKesKeyPath = fullPoolSpecificKeyDir </> "kes.skey"
            fullVrfKeyPath = fullPoolSpecificKeyDir </> "vrf.skey"
            fullOpCertPath = fullPoolSpecificKeyDir </> "opcert.cert"
            fullByronDeleCertPath = fullPoolSpecificKeyDir </> "byron-delegation.cert"
            fullByronSignKeyPath = fullPoolSpecificKeyDir </> "byron-delegate.key"

        putStrLn $ "[Pool " ++ show i ++ "] Verifying existence of critical key files (using full paths)..."
        let criticalFilesToVerify =
                [ (fullKesKeyPath, "KES Key")
                , (fullVrfKeyPath, "VRF Key")
                , (fullOpCertPath, "Operational Certificate")
                ]

        missingFiles <- filterM (fmap not . doesFileExist . fst) criticalFilesToVerify
        unless (null missingFiles) $ do
            forM_ missingFiles $ \(fp, name) ->
                hPutStrLn stderr $ "[Pool " ++ show i ++ "] FATAL ERROR: Required file (" ++ name ++ ") does not exist: " ++ fp
            fail $ "Missing critical file(s) for pool " ++ show i

        putStrLn $ "[Pool " ++ show i ++ "] All critical key files verified."

        -- Arguments for cardano-node, using paths relative to its CWD (baseOutDir)
        let nodeRunArgs =
                [ "run"
                , -- Config and topology are already just filenames due to takeFileName, correctly relative to baseOutDir
                  "--config"
                , takeFileName (snaConfigPath args)
                , "--topology"
                , takeFileName (snaTopologyPath args)
                , -- Other paths are now explicitly relative to baseOutDir
                  "--database-path"
                , relDbPathForNode
                , "--socket-path"
                , relSockPathForNode
                , "--shelley-kes-key"
                , relPoolKeysPath </> "kes.skey"
                , "--shelley-vrf-key"
                , relPoolKeysPath </> "vrf.skey"
                , "--byron-delegation-certificate"
                , relPoolKeysPath </> "byron-delegation.cert"
                , "--byron-signing-key"
                , relPoolKeysPath </> "byron-delegate.key"
                , "--shelley-operational-certificate"
                , relPoolKeysPath </> "opcert.cert"
                , "--host-addr"
                , "0.0.0.0"
                , "--port"
                , show nodePort
                ]

        putStrLn $ "[Pool " ++ show i ++ "] Constructing command for cardano-node: cardano-node " ++ unwords nodeRunArgs
        putStrLn $ "[Pool " ++ show i ++ "] (Note: paths in command are relative to the node's future CWD)"

        let processSpec =
                (proc "cardano-node" nodeRunArgs)
                    { std_err = Inherit
                    , cwd = Just baseOutDir 
                    }

        putStrLn $ "[Pool " ++ show i ++ "] Intended CWD for node process: " ++ baseOutDir

        eLogHandle <- try (openFile fullLogFilePath AppendMode) :: IO (Either IOException Handle)

        case eLogHandle of
            Left ex ->
                hPutStrLn stderr $ "[Pool " ++ show i ++ "] FATAL: Error opening log file " ++ fullLogFilePath ++ ". Exception: " ++ show ex
            Right logHandle -> do
                putStrLn $ "[Pool " ++ show i ++ "] Log file " ++ fullLogFilePath ++ " opened successfully."
                let finalProcessSpec = processSpec{std_out = UseHandle logHandle}

                eProcess <-
                    try (createProcess finalProcessSpec) ::
                        IO (Either IOException (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle))
                case eProcess of
                    Left exCreate -> do
                        hPutStrLn stderr $ "[Pool " ++ show i ++ "] FATAL: Failed to create cardano-node process. Exception: " ++ show exCreate
                        hClose logHandle
                    Right (_, _, _, _ph) -> do
                        putStrLn $ "[Pool " ++ show i ++ "] ✓ Successfully created cardano-node process."
                        putStrLn $ "[Pool " ++ show i ++ "] Node output (stdout) is being redirected to: " ++ fullLogFilePath
                        putStrLn $ "[Pool " ++ show i ++ "] Node errors (stderr) should appear on this console."

-- | Dump default spec files and config/topology into a directory
runDumpSpecs :: FilePath -> IO (FilePath, FilePath, FilePath, FilePath, FilePath)
runDumpSpecs dir = do
    let specsDir = dir </> "specs"
        shelleyPath = specsDir </> "shelley.json"
        alonzoPath = specsDir </> "alonzo.json"
        conwayPath = specsDir </> "conway.json"
        configPath = dir </> "config.json"
        topologyPath = dir </> "topology.json"

    createDirectoryIfMissing True specsDir

    BS.writeFile shelleyPath specShelley
    BS.writeFile alonzoPath specAlonzo
    BS.writeFile conwayPath specConway
    BS.writeFile configPath specConfig
    BS.writeFile topologyPath specTopology

    putStrLn $ "Default specs, config & topology dumped to: " ++ dir
    return (shelleyPath, alonzoPath, conwayPath, configPath, topologyPath)

-- | Default journey: Uses embedded specs.
runDefault :: FilePath -> PoolCount -> IO ()
runDefault outDir poolCount@(PoolCount nPools) = do
    putStrLn $ "Running default testnet setup in: " ++ outDir
    (sSpec, aSpec, cSpec, cfgPath, topoPath) <- runDumpSpecs outDir

    -- Generate relays file if multiple pools
    let mRelaysPath =
            if nPools > 1
                then Just (outDir </> "pool-relays.json")
                else Nothing
    case mRelaysPath of
        Just relaysPath -> do
            let entries =
                    [ "\""
                        <> show (i - 1) -- relay group name, 0-indexed
                        <> "\": [ \
                           \  { \"single host address\": { \
                           \    \"IPv4\": \"127.0.0.1\", \
                           \    \"IPv6\": null, \
                           \    \"port\": "
                        <> show (6000 + (i - 1)) -- Default base port
                        <> " } } ]"
                    | i <- [1 .. nPools]
                    ]
                relaysContent = "{\n" <> intercalate ",\n" entries <> "\n}\n"
            writeFile relaysPath relaysContent
            putStrLn $ "→ Wrote multi-pool relays config to: " <> relaysPath
        Nothing -> pure ()

    let createArgs =
            CreateTestnetDataArgs
                { ctaSpecShelley = sSpec
                , ctaSpecAlonzo = aSpec
                , ctaSpecConway = cSpec
                , ctaOutDir = outDir
                , ctaPoolCount = poolCount
                , ctaRelaysFile = mRelaysPath
                , ctaTestnetMagic = 42
                , ctaTotalSupply = 43000000000000
                , ctaDelegatedSupply = 9000000000000
                , ctaDrepKeys = 3
                , ctaStakeDelegators = 3
                }
    createTestnetData createArgs

    let spawnArgs =
            SpawnNodesArgs
                { snaConfigPath = cfgPath
                , snaTopologyPath = topoPath
                , snaOutDir = outDir
                , snaPoolCount = poolCount
                , snaBasePort = 6000 -- Default base port
                }
    spawnNodes spawnArgs
    putStrLn "Testnet setup finished. Nodes are starting in the background."
    putStrLn "You may need to wait a few moments for nodes to fully initialize and create their socket files."

-- | Custom journey: Uses user-provided spec files.
runCustom :: CustomPaths -> PoolCount -> IO ()
runCustom paths@CustomPaths{..} poolCount@(PoolCount nPools) = do
    putStrLn $ "Running custom testnet setup in: " ++ cpOutDir
    putStrLn $ "Number of pools to set up: " ++ show nPools

    -- Validate input files exist
    forM_
        [ (cpShelleySpec, "Shelley spec")
        , (cpAlonzoSpec, "Alonzo spec")
        , (cpConwaySpec, "Conway spec")
        , (cpConfigFile, "Config file")
        , (cpTopology, "Topology file")
        ]
        $ \(path, label) -> do
            exists <- doesFileExist path
            unless exists $ do
                hPutStrLn stderr $ "Error: " ++ label ++ " not found at " ++ path
                exitFailure

    -- Prepare output directory and copy custom files
    let specsDir = cpOutDir </> "specs"
        destShelley = specsDir </> "shelley.json"
        destAlonzo = specsDir </> "alonzo.json"
        destConway = specsDir </> "conway.json"
        destConfig = cpOutDir </> "config.json"
        destTopology = cpOutDir </> "topology.json"

    createDirectoryIfMissing True specsDir
    copyFile cpShelleySpec destShelley
    copyFile cpAlonzoSpec destAlonzo
    copyFile cpConwaySpec destConway
    copyFile cpConfigFile destConfig
    copyFile cpTopology destTopology
    putStrLn "Custom spec and config files copied to output directory."

    -- For custom, we won't automatically generate a relays file for create-testnet-data.
    -- The user's topology.json and config.json (for P2P) should handle inter-node communication.
    -- If cardano-cli requires --relays for multi-pool genesis with custom files,
    -- the user would need to provide such a file and potentially a CLI option for it.
    -- For now, passing Nothing for ctaRelaysFile.
    let mRelaysPathCustom = Nothing -- Or could be an option in CustomPaths
    let createArgs =
            CreateTestnetDataArgs
                { ctaSpecShelley = destShelley
                , ctaSpecAlonzo = destAlonzo
                , ctaSpecConway = destConway
                , ctaOutDir = cpOutDir
                , ctaPoolCount = poolCount
                , ctaRelaysFile = mRelaysPathCustom
                , ctaTestnetMagic = 42 -- Or make this configurable via CLI for custom
                , ctaTotalSupply = 43000000000000 -- Or make configurable
                , ctaDelegatedSupply = 9000000000000 -- Or make configurable
                , ctaDrepKeys = 3 -- Or make configurable
                , ctaStakeDelegators = 3 -- Or make configurable
                }
    createTestnetData createArgs

    let spawnArgs =
            SpawnNodesArgs
                { snaConfigPath = destConfig
                , snaTopologyPath = destTopology
                , snaOutDir = cpOutDir
                , snaPoolCount = poolCount
                , snaBasePort = 6000 -- Or make this configurable via CLI for custom
                }
    spawnNodes spawnArgs
    putStrLn "Testnet setup finished. Nodes are starting in the background."
    putStrLn "You may need to wait a few moments for nodes to fully initialize and create their socket files."
