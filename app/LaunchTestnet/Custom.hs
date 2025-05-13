{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module LaunchTestnet.Custom (
    runCustom,
) where

import CLI (CustomPaths (..), PoolCount (..))
import Control.Monad (forM_, unless)
import LaunchTestnet.Commons
import System.Directory (copyFile, createDirectoryIfMissing, doesFileExist)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)

runCustom :: CustomPaths -> PoolCount -> Word -> IO ()
runCustom paths@CustomPaths{..} poolCount testnetMagicFromCli = do
    putStrLn $ "Running custom testnet setup in: " ++ path cpOutDir
    let (PoolCount nPools) = poolCount
    putStrLn $ "Number of pools to set up: " ++ show nPools

    forM_
        [ (cpShelleySpec, "Shelley spec")
        , (cpAlonzoSpec, "Alonzo spec")
        , (cpConwaySpec, "Conway spec")
        , (cpConfigFile, "Config file")
        , (cpTopology, "Topology file")
        ]
        $ \(fp, label) -> do
            exists <- doesFileExist fp
            unless exists $ do
                hPutStrLn stderr $ "Error: " ++ label ++ " not found at " ++ path fp
                exitFailure

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

    let customBasePort = 6000
    mRelaysPathCustom <- generateRelayFile cpOutDir poolCount customBasePort

    let createArgs =
            CreateTestnetDataArgs
                { ctaSpecShelley = destShelley
                , ctaSpecAlonzo = destAlonzo
                , ctaSpecConway = destConway
                , ctaOutDir = cpOutDir
                , ctaPoolCount = poolCount
                , ctaRelaysFile = mRelaysPathCustom
                , ctaTestnetMagic = 42
                , ctaTotalSupply = 43000000000000
                , ctaDelegatedSupply = 9000000000000
                , ctaDrepKeys = 3
                , ctaStakeDelegators = 3
                }
    createTestnetData createArgs

    let spawnArgs =
            SpawnNodesArgs
                { snaConfigPath = destConfig
                , snaTopologyPath = destTopology
                , snaOutDir = cpOutDir
                , snaPoolCount = poolCount
                , snaBasePort = customBasePort
                }
    spawnNodes spawnArgs
    putStrLn $ successText "\nCustom testnet setup finished. Nodes are starting in the background."
    putStrLn "You may need to wait a few moments for nodes to fully initialize and create their socket files."
    
    printEnvInstructions cpOutDir poolCount testnetMagicFromCli 