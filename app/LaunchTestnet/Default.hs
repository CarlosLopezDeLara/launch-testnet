{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module LaunchTestnet.Default (
    runDefault,
) where

import CLI (PoolCount (..))
import LaunchTestnet.Commons

runDefault :: FilePath -> PoolCount -> IO ()
runDefault outDir poolCount = do
    putStrLn $ "Running default testnet setup in: " ++ path outDir -- Use path helper
    (sSpec, aSpec, cSpec, cfgPath, topoPath) <- runDumpSpecs outDir

    mRelaysPath <- generateRelayFile outDir poolCount 6000

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
                , snaBasePort = 6000
                }
    spawnNodes spawnArgs
    putStrLn $ successText "\nDefault testnet setup finished. Nodes are starting in the background."
    putStrLn "You may need to wait a few moments for nodes to fully initialize and create their socket files."
