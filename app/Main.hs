{-# LANGUAGE OverloadedStrings #-}

module Main where

import CLI (Command (..), optsParser) 
import Options.Applicative (customExecParser, prefs, showHelpOnEmpty, showHelpOnError)

-- Import the new modules
import LaunchTestnet.Default (runDefault)
import LaunchTestnet.Custom (runCustom)
import LaunchTestnet.Commons (runDumpSpecs) 

main :: IO ()
main = do
    let parserPrefs = prefs (showHelpOnEmpty <> showHelpOnError)
    cmd <- customExecParser parserPrefs optsParser
    case cmd of
        Default outDir poolCount      -> runDefault outDir poolCount
        DumpSpecs dir                 -> runDumpSpecs dir >> putStrLn "Default spec files dumped successfully."
        Custom paths poolCount        -> runCustom paths poolCount