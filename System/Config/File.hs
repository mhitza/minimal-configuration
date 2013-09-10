{-# LANGUAGE ImplicitParams, RankNTypes #-}
module System.Config.File
    ( Configuration(..)
    , withConfiguration
    , withConfigurationImplicit
    , saveConfiguration
    , hasValue
    , getValue
    , removeValue
    , replaceValue )
where

    import System.FilePath
    import System.Directory
    import qualified Data.TConfig as TConfig

    import Control.Monad
    import Data.Map
    import Data.Maybe


    data Configuration = Configuration 
                       { new :: Bool
                       , filepath :: FilePath
                       , options :: Map String String
                       }

    homeDirectoryPath :: IO String
    homeDirectoryPath = do
        homeDir <- getHomeDirectory
        return $ homeDir ++ [pathSeparator]


    readOrCreateAndRead :: FilePath -> IO Configuration
    readOrCreateAndRead filepath' = do
        fileFound <- doesFileExist filepath'
        unless fileFound $ writeFile filepath' ""
        config <- TConfig.readConfig filepath'
        return Configuration { new=not fileFound, filepath=filepath', options=config }


    withConfiguration :: String -> (Configuration -> IO b) -> IO b
    withConfiguration filename f = do
        homeDir <- homeDirectoryPath
        configuration <- readOrCreateAndRead $ homeDir ++ filename
        f configuration


    withConfigurationImplicit :: String -> ((?configuration :: Configuration) => IO b) -> IO b
    withConfigurationImplicit filename f = withConfiguration filename (\c -> let ?configuration = c in f)


    saveConfiguration :: Configuration -> IO ()
    saveConfiguration (Configuration { filepath=f, options=o }) = TConfig.writeConfig f o


    hasValue :: String -> Configuration -> Bool
    hasValue key = isJust . TConfig.getValue key . options

    getValue :: String -> Configuration -> Maybe String
    getValue key = TConfig.getValue key . options

    addValue :: Configuration -> String -> String -> Configuration
    addValue configuration key value = (\o -> configuration { options=o }) . TConfig.addKey key value $ options configuration

    removeValue :: Configuration -> String -> Configuration
    removeValue configuration key = (\o -> configuration { options=o }) . TConfig.remKey key $ options configuration

    replaceValue :: Configuration -> String -> String -> Configuration
    replaceValue configuration key value = (\o -> configuration { options=o }) . TConfig.repConfig key value $ options configuration
