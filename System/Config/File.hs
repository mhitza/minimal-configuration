{-# LANGUAGE ImplicitParams, RankNTypes #-}
{- |
Stability   :  unstable
Portability :  non-portable

module description starting at first column
-}
module System.Config.File
    ( Key
    , Value
    , InteractiveValidator
    , Configuration()
    -- * Configuration interaction
    , withConfiguration
    , withConfigurationImplicit
    , saveConfiguration
    -- * Working with the options
    , hasV
    , getV
    , removeV
    , replaceV
    -- * Interactive configuration building
    , acceptAnything
    , acceptNonBlank
    , fillInteractively
    , fillInteractivelyWhen
    )
where

    import System.FilePath
    import System.Directory
    import qualified Data.TConfig as TConfig

    import Control.Monad
    import Data.Map
    import Data.Maybe
    import System.IO


    type Key                  = String 
    type Value                = String
    type InteractiveValidator = Value -> IO (Either String Value)

    data Configuration = Configuration 
                       { new :: Bool
                       , filepath :: FilePath
                       , options :: Map Key Value
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


    acceptAnything :: InteractiveValidator
    acceptAnything = return . Right 

    acceptNonBlank :: InteractiveValidator
    acceptNonBlank value | Prelude.null value = return $ Left "Empty string is not accepted"
                         | otherwise          = return $ Right value


    fillInteractivelyWhen :: (Configuration -> Bool) -> Configuration -> [(Key, InteractiveValidator)] -> IO Configuration
    fillInteractivelyWhen pred configuration methods | pred configuration = fillInteractively configuration methods
                                                     | otherwise          = return configuration

    fillInteractively :: Configuration -> [(Key, InteractiveValidator)] -> IO Configuration
    fillInteractively configuration methods = interactiveBuild >>= (return . Prelude.foldl (\c (key,value) -> setOrReplace key value c) configuration) where
        interactiveBuild = forM methods (uncurry requestLoop)
        setOrReplace key value c | hasV configuration key = replaceV c key value
                                 | otherwise              = addV c key value
        requestLoop key validator = do
            putStr (key ++ ": ")
            hFlush stdout
            input <- getLine >>= validator
            case input of (Right v) -> return (key, v)
                          (Left v)  -> putStrLn v >> requestLoop key validator


    withConfiguration :: String -> (Configuration -> IO b) -> IO b
    withConfiguration filename f = do
        homeDir <- homeDirectoryPath
        configuration <- readOrCreateAndRead $ homeDir ++ filename
        f configuration


    withConfigurationImplicit :: String -> ((?configuration :: Configuration) => IO b) -> IO b
    withConfigurationImplicit filename f = withConfiguration filename (\c -> let ?configuration = c in f)


    saveConfiguration :: Configuration -> IO ()
    saveConfiguration (Configuration { filepath=f, options=o }) = TConfig.writeConfig f o


    newC :: Configuration -> Bool
    newC = new 

    emptyC :: Configuration -> Bool
    emptyC = Data.Map.null . options

    hasV :: Configuration -> Key -> Bool
    hasV configuration key = isJust . TConfig.getValue key $ options configuration

    getV :: Configuration -> Key -> Maybe Value
    getV configuration key = TConfig.getValue key $ options configuration

    addV :: Configuration -> Key -> Value -> Configuration
    addV configuration key value = (\o -> configuration { options=o }) . TConfig.addKey key value $ options configuration

    removeV :: Configuration -> Key -> Configuration
    removeV configuration key = (\o -> configuration { options=o }) . TConfig.remKey key $ options configuration

    replaceV :: Configuration -> Key -> Value -> Configuration
    replaceV configuration key value = (\o -> configuration { options=o }) . TConfig.repConfig key value $ options configuration
