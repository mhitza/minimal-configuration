{- |
Stability   :  unstable
Portability :  portable

IMPORTANT NOTE: this module works with the user's home directory, and that is the place where
the configuration will be read from and persisted.

Before even taking on the reference, consider the following example. Four things are
done here: load configuration stored in /.apprc/, get the /name/ value and print a
greeting; alternatively request that information from the user and save the new configuration.

> import System.Config.File
>
> main = withConfiguration ".apprc" $ \conf -> do
>     let name = getV conf "name"
>     case name of Just n  -> putStrLn $ "hello " ++ n
>                  Nothing -> conf' <- fillInteractively conf [("name", acceptNonBlank)]
>                             saveConfiguration conf'
-}
module System.Config.File (
    -- * Types
      Key 
    , Value
    , InteractiveValidator
    , Configuration()
    -- * Configuration interaction
    , withConfiguration
    , loadConfiguration
    , saveConfiguration
    -- * Working with the options
    , newC
    , emptyC
    -- * Accessors/mutators
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


    type Key   = String 
    type Value = String
    -- |Via the 'Left' data constructor we are able to pass the message necessary to
    -- notify the user that the inputed data is not valid
    type InteractiveValidator = Value -> IO (Either String Value)


    -- |While the internal representation is not exposed directly, an implementation
    -- of the 'Show' instance is provided in order to dump the configuration when that
    -- may be aidful in debugging. However, you will only see the key values stored
    -- inside the 'Map'
    data Configuration = Configuration 
                       { new :: Bool
                       , filepath :: FilePath
                       , options :: Map Key Value
                       }

    instance Show Configuration where
        show c = show $ options c


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


    withConfiguration :: String -- ^Configuration file name
                      -> (Configuration -> IO b)
                      -> IO b
    withConfiguration filename f = loadConfiguration filename >>= \c -> f c
    -- ^However if you like to stack software ala @ withSocketsDo $ withX $ withY @ this might not 
    -- be your preferred approach. You could go with the following approach, which was excluded for
    -- library portability:
    --
    -- > {-# LANGUAGE ImplicitParams, RankNTypes #-}
    -- > import System.Config.File
    -- >
    -- > withConfigurationImplicit :: String -> ((?configuration :: Configuration) => IO b) -> IO b
    -- > withConfigurationImplicit filename f = withConfiguration filename (\c -> let ?configuration = c in f)
    -- >
    -- > main = withConfigurationImplicit ".apprc" $ do
    -- >    print $ hasV "name" ?configuration
    -- >    print $ getV "name" ?configuration


    loadConfiguration :: String -- ^ Configuration file name
                      -> IO Configuration
    loadConfiguration filename = do
        homeDir <- homeDirectoryPath
        configuration <- readOrCreateAndRead $ homeDir ++ filename
        return configuration


    -- | The configuration will be saved into the same file it was read from, obviously
    saveConfiguration :: Configuration -> IO ()
    saveConfiguration (Configuration { filepath=f, options=o }) = TConfig.writeConfig f o


    -- | Has this configuration just been created?
    newC :: Configuration -> Bool
    newC = new 


    -- | Configuration doesn't contain any values?
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
