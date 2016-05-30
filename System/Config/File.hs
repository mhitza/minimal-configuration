{- |
Stability   :  unstable
Portability :  portable
-}
module System.Config.File (
    -- * Basics
    -- ** Types
      Key 
    , Value
    , Configuration()
    -- ** Managing
    , saveConfiguration
    , loadGlobal
    , loadLocal
    -- ** CRUD
    , addV
    , hasV
    , getV
    , removeV
    , replaceV
    -- * Data \"entry\"
    -- 
    -- | It proved useful that for a few small cases to also have a way to \"build\"
    -- the configuration interactively. When you consider easy to validate fields
    -- (that don't depend on other fields), it seems to be worth to have this functionality
    -- included.
    
    -- ** Validation
    -- ** Predicates
    , newC
    , emptyC
    )
where

    import System.FilePath
    import System.Directory

    import Control.Monad
    import Data.Maybe

    import Data.Text (Text)
    import Data.Ini (Ini(..), unIni, WriteIniSettings(..), KeySeparator(..))
    import qualified Data.Ini as Ini
    import qualified Data.HashMap.Strict as HashMap


    type Key   = Text
    type Value = Text


    -- |While the internal representation is not exposed directly, an implementation
    -- of the 'Show' instance is provided in order to dump the configuration when that
    -- may be aidful in debugging. However, you will only see the key values stored
    -- inside the 'Map'
    data Configuration = Configuration 
                       { new :: Bool
                       , filepath :: FilePath
                       , options :: Ini
                       }

    instance Show Configuration where
        show c = show $ options c


    readOrCreateAndRead :: IO FilePath -> FilePath -> IO (Either String Configuration)
    readOrCreateAndRead filepath_provider filename = do
        file <- fmap (</> filename) filepath_provider
        fileFound <- doesFileExist file
        unless fileFound $ writeFile file ""
        config <- Ini.readIniFile file
        case config of (Left s)  -> return $ Left s
                       (Right i) -> return . Right $ Configuration { new=not fileFound, filepath=file, options=i }


    loadGlobal :: FilePath -> IO (Either String Configuration)
    loadGlobal = readOrCreateAndRead getHomeDirectory
    -- The configuration file name given is relative to the users home directory


    loadLocal :: FilePath -> IO (Either String Configuration)
    loadLocal = readOrCreateAndRead getCurrentDirectory
    -- Load configuration file relative to the current directory


    -- | The configuration will be saved into the same file it was read from, obviously
    saveConfiguration :: Configuration -> IO ()
    saveConfiguration (Configuration { filepath=f, options=o }) = Ini.writeIniFileWith settings f o
      where settings = WriteIniSettings { writeIniKeySeparator=EqualsKeySeparator }


    -- | Has this configuration just been created?
    newC :: Configuration -> Bool
    newC = new 


    -- | Configuration doesn't contain any values?
    emptyC :: Configuration -> Bool
    emptyC = HashMap.null . unwrap

    unwrap :: Configuration -> HashMap.HashMap Text (HashMap.HashMap Text Text)
    unwrap = unIni . options

    wrap :: Configuration -> HashMap.HashMap Text (HashMap.HashMap Text Text) -> Configuration
    wrap configuration  = \o -> configuration { options=(Ini o) }


    hasV :: Key -> Configuration -> Key -> Bool
    hasV section configuration key = isJust (getV section configuration key)


    getV :: Key -> Configuration -> Key -> Maybe Value
    getV section configuration key = join . fmap (HashMap.lookup key) . HashMap.lookup section $ unwrap configuration


    addV :: Key -> Configuration -> Key -> Value -> Configuration
    addV section configuration key value = wrap configuration . HashMap.adjust (HashMap.insert key value) section $ unwrap configuration


    removeV :: Key -> Configuration -> Key -> Configuration
    removeV section configuration key = wrap configuration . HashMap.adjust (HashMap.delete key) section $ unwrap configuration


    replaceV :: Key -> Configuration -> Key -> Value -> Configuration
    replaceV section configuration key value = wrap configuration $ HashMap.adjust (HashMap.adjust (const value) key) section $ unwrap configuration
