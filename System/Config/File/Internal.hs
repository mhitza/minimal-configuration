{- |
Stability   :  unstable
Portability :  portable
-}
module System.Config.File.Internal
where

    import System.Config.Types

    import qualified Data.HashMap.Strict as HashMap
    import Data.Ini (Ini(..))
    import qualified Data.Ini as Ini

    import System.Directory (doesFileExist)
    import System.FilePath ((</>))
    import Data.Bool (bool)


    readOrCreateAndRead :: IO FilePath -> FilePath -> IO (Either String Configuration)
    readOrCreateAndRead filepath_provider filename = do
        file <- fmap (</> filename) filepath_provider
        fileFound <- doesFileExist file
        config <- bool (return . Right $ Ini HashMap.empty) (Ini.readIniFile file) fileFound
        case config of (Left s)  -> return $ Left s
                       (Right i) -> return . Right $ Configuration { new=not fileFound, filepath=file, options=i }
