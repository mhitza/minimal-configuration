{- |
Stability   :  unstable
Portability :  portable
-}
module System.Config.AssistedSetup
where

    import System.Config.Types
    import System.Config.File
    import System.Console.Byline
    import Data.Text (Text())

    import Control.Monad (foldM)


    fillIn :: Configuration -> [(Section, Key, Byline IO Value)] -> IO Configuration
    fillIn = foldM apply where
        apply c (section, key, rule) = do value <- runByline rule :: IO (Maybe Value)
                                          return $ maybe c (flip (set section key) c) value

    
    type Validator = Text -> Either Text Text

    mandatory :: Text -> Validator -> Byline IO Text
    mandatory message validator = askUntil message' Nothing (return . validator') where
        message' = text message
        validator' = either (Left . text) (Right . id) . validator
