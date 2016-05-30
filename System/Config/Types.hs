{- |
Stability   :  unstable
Portability :  portable
-}
module System.Config.Types
  ( Section
  , Key
  , Value
  , Configuration(..) )
where

    import Data.Text (Text)
    import Data.Ini (Ini(..))


    type Section = Text
    type Key     = Text
    type Value   = Text


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

