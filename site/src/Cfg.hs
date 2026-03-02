module Cfg where

import qualified Data.Map.Strict as Map
import           Data.Text       ( Text )

data Lng = Lng
   { lngNam :: Text
   , lngPfx :: Text   -- single-line comment prefix
   } deriving (Show, Eq)

haskell, fish :: Lng
haskell = Lng "haskell" "--"
fish    = Lng "fish"    "#"

extMap :: Map.Map String Lng
extMap = Map.fromList
   [ (".hs",   haskell)
   , (".fish", fish)
   ]

lngByExt :: String -> Maybe Lng
lngByExt = flip Map.lookup extMap
