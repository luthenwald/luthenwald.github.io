module Scn (Src(..), scn) where

import           Cfg              ( Lng, lngByExt )

import           System.Directory ( doesDirectoryExist, listDirectory )
import           System.FilePath  ( takeExtension, (</>) )

data Src = Src
   { srcPth :: FilePath
   , srcLng :: Lng
   } deriving (Show, Eq)

scn :: FilePath -> IO [Src]
scn dir = do
   ent <- listDirectory dir
   concat <$> mapM (go . (dir </>)) ent
 where
   go pth = do
      isD <- doesDirectoryExist pth
      if isD then scn pth
      else pure $ case lngByExt (takeExtension pth) of
         Just lng -> [Src pth lng]
         Nothing  -> []
