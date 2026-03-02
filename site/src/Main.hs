module Main (main) where

import           Bld                 ( bld )

import qualified Data.Text           as T

import           Options.Applicative

data Opt = Opt
   { optSrc :: FilePath
   , optOut :: FilePath
   , optUrl :: String
   }

opt :: ParserInfo Opt
opt = info (hlp <*> prs) (fullDesc <> header "site - static site generator")
 where
   prs = Opt
      <$> argument str (metavar "SRC_DIR"  <> help "source directory")
      <*> argument str (metavar "OUT_DIR"  <> help "output directory")
      <*> argument str (metavar "BASE_URL" <> help "base URL for the site")
   hlp = helper

main :: IO ()
main = do
   o <- execParser opt
   bld (optSrc o) (optOut o) (T.pack (optUrl o))
