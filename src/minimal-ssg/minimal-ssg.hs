-- title       = Agora: A minimal ssg after ten months' crawling
-- pubDate     = 2026-03-06
-- tags        = haskell, parser, 2026
-- description =

import qualified Data.Text           as T

-- > Please note that *Agora* is absolutely opinionated & have zero configuration support. Unless we share the same appreciation, it
--   would be confusing/imperfect for you to build your own site.
--
-- | Why building a ssg (april.2025)
--
--
--
--
--
-- The first attemp to actually build my own ssg began in April, 2025. It is written in Nim lang, and is (poorly) named `stoa`. To be
-- precisely, `stoa` is the markup language for that ssg, `stoae` (the plural of `stoa`, i suppose) is the directory/foundation where
-- `stoa` files are located, and `stoac` is the compiler that compiles `stoae` into the website/temple.
--
-- | The crawling to stoa 1.0.0
--
-- | The hierarchy of agora
--
-- | Stoa the markup language
--
-- || Interlude: an aching depart from lisp markup
--
-- | Polis the generated site
--
-- | Stoac the compiler (might need a better name)
--
-- || The necessary parts we need
--
-- || Data models
--
-- || Parsers
--
-- || Scanner & Builder
--
-- | Implementing mssg
--
-- We will use a minimal & ideomatic dependencies to save some locs.
--
-- || Homepage. tag
--
-- || CLI with optparse-applicative
--
-- It's ideomatic to use [optparse-applicative]() build to minimal CLI.

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

-- main :: IO ()
-- main = do
--    o <- execParser opt
--    bld (optSrc o) (optOut o) (T.pack (optUrl o))

-- | Reflexions
