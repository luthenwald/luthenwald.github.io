-- title       = Agora: A minimal ssg after eleven months' crawling
-- pubDate     = 2026-03-06
-- tags        = haskell, parser, 2026
-- description = Ideally, we can produce the best work in the best environment. To produce the best articles, I determined to
--               build the perfect static site generator for my preferences. It takes eleven months to reach version 1.0.0. This
--               is the story/design behind the agora.

import           Data.Text           ( Text )
import qualified Data.Text           as T

import           Options.Applicative

-- > Please note that *Agora* is absolutely opinionated & have zero configuration support. Unless we share the same appreciation, it
--   would be confusing/inefficient for you to use.
--
-- | Why building a ssg (april.2025)
--
-- In my search for the perfect ssg (static site generator), only [Bagatto](https://bagatto.co/) truly caught my eye. It
-- self-describes as a *transparent, extensible ssg*, plus the (most) minimal & elegant frontpage, I appreciated Bagatto immediately.
--
-- But yet still, i wanted a mechanism that fits my needs exactly. Rather than extensibility/flexibility, i prefer a more
-- compact/accurate one that align with my ideomatic preferences. I believe that only in an absolutely customized environment, I
-- shall be able to compose proses of quality.
--
-- A personal ssg is less reusable, less forgiving, and less immediately ergonomic for anyone except its author. It gives up the
-- comfort of established conventions. What it gains instead is a tighter correspondence between Weltanschauung & toolchain.
--
-- In other words, this is how i crafted the ssg that would do exactly what i want.
--
-- | The crawling to agora 1.0.0
--
-- || Genesis: a minimal markup language
--
-- > (Un)fortunately, the revision of this version of `stoa` is completely lost.
--
-- The first attemp to actually build my own ssg began in April, 2025. It is written in Nim lang, and is named `stoa`. To be
-- precisely, `stoa` is the markup language for that ssg, `stoae` (the plural of `stoa`) is the directory/foundation where `stoa`
-- files are located, and `stoac` is the compiler that compiles `stoae` into the polis/website. Other terms/aliases are also derived
-- based on the central greek word `stoa`.
--
-- ||| Why not use markdown
--
-- It's more of an aesthetic matter than anything else. For example, I find `|` is prettier than `#` as the delimiter of headings.
-- As I've mentioned earlier, it's more possible to write something of great value when I'm using a markup language I'm most
-- comfortable with. Thus if i this `|` is prettier than `#`, then this single matter is enough for me to design another
-- markup language.
--
-- The design rule of `stoa` is simple: only add a new markup when i actually need it, and give it the most minimal & elegant syntax.
--
-- The very initial vocabulary of `stoa` is:
--
-- -- TODO: use a haskell representation for this
--
-- - `| xxx`/`- xxx` for headings/(unordered-)lists where different nums of `|`/`-` indicate different levels.
-- - `*xxx*` for inline bolds
-- - `[xxx](xxx)` for inline links
-- - `|> lang ... |>` for code blocks
--
-- and that's all.
--
-- Essentially, the five elements above are already sufficient for a decent markup language. The design of agora will start from this.
--
-- || From markup language to literate programming
--
-- In most of my blogs, I would be using code for better illustration. And i need to verify the code compiles. If we are in a markup
-- file (.stoa), we need to implement another scanner that watch for file change, extract all code blocks, & pass to the actual compiler
-- for that language, which is inefficient.
--
-- || Interlude: an aching depart from lisp markup
--
-- ||| Parser comparison
--
-- Let's try to implement a parser for a subset of markdown & a lisp markup language respectively ...
--
-- TODO: use monadic parser here to implement a parser

-- ||| Lisp markup can be easily extended
--
-- ||| Lisp markup strays from my principle
--
-- The simplicity of a lisp markup compiler tempts me to add more features in the markup. It starts from `class-id`, but then more
-- and more combinators are added, to which extent, the complexity of the language has deviated from my primitive purpose of building
-- a minimal ssg.
--
-- | The hierarchy/anatomy of agora
--
-- || Stoa the markup language
--
-- We will illustrate the architecture of `stoa` by actually defining the data models.
--
-- `lexis`/inline is the

data Inl
   = Txt Text
   | Lnk Text Text
   | Bld Text
   | Cde Text
   | Ref Int
   deriving (Show, Eq)

-- `meros`/block is a list of inlines of a block of raw text that won't be processed by the parser.

data Blk
   = Hdg Int [Inl] Text
   | Par [Inl]
   | Lst Int [Inl]
   | Cal [Inl]
   | Ftn Int [Inl]
   | Cod Text
   | Vrb Text
   | Raw Text
   | Ins FilePath
   | Img FilePath
   deriving (Show, Eq)

-- we have an outline for each article page

data Out = Out
   { outTxt :: Text
   , outLvl :: Int
   , outId  :: Text
   } deriving (Show, Eq)

-- and the metadata of each article

data Met = Met
   { metTtl :: Text
   , metPub :: Text
   , metTag :: Text
   , metDsc :: Text
   } deriving (Show, Eq)

-- then a whole article page can be represented as

data Doc = Doc
   { docMet :: Met
   , docBlk :: [Blk]
   , docOut :: [Out]
   , docPth :: FilePath
   } deriving (Show, Eq)

-- we also aggregate the tags of all articles

data Tag = Tag
   { tagNam :: Text
   , tagDoc :: [Doc]
   } deriving (Show, Eq)

-- we will then define the parser combinators

-- || Polis the generated site
--
-- ||| The law of 1.44
--
-- ||| Tufte css but no sidenotes
--
-- || Stoac the compiler (might need a better name)
--
-- ||| Data models
--
-- ||| Parsers
--
-- ||| Scanner & Builder
--
-- | Implementing
--
-- We will use a minimal & ideomatic dependencies to save some locs.
--
-- || Home & tags
--
-- || CLI with optparse-applicative
--
-- It's ideomatic to use [optparse-applicative]() build to minimal CLI.


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
