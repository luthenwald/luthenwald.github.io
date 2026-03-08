-- title       = Agora: A minimal ssg after eleven months crawling
-- pubDate     = 2026-03-06
-- tags        = haskell, parser, 2026
-- description = Ideally, we can produce the best work in the best environment. Thus I determined to build the perfect static site
--               generator based on my preferences. It takes eleven months to reach version 1.0.0. This is the story/design behind
--               agora the ssg.

-- So we can use string literals as Text values
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import           Data.Bifunctor       ( first )
import Control.Monad (void)
import           Data.Text            ( Text )
import qualified Data.Text            as T
import           Data.Void            ( Void )

import           Options.Applicative  ( ParserInfo, argument, fullDesc, header,
                                        help, helper, info, metavar, str )

import           Text.Megaparsec      ( Parsec, anySingle, between, choice,
                                         eof, errorBundlePretty, many,
                                        manyTill, runParser, skipMany, some,
                                        takeWhile1P, takeWhileP, try, (<|>) )
import           Text.Megaparsec.Char ( char, eol, hspace1, newline, string )

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
-- | The crawling to ἀγορά 1.0.0
--
-- || Genesis of στοά: a minimal markup language
--
-- > (Un)fortunately, the revision of this version of στοά is completely lost.
--
-- The first attemp to actually build my own ssg began in April.2025. It is written in Nim lang, named *stoa/στοά*. To be precisely,
-- στοά is the markup language for ἀγορά, *stoae/στοές* (the plural of stoa) is the directory/foundation where στοά files are
-- located, and *stoac* is the compiler that compiles στοές into the *polis/πόλις/website*. Other terms/aliases are also derived
-- based on the central greek word στοά.
--
-- ||| Why not markdown
--
-- Its more of an aesthetic matter. For example, I find `|` is prettier than `#` as the delimiter of headings. As I've mentioned
-- earlier, it's more possible to write something of value when I'm using a markup language most comfortable with. Thus if
-- i consider `|` prettier than `#`, then this single deviation is enough for me to design another markup language. And then i'm
-- free to use any delimiter, any markup, i.e. a totally ideomatic markup language.
--
-- The syntax rule of `stoa` is simple: a new markup is added only when i actually need it, and give it the most minimal & elegant
-- syntax.
--
-- We can already describe it as a tiny language & implement a parser for it. We use `Gen*` names so this genesis vocabulary stays
-- distinct from the later, formal definitions of stoa.
--
-- The foundational form is *lexis/λέξις/inline*, which is a sum of bold, link & regular text.

-- We define a tiny parser for this data model with [megaparsec](https://hackage.haskell.org/package/megaparsec):
type GenPar = Parsec Void Text

-- `megaparsec` does not go back automatically. take this example from [Megaparsec tutorial](https://markkarpov.com/tutorial/megaparsec.html)

alternatives :: GenPar (Char, Char)
alternatives = foo <|> bar
  where
   foo = (,) <$> char 'a' <*> char 'b'
   bar = (,) <$> char 'a' <*> char 'c'

-- this works
--
-- ````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````
-- λ> parseTest alternatives "ab"
-- ('a','b')
-- ````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````
--
-- but this doesn't
--
-- ````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````
-- λ> parseTest alternatives "ac"
-- 1:2:
--   |
-- 1 | ac
--   |  ^
-- unexpected 'c'
-- expecting 'b'
-- ````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````
--
-- i'll just paste the explanation from the tutorial:
--
-- > What happens here is that char 'a' part of `foo` (which is tried first) succeeded and consumed an a from the input stream. char
--   'b' then failed to match against 'c' and so we ended up with this error. An important detail here is that `(<|>)` did not even try
--   `bar` because `foo` has consumed some input
--
-- Note that `megaparsec` do support backtracking, but i take this as a very intriguing point & will adopt in my markup.
--
-- Thus an important principle for our markup language is: the alternatives should always work, i.e. any markup should have the unique
-- starting delimiter/char.

-- An inline is plain text, bold (*...*), or link [label](url).

data GenInl
   = GenTxt Text
   | GenBld Text
   | GenLnk Text Text
   deriving (Show, Eq)

-- The parser for inline is to choose between bold, link or plain text.

genInl :: GenPar GenInl
genInl = choice [genBld, genLnk, genTxt]
 where
   -- *text*, after `*`, consume one contiguous run of `non-*` characters, then close with `*`
   genBld = GenBld <$> between (char '*') (char '*') (takeWhile1P (Just "bold text") (`notElem` ("*\n" :: String)))
   -- [label](url)
   genLnk =
     GenLnk
      <$> between (char '[') (string "](") (takeWhile1P (Just "link label") (`notElem` ("]\n" :: String)))
      <*> takeWhile1P (Just "link URL") (`notElem` (")\n" :: String)) <* char ')'
   -- one or more chars that are neither `*` nor `[`.
   genTxt = GenTxt <$> takeWhile1P (Just "plain text") (`notElem` ("*[\n" :: String))

-- The next level is *meros/μέρος/block*, which is a sum of heading, (unordered-)list & (fenced) code block. Heading & list are
-- constituted by a list of inlines, while code block is the language name plus the raw text of code.
--
-- > We won't consider indented lists in this article to save some space.

data GenBlk
   = GenHdg Int [GenInl]
   | GenLst Int [GenInl]
   | GenCde Text Text
   | GenPgr [GenInl]
   deriving (Show, Eq)

endBlk :: GenPar ()
endBlk = void eol <|> eof

genBlk :: GenPar GenBlk
genBlk = choice [genCde, genHdg, genLst, genPgr]
 where
   -- parse & get the level
   genLvl c = T.length <$> takeWhile1P (Just "block marker") (== c) <* hspace1
   -- parse inlines
   genInls = some genInl
   genHdg = GenHdg <$> genLvl '|' <*> (genInls <* endBlk)
   genLst = GenLst <$> genLvl '-' <*> (genInls <* endBlk)
   genCde = do
      _   <- string ">>=" *> hspace1
      lng <- takeWhileP (Just "language") (/= '\n') <* eol
      bod <- T.pack <$> manyTill anySingle (try (eol *> string ">>=" *> endBlk))
      pure (GenCde lng bod)
   genPgr = GenPgr <$> (some genInl <* endBlk)

-- pure runner
parseGen :: GenPar a -> FilePath -> Text -> Either String a
parseGen p name input =
   first errorBundlePretty (runParser p name input)

-- whole document parser
genDoc :: GenPar [GenBlk]
genDoc = skipMany eol *> many (genBlk <* skipMany eol) <* eof

-- sample test document generated by Sonnet-4.6
sampleDoc :: Text
sampleDoc = T.unlines
   [ "| on the nature of minimal markup"
   , "|| *simplicity as design principle*"
   , "||| [stoa at a glance](https://example.com/stoa)"
   , "| a *grounded* approach to syntax"
   , "|| read [the rationale](https://example.com/why) before anything else"
   , "||| *elegance* from [constraint](https://example.com/c)"
   , "| plain text, *a claim*, and [evidence](https://example.com/e) together"
   , ""
   , "markup begins in plain prose."
   , "*only emphasis*, without ornamentation."
   , "[the full story](https://example.com/s) lives elsewhere."
   , "a parser without *backtracking* finds edge cases."
   , "combining [megaparsec](https://example.com/mp) with plain text works."
   , "*bold first*, then [a link](https://example.com/bl) follows."
   , "text, *emphasis*, and [a reference](https://example.com/r) in sequence."
   , ""
   , "- a plain list entry"
   , "  an indented list"
   , "-- *an emphasized entry*"
   , "--- [a linked entry](https://example.com/li)"
   , "- text with *emphasis* inline"
   , "-- plain plus [a link](https://example.com/l2) after"
   , "--- *bold* before [anchor](https://example.com/l3)"
   , "- text, *bold*, and [link](https://example.com/all) combined"
   , ""
   , ">>= haskell"
   , "genInl :: GenPar GenInl"
   , "genInl = choice [genBld, genLnk, genTxt]"
   , ">>="
   , ""
   , ">>= sh"
   , "echo \"a minimal invocation\""
   , ">>="
   ]

-- test values returning parsed results
testGenDoc :: IO ()
testGenDoc = do
  putStrLn . T.unpack $ ppResult $ parseGen genDoc "<document>" sampleDoc


-- ``````````````````````````````````````````````````````````````````````````````````````````
-- λ> testGenDoc
-- Hdg 1  "on the nature of minimal markup"
-- Hdg 2  *simplicity as design principle*
-- Hdg 3  [stoa at a glance](https://example.com/stoa)
-- Hdg 1  "a " *grounded* " approach to syntax"
-- Hdg 2  "read " [the rationale](https://example.com/why) " before anything else"
-- Hdg 3  *elegance* " from " [constraint](https://example.com/c)
-- Hdg 1  "plain text, " *a claim* ", and " [evidence](https://example.com/e) " together"
-- Par    "markup begins in plain prose."
-- Par    *only emphasis* ", without ornamentation."
-- Par    [the full story](https://example.com/s) " lives elsewhere."
-- Par    "a parser without " *backtracking* " finds edge cases."
-- Par    "combining " [megaparsec](https://example.com/mp) " with plain text works."
-- Par    *bold first* ", then " [a link](https://example.com/bl) " follows."
-- Par    "text, " *emphasis* ", and " [a reference](https://example.com/r) " in sequence."
-- Lst 1  "a plain list entry"
-- Lst 2  *an emphasized entry*
-- Lst 3  [a linked entry](https://example.com/li)
-- Lst 1  "text with " *emphasis* " inline"
-- Lst 2  "plain plus " [a link](https://example.com/l2) " after"
-- Lst 3  *bold* " before " [anchor](https://example.com/l3)
-- Lst 1  "text, " *bold* ", and " [link](https://example.com/all) " combined"
-- Cod haskell
--    genInl :: GenPar GenInl
--    genInl = choice [genBld, genLnk, genTxt]

-- Cod sh
--    echo "a minimal invocation"-
-- ``````````````````````````````````````````````````````````````````````````````````````````

-- ||| Pretty-printer generated by Sonnet-4.6

indent :: Int -> Text -> Text
indent n =
  T.unlines
    . map (T.replicate n " " <>)
    . T.lines

quote :: Text -> Text
quote t = "\"" <> escape t <> "\""
  where
    escape =
      T.concatMap $ \c -> case c of
        '"'  -> "\\\""
        '\\' -> "\\\\"
        '\n' -> "\\n"
        '\t' -> "\\t"
        _    -> T.singleton c

ppInl :: GenInl -> Text
ppInl = \case
  GenTxt t       -> quote t
  GenBld t       -> "*" <> t <> "*"
  GenLnk lbl url -> "[" <> lbl <> "](" <> url <> ")"

ppInls :: [GenInl] -> Text
ppInls = T.unwords . map ppInl

ppBlk :: GenBlk -> Text
ppBlk = \case
  GenHdg lvl inls -> "Hdg " <> T.pack (show lvl) <> "  " <> ppInls inls
  GenLst lvl inls -> "Lst " <> T.pack (show lvl) <> "  " <> ppInls inls
  GenPgr inls     -> "Par    " <> ppInls inls
  GenCde lng bod  -> "Cod " <> lng <> "\n" <> indent 3 bod

ppDoc :: [GenBlk] -> Text
ppDoc = T.unlines . map ppBlk

ppResult :: Either String [GenBlk] -> Text
ppResult = \case
  Left err   -> T.pack err
  Right blks -> ppDoc blks

-- Essentially, the five elements above are already sufficient for a decent markup language. The design of agora will start from this.
--
-- || From markup language to literate programming
--
-- In most of my blogs, I would be using code for better illustration. And i need to verify the code compiles. If we are in a markup
-- file (.stoa), we need to implement another scanner that watch for file change, extract all code blocks, & pass to the actual compiler
-- for that language, which is inefficient.
--
-- I always write the prose & the code at the same time. And I always need to make sure the code compile.
--
-- || Interlude: an aching depart from lisp markup
--
-- is lisp-markup readable (for me)? i'm indeed a lisp fan.
--
-- ||| Parser comparison
--
-- What if represent the minimal markup (minimal stoa) above as lisp markup? (cl-who https://edicl.github.io/cl-who/)
--
-- TODO: use some parser here to implement a parser

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
opt = info (helper <*> prs) (fullDesc <> header "site - static site generator")
 where
   prs = Opt
      <$> argument str (metavar "SRC_DIR"  <> help "source directory")
      <*> argument str (metavar "OUT_DIR"  <> help "output directory")
      <*> argument str (metavar "BASE_URL" <> help "base URL for the site")

-- main :: IO ()
-- main = do
--    o <- execParser opt
--    bld (optSrc o) (optOut o) (optUrl o)

-- | Reflexions
