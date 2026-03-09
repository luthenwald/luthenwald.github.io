-- title       = Agora: A minimal ssg after eleven months crawling
-- pubDate     = 2026-03-06
-- tags        = haskell, parser, 2026
-- description = Ideally, we can produce the best work in the best environment. Thus I determined to build the perfect static site
--               generator based on my preferences. It takes eleven months to reach version 1.0.0. This is the story/design behind
--               agora the ssg.

-- So we can use string literals as Text values
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad        ( void )

import           Data.Text            ( Text )
import qualified Data.Text            as T
import           Data.Void            ( Void )

import           Options.Applicative  ( ParserInfo, argument, fullDesc, header,
                                        help, helper, info, metavar, str )

import           Text.Megaparsec      ( Parsec, anySingle, between, choice,
                                       eof, many, manyTill, parseMaybe, skipMany,
                                       some, takeWhile1P,
                                       takeWhileP, (<|>) )
import           Text.Megaparsec.Char ( char, eol, hspace1, newline, string )

-- | The crawling to ἀγορά 1.0.0
--
-- > Please note that *Agora* is absolutely opinionated & have zero configuration support. Unless we share the same appreciation, it
--   would be confusing/inefficient for you to use.
--
-- In my search for the perfect ssg (static site generator), only [Bagatto](https://bagatto.co/) truly caught my eye. It
-- self-describes as a *transparent, extensible ssg*, plus the (most) minimal & elegant frontpage, I appreciated Bagatto immediately.
--
-- But yet still, i'm searching for a mechanism that fits my needs exactly. Rather than extensibility/flexibility, i prefer a
-- more compact/accurate one that aligns with my ideomatic preferences. I believe that only in an absolutely customized/tailored
-- environment, I shall create stuff of quality.
--
-- I (naïvely) thought a month is more than enough to formalize this ssg. I started the prototyping phase in april.2025. The first
-- blog was published in july. A month later my mind totally shifted, completely erased the existence of it & began to rebuild it
-- from scratch, but in a completely opposite manner. I temporarily leaped to lisp markup but soon transited to literate programming.
-- The first blog was then published in november. Another four months later, I now consider the ssg stable. In other words, this is
-- how i crafted ἀγορά, the ssg that does exactly what i want.
--
-- | Genesis of στοά: a minimal markup language
--
-- > (Un)fortunately, the revision of this version of στοά is completely lost.
--
-- The first attemp to actually build my own ssg began in april.2025. It is written in NimLang, named *stoa/στοά*. To be precisely,
-- στοά is the surface markup language for ἀγορά, *stoae/στοές* (the plural of stoa) is the directory/foundation where στοά files are
-- located, and *stoac* is the compiler that compiles στοές into the *polis/πόλις/website*. Other terms/aliases are also derived
-- based on the central greek word στοά.
--
-- || Why not markdown
--
-- Its more of an aesthetic matter. For example, I find `|` is prettier than `#` as the delimiter of headings. As I've mentioned
-- earlier, it's more probable to create something of value when using a markup language/scheme most comfortable with. Thus if i
-- consider `|` prettier than `#`, then this single deviation is sufficient for me to design another markup language. And then i'm
-- free to use any delimiter, keep an ideomatic set of markup kinds.
--
-- We can already describe it as a tiny language & implement a parser for it. We use `Gen*` names so this genesis vocabulary stays
-- distinct from the later, formal definitions of στοά.
--
-- We will start from an empty set of markup kind. The first addition rule is simple: a markup is added only when i necessarily need
-- it, and it should be given the most minimal & preferable delimiters.
--
-- The foundational form of στοά is *lexis/λέξις/inline*, which is a sum of plain text and all inline markup kinds (e.g. bold, link,
-- etc.).

data GenInl
   = GenTxt Text
   | GenBld Text
   | GenLnk Text Text    -- label, link
   deriving (Show, Eq)

-- The next level is *meros/μέρος/block*, which is a sum of plain paragraph and all block markup kinds constituted by inlines (e.g.
-- heading, list, etc.) or raw text (e.g. code block).

data GenBlk
   = GenPgr [GenInl]
   | GenHdg Int [GenInl]   -- level, content
   | GenLst Int [GenInl]   -- level, content
   | GenCde Text Text      -- lang,  content
   deriving (Show, Eq)

-- We define a tiny parser for this data model with [megaparsec](https://hackage.haskell.org/package/megaparsec).

type GenPar = Parsec Void Text

-- Note that `megaparsec` does not go back automatically.
-- Taken this example from [Megaparsec tutorial](https://markkarpov.com/tutorial/megaparsec.html#controlling-backtracking-with-try)

alternatives :: GenPar (Char, Char)
alternatives = foo <|> bar
 where
   foo = (,) <$> char 'a' <*> char 'b'
   bar = (,) <$> char 'a' <*> char 'c'

-- This works
--
-- ````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````
-- λ> parseTest alternatives "ab"
-- ('a','b')
-- ````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````
--
-- But this doesn't
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
-- I'll just paste the explanation from the tutorial:
--
-- > What happens here is that char 'a' part of `foo` (which is tried first) succeeded and consumed an a from the input stream. char
--   'b' then failed to match against 'c' and so we ended up with this error. An important detail here is that `(<|>)` did not even try
--   `bar` because `foo` has consumed some input
--
-- Albeit `megaparsec` do support backtracking with `try`, but this would slow down the parsing process. I take this as a very
-- intriguing/important point & will adopt it into the delimiter rule as: any inline/block markup should have the unique starting
-- delimiter. That is, we can't delimit bold with `**` & italic with `*` (as markdown) because they share the same starting delimiter
-- `*`. Instead we can delimit bold with `*` & italic with `/` (as [neorg](https://github.com/nvim-neorg/neorg)).
--
-- We can start to build the toy parser based on the design principle & delimiter rules as described above.
--
-- The parser for inline is to choose between bold, link or plain text.

genInl :: GenPar GenInl
genInl = choice [genBld, genLnk, genTxt]
 where
   -- *text*, take all characters between `*` and `*` till meeting `*` or the end of line
   genBld = GenBld <$> between (char '*') (char '*') (takeWhile1P (Just "bold text") (`notElem` ("*\n" :: String)))
   -- [label](url), take all characters between `[` and `](` as label, then take all characters till `)` as link
   genLnk =
     GenLnk
      <$> between (char '[') (string "](") (takeWhile1P (Just "link label") (`notElem` ("]\n" :: String)))
      <*> takeWhile1P (Just "link URL") (`notElem` (")\n" :: String)) <* char ')'
   -- plain text, take all characters till meeting `*`, `[` or end of line
   genTxt = GenTxt <$> takeWhile1P (Just "plain text") (`notElem` ("*[\n" :: String))

-- Now we continue with blocks. For simplicity, we won't implement indented lists in this article, but practically stoa accept
-- indented blocks (so we can span a block to multiple lines). A block terminates at either end of line or end of file. We factor
-- this boundary into `endBlk` so the block parsers don't repeat it.

endBlk :: GenPar ()
endBlk = void eol <|> eof

-- The block parser is a `choice` among four kinds, with `genPgr` as the unconditional fallback.
--
-- The code block delimiter starts with `@` followed by the language name on a
-- line of its own (e.g. `@haskell`) and closes with `@end` on a line of its
-- own. The body is treated as raw text between these markers.

genBlk :: GenPar GenBlk
genBlk = choice [genCde, genHdg, genLst, genPgr]
 where
   -- counts consecutive occurrences of the delimiter as level
   genLvl c = T.length <$> takeWhile1P (Just "block marker") (== c) <* hspace1
   -- parse one or more inlines
   genInls = some genInl
   -- `| inline`, parse level and inlines till end of block
   genHdg = GenHdg <$> genLvl '|' <*> (genInls <* endBlk)
   -- `- unordered-list`, parse level and inlines till end of block
   genLst = GenLst <$> genLvl '-' <*> (genInls <* endBlk)
   -- `@lang`
   -- `lines of raw text`
   -- `@end`
   -- consume the starting `@`, parse the language label, then collect raw text
   -- till consuming the closing `@end` marker.
   genCde = do
      _   <- char '@'
      lng <- takeWhileP (Just "language") (/= '\n') <* eol
      bod <- T.pack <$> manyTill anySingle (string "@end" *> endBlk)
      pure (GenCde lng bod)
   genPgr = GenPgr <$> (some genInl <* endBlk)

-- We now define a simple test suite for this parser.

-- sample test document generated by Sonnet-4.6
sampleGenDoc :: Text
sampleGenDoc = T.unlines
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
   , "-- *an emphasized entry*"
   , "--- [a linked entry](https://example.com/li)"
   , "- text with *emphasis* inline"
   , "-- plain plus [a link](https://example.com/l2) after"
   , "--- *bold* before [anchor](https://example.com/l3)"
   , "- text, *bold*, and [link](https://example.com/all) combined"
   , ""
   , "@haskell"
   , "genInl :: GenPar GenInl"
   , "genInl = choice [genBld, genLnk, genTxt]"
   , "@end"
   , ""
   , "@sh"
   , "echo \"a minimal invocation\""
   , "@end"
   ]

testGenDoc :: IO ()
testGenDoc =
 case parseMaybe genDoc sampleGenDoc of
   Just blks -> putStrLn . T.unpack $ ppDoc blks
   Nothing   -> putStrLn "parse failed"
 where
   genDoc = many (genBlk <* skipMany eol) <* eof

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
--
-- Cod sh
--    echo "a minimal invocation"-
-- ``````````````````````````````````````````````````````````````````````````````````````````

-- ||| Pretty-printer generated by Sonnet-4.6
--
-- > You can safely skip this section.

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

-- Essentially, the elements above are already sufficient for a workable (but not decent) markup language. The design of agora will
-- start from this.
--
-- | A leap to literate programming
--
-- Most of my blogs are code-heavy and I need to make sure all the code compile. When we are in a markup file, e.g. `foo.stoa`, we
-- need to implement another scanner that (optionally) watches for file change, extracts all code blocks & passes them to the actual
-- compiler for that language (e.g. ghc). There're quite a lot flaws in this design. For example, the compiler errors would be harder
-- to deduce: line numbers in error report doesn't match its actual one in `foo.stoa`. The workflow would be further complicated if
-- we want to support formatters, linters, etc.
--
-- All these sorts of troubles lead me to another option, i.e. literate programming. Instead of working in `foo.stoa`, we work in
-- `foo.hs`/`foo.rkt` (if we are writing a haskell/racket prose). Generally we want a invariant scheme that is language-independent,
-- or it's logically same to write in haskell, racket, rust or any programming language.
--
-- Out of this language-independent concern, we can update the mechanism of stoac as:
-- - code blocks are removed from stoa and become primitive elements
-- - stoa prose is written in standalone single-line comments
-- - a prose comment is standalone only when the line above and below are empty or comment lines
-- - other comments stay as code comments and are not parsed as stoa prose
-- - the full source file must compile with host language's standard compiler
--
-- || Reimplement stoac
--
-- Now we reimplement the data models & corresponding parsers. Unsurprisingly, the parser for inlines are exactly the same
-- as the genesis.

data LitInl
   = LitTxt Text
   | LitBld Text
   | LitLnk Text Text
   deriving (Show, Eq)

litInl :: GenPar LitInl
litInl = choice [litBld, litLnk, litTxt]
 where
   litBld = LitBld <$> between (char '*') (char '*') (takeWhile1P (Just "bold text") (`notElem` ("*\n" :: String)))
   litLnk =
     LitLnk
      <$> between (char '[') (string "](") (takeWhile1P (Just "link label") (`notElem` ("]\n" :: String)))
      <*> takeWhile1P (Just "link URL") (`notElem` (")\n" :: String)) <* char ')'
   litTxt = LitTxt <$> takeWhile1P (Just "plain text") (`notElem` ("*[\n" :: String))

-- The data model of blocks & parser for the prose block also stay invariant.

data LitBlk
   = LitHdg Int [LitInl]
   | LitLst Int [LitInl]
   | LitPgr [LitInl]
   | LitCde Text
   deriving (Show, Eq)

-- The only difference is we are removing the choice of code block.

litPrsBlk :: GenPar LitBlk
litPrsBlk = choice [litHdg, litLst, litPgr]
 where
   litLvl c = T.length <$> takeWhile1P (Just "block marker") (== c) <* hspace1
   litInls  = some litInl
   litHdg   = LitHdg <$> litLvl '|' <*> (litInls <* eof)
   litLst   = LitLst <$> litLvl '-' <*> (litInls <* eof)
   litPgr   = LitPgr <$> (litInls <* eof)

-- We presume ourselves in a haskell file, so single line comment is prefixed with `--`. Ideally, the value of `singleLineComment`
-- should read from a hashmap from language name to the single line comment in that lang.
--
-- Now we can check whether a comment is prose comment or not.

isEmp :: Text -> Bool
isEmp = T.null . T.strip

isCmt :: Text -> Bool
isCmt = T.isPrefixOf "--" . T.stripStart

isPrs :: Maybe Text -> Text -> Maybe Text -> Bool
isPrs prv cur nxt =
 isCmt cur
   && maybe True (\ln -> isEmp ln || isCmt ln) prv
   && maybe True (\ln -> isEmp ln || isCmt ln) nxt

-- We then strip & parse the prose comments.

-- strip the prose comment
strCmt :: Text -> Text
strCmt ln =
 if isCmt ln
    then T.dropWhile (== ' ') . T.drop 2 . T.stripStart $ ln
    else ln

prsLne :: Text -> [LitBlk]
prsLne ln =
  case T.strip bod of
    "" -> []
    _  -> [maybe (LitPgr [LitTxt bod]) id (parseMaybe litPrsBlk bod)]
 where
   bod = strCmt ln

-- We can now try to parse the whole file.

-- transform lines of code into a single LitCde
toCde :: [Text] -> [LitBlk]
toCde lns =
 case T.intercalate "\n" lns of
   ""  -> []
   txt -> [LitCde txt]

-- lex through the whole file line by line recognize code/prose blocks and parse them
litFromLns :: [Text] -> [LitBlk]
litFromLns lns = go trip [] []
 where
   prv = Nothing : map Just lns
   nxt = map Just (drop 1 lns) <> [Nothing]
   trip = zip3 prv lns nxt

   go [] acc cde = acc <> toCde cde
   go ((p, l, n) : rst) acc cde
     | isPrs p l n = go rst (acc <> toCde cde <> prsLne l) []
     | isEmp l =
       case cde of
         [] -> go rst acc []
         _  -> go rst acc (cde <> [l])
     | otherwise = go rst acc (cde <> [l])

litDoc :: GenPar [LitBlk]
litDoc = litFromLns . T.lines <$> takeWhileP (Just "source text") (const True)

testLitDoc :: IO ()
testLitDoc =
  case parseMaybe (litDoc <* eof) sampleLitDoc of
    Just blks -> putStrLn . T.unpack $ ppLitDoc blks
    Nothing   -> putStrLn "parse failed"

sampleLitDoc :: Text
sampleLitDoc = T.unlines
   [ "-- | A tiny literate article"
   , "--"
   , "-- This paragraph has *bold* and [a link](https://example.com)."
   , "-- - item one"
   , "-- -- nested *item*"
   , ""
   , "inc :: Int -> Int"
   , "inc x = x + 1"
   , "-- after inc"
   , ""
   , "-- | Another section"
   , "-- prose after code."
   , ""
   , "-- before msg"
   , "msg :: Text"
   , "msg = \"hello\""
   ]

ppLitInl :: LitInl -> Text
ppLitInl = \case
  LitTxt txt     -> quote txt
  LitBld txt     -> "*" <> txt <> "*"
  LitLnk lbl url -> "[" <> lbl <> "](" <> url <> ")"

ppLitInls :: [LitInl] -> Text
ppLitInls = T.unwords . map ppLitInl

ppLitBlk :: LitBlk -> Text
ppLitBlk = \case
  LitHdg lvl inls -> "Hdg " <> T.pack (show lvl) <> "  " <> ppLitInls inls
  LitLst lvl inls -> "Lst " <> T.pack (show lvl) <> "  " <> ppLitInls inls
  LitPgr inls     -> "Par    " <> ppLitInls inls
  LitCde txt      -> "Cod\n" <> indent 3 txt

ppLitDoc :: [LitBlk] -> Text
ppLitDoc = T.unlines . map ppLitBlk

-- ````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````
-- λ> testLitDoc
-- Hdg 1  "A tiny literate article"
-- Par    "This paragraph has " *bold* " and " [a link](https://example.com) "."
-- Lst 1  "item one"
-- Lst 2  "nested " *item*
-- Cod
--    inc :: Int -> Int
--    inc x = x + 1
--    -- after inc

-- Hdg 1  "Another section"
-- Par    "prose after code."
-- Cod
--    -- before msg
--    msg :: Text
--    msg = "hello"
-- ````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````

-- | Interlude: an aching depart from lisp markup
--
-- is lisp-markup readable (for me)? i'm indeed a lisp fan.
--
-- || Parser comparison
--
-- What if represent the minimal markup (minimal stoa) above as lisp markup? (cl-who https://edicl.github.io/cl-who/)
--
-- TODO: use some parser here to implement a parser

-- || Lisp markup can be easily extended
--
-- || Lisp markup strays from my principle
--
-- The simplicity of a lisp markup compiler tempts me to add more features in the markup. It starts from `class-id`, but then more
-- and more combinators are added, to which extent, the complexity of the language has deviated from my primitive purpose of building
-- a minimal ssg.
--
-- | The hierarchy/anatomy of agora
--
-- | Stoa the markup language
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

-- | Polis the generated site
--
-- || The law of 1.44
--
-- || Tufte css but no sidenotes
--
-- | Stoac the compiler (might need a better name)
--
-- || Data models
--
-- || Parsers
--
-- || Scanner & Builder
--
-- | Implementing
--
-- We will use a minimal & ideomatic dependencies to save some locs.
--
-- || Syntax highlighting
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
