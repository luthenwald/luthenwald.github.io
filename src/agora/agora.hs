-- title       = Agora: SSG after eleven months' crawling
-- pubDate     = 2026-03-11
-- tags        = haskell, parser, SSG, 2026
-- description = Agora is a personal static site generator engineered to sustain an ideal writing environment. This essay traces
--               eleven months of architectural evolution, moving through genesis stoa markup and S-expression inspired markup
--               iterations before settling on a final literate programming design.

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad           ( void, when )

import           Data.Char               ( isAlphaNum, toLower )
import           Data.List               ( sortBy )
import           Data.Map.Strict         ( Map )
import qualified Data.Map.Strict         as Map
import           Data.Ord                ( Down (..), comparing )
import           Data.Text               ( Text )
import qualified Data.Text               as T
import qualified Data.Text.IO            as TIO
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TLE
import           Data.Void               ( Void )

import           Options.Applicative     ( ParserInfo, argument, execParser,
                                           fullDesc, header, help, helper,
                                           info, metavar, str )

import           System.Directory        ( createDirectoryIfMissing,
                                           doesDirectoryExist, listDirectory )
import           System.Exit             ( ExitCode (..) )
import           System.FilePath         ( takeBaseName, takeExtension, (</>) )
import           System.Process.Typed    ( proc, readProcess )

import qualified Text.Megaparsec         as M
import           Text.Megaparsec         ( Parsec, anySingle, between, choice,
                                           eof, many, manyTill, optional,
                                           parse, parseMaybe, satisfy,
                                           skipMany, some, takeWhile1P,
                                           takeWhileP, try, (<|>) )
import           Text.Megaparsec.Char    ( char, eol, hspace1, newline,
                                           string )
import           Text.Read               ( readMaybe )

-- | The crawling to ἀγορά 1.0.0
--
-- > Please note that ἀγορά is absolutely opinionated & have zero configuration support. Unless we share the same appreciation, it
--   would be confusing/inefficient for you to use.
--
-- In my research for the perfect static site generator (SSG), only [Bagatto](https://bagatto.co/) truly caught my eye. It
-- self-describes as a *transparent, extensible SSG*, plus the (most) minimal & elegant frontpage, I appreciated Bagatto immediately.
--
-- But yet still, i'm searching for a mechanism that fits my needs exactly. Rather than extensibility/flexibility, i prefer a
-- more compact/accurate one that aligns with my ideomatic preferences. I believe that only in an absolutely customized/tailored
-- environment, I shall create stuff of quality.
--
-- I (naïvely) thought a month is more than enough to formalize this SSG. I started the prototyping phase in april.2025. The first
-- blog was published in july. A month later my mind totally shifted, completely erased the existence of it & began to rebuild it
-- from scratch, but in a completely opposite manner. I temporarily leaped to lisp markup but soon transited to literate programming.
-- The first blog was then published in november. Another four months later, I now consider the SSG stable. In other words, this is
-- how i crafted ἀγορά, the SSG that does exactly what i want.
--
-- | Genesis of στοά: a minimal markup language
--
-- > (Un)fortunately, the revision of this version of ἀγορά is completely lost.
--
-- The first attempt to actually build my own SSG began in april.2025. It was written in [NimLang](https://nim-lang.org/), named
-- *stoa/στοά*. To be precise, στοά is the surface markup language for ἀγορά. The directory where στοά files are located is called
-- *stoae/στοές* (the plural of stoa), and *stoac* is the compiler that transforms στοές into the *polis/πόλις* (the website). Other
-- aliases follow this central Greek theme.
--
-- || Why not markdown
--
-- It is fundamentally an aesthetic matter. For example, I find `|` visually prettier than `#` as the delimiter for headings. As I've
-- mentioned earlier, I believe we are more likely to create something of value when operating in a medium we find most comfortable
-- with. If I consider `|` prettier than `#`, this single aesthetic deviation is sufficient justification to design an entirely new
-- markup language. From there, I am free to redefine every delimiter and keep a strictly ideomatic set of markup types.
--
-- Let's construct a tiny implementation to capture this initial design. We will use the `Gen*` prefix so this
-- [genesis](https://youtu.be/hbl9m9uJjOU?si=x0p804mckY_E074F) vocabulary stays distinct from the later formal definitions.
--
-- || Data models for genesis markup
--
-- We start from an empty set of markup kinds. The foundational rule is strict minimalism: a markup is added only when absolutely
-- necessary, and it must be given the most preferable, minimal delimiters.
--
-- The lowest level of στοά is *lexis/λέξις* (inline). An inline is either plain text or a decorated text element (like bold or a
-- link). We define the AST for inlines as a simple sum type.

data GenInl
   = GenTxt Text
   | GenBld Text
   | GenLnk Text Text    -- label, link
   deriving (Show, Eq)

-- The next structural level is *meros/μέρος* (block). A block is a discrete paragraph, heading, list, or code snippet. Blocks are
-- generally composed of an array of inlines, except for code blocks which preserve their content as raw, unparsed text.

data GenBlk
   = GenPgr [GenInl]
   | GenHdg Int [GenInl]   -- level, content
   | GenLst Int [GenInl]   -- level, content
   | GenCde Text Text      -- lang,  content
   deriving (Show, Eq)

-- || Parsing without backtracking
--
-- We use [megaparsec](https://hackage.haskell.org/package/megaparsec) to build our parser.

type GenPar = Parsec Void Text

-- By default, `megaparsec` [does not backtrack automatically](https://markkarpov.com/tutorial/megaparsec.html#controlling-backtracking-with-try).
-- If a parser consumes any input before failing, alternative branches (`<|>`) are not tried. While you can force backtracking using
-- the `try` combinator, this would slow down the parsing process by keeping arbitrary input in memory.
--
-- We can treat this limitation as another design constraint: every inline and block markup must possess a globally unique starting
-- delimiter.
--
-- For instance, Markdown delimits bold with `**` and italic with `*`. A parser seeing an `*` doesn't immediately know which branch
-- to commit to, which requires backtracking. In στοά, we avoid this entirely: delimit bold with `*` and italic with `/` (similar
-- to [Neorg](https://github.com/nvim-neorg/neorg)). Because the first character strictly defines the node type, our parser becomes
-- incredibly fast and structurally predictable.
--
-- With this zero-backtracking rule in hand, we can implement the lexer/parser.
--
--
-- `genInl` is the root inline parser. It attempts to parse bold text, a link, or defaults to plain text. The `choice` combinator
-- evaluates these in order without backtracking, which is safe because `*`, `[`, and plain characters do not overlap.

genInl :: GenPar GenInl
genInl = choice [genBld, genLnk, genTxt]
 where
   -- `genBld` expects an opening `*`, reads characters until a newline or closing `*`,
   -- and consumes the closing `*`.
   genBld = GenBld <$> between (char '*') (char '*') (takeWhile1P (Just "bold text") (`notElem` ("*\n" :: String)))

   -- `genLnk` parses `[label](url)`. It leverages `between` for the label, expecting the exact literal `](` as the bridge,
   -- then parses the URL until the closing `)`.
   genLnk =
     GenLnk
      <$> between (char '[') (string "](") (takeWhile1P (Just "link label") (`notElem` ("]\n" :: String)))
      <*> takeWhile1P (Just "link URL") (`notElem` (")\n" :: String)) <* char ')'

   -- `genTxt` is the fallback inline element.
   -- It greedily consumes any character that is not a newline or the start of another inline markup (`*`, `[`).
   genTxt = GenTxt <$> takeWhile1P (Just "plain text") (`notElem` ("*[\n" :: String))

-- || Parsing block elements
--
-- For simplicity, we *won't implement indented blocks* here, meaning blocks will always span a single physical line (except for raw
-- code blocks). A block logically terminates at either a newline or the end of the file. We factor this logic into `endBlk` so the
-- block parsers don't repeat it.

endBlk :: GenPar ()
endBlk = void eol <|> eof

-- The block lexer evaluates four distinct block types. Since paragraphs do not have a distinct leading symbol, `genPgr` acts as the
-- unconditional fallback at the end of `choice`.

genBlk :: GenPar GenBlk
genBlk = choice [genCde, genHdg, genLst, genPgr]
 where
   -- `genLvl` counts consecutive occurrences of a delimiter character (e.g., `|||`)
   -- and uses its length as the integer "level", consuming trailing horizontal whitespace before the content.
   genLvl c = T.length <$> takeWhile1P (Just "block marker") (== c) <* hspace1

   -- `genInls` applies `genInl` one or more times to construct a block's content array.
   genInls = some genInl

   -- `genHdg` constructs a heading. It reads the level from `|`, followed by parsed inlines.
   genHdg = GenHdg <$> genLvl '|' <*> (genInls <* endBlk)

   -- `genLst` constructs a list item. It reads the level from `-`, followed by parsed inlines.
   genLst = GenLst <$> genLvl '-' <*> (genInls <* endBlk)

   -- `genCde` handles raw code blocks. The code block delimiter is `@` followed by the language string,
   -- terminated by a standalone `@end`. It consumes the language line,
   -- then greedily collects raw character data (`manyTill anySingle`) until the closing `@end` is reached.
   genCde = do
      _   <- char '@'
      lng <- takeWhileP (Just "language") (/= '\n') <* eol
      bod <- T.pack <$> manyTill anySingle (string "@end" *> endBlk)
      pure (GenCde lng bod)

   -- `genPgr` is the fallback paragraph block.
   -- It simply accumulates parsed inline elements until the end of the line.
   genPgr = GenPgr <$> (some genInl <* endBlk)

-- || Testing the genesis parser
--
-- We can now define a simple test suite to ensure our minimal genesis parser functions correctly.

-- sample test document generated by Gemini-3.1
sampleGenDoc :: Text
sampleGenDoc = T.unlines
   [ "| Syntax and Semantics"
   , "|| *The Role of Delimiters*"
   , "||| [A minimal case study](https://example.com/study)"
   , ""
   , "The foundation of any markup language relies on plain text, followed by *syntactic forms*, and finally [references](https://example.com/ref) to external domains."
   , ""
   , "- Structural boundaries"
   , "-- *Lexical analysis*"
   , "--- [Abstract syntax trees](https://example.com/ast)"
   , ""
   , "@haskell"
   , "genInl :: GenPar GenInl"
   , "genInl = choice [genBld, genLnk, genTxt]"
   , "@end"
   , ""
   , "@sh"
   , "echo \"a minimal invocation\""
   , "echo done"
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
-- Hdg 1  "Syntax and Semantics"
-- Hdg 2  *The Role of Delimiters*
-- Hdg 3  [A minimal case study](https://example.com/study)
-- Par    "The foundation of any markup language relies on plain text, followed by " *syntactic forms* ", and finally " [references](https://example.com/ref) " to external domains."
-- Lst 1  "Structural boundaries"
-- Lst 2  *Lexical analysis*
-- Lst 3  [Abstract syntax trees](https://example.com/ast)
-- Cod haskell
--    genInl :: GenPar GenInl
--    genInl = choice [genBld, genLnk, genTxt]
--
-- Cod sh
--    echo "a minimal invocation"
--    echo done
-- ``````````````````````````````````````````````````````````````````````````````````````````

-- || Pretty-printer generated by Sonnet-4.6
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

-- Essentially, the elements above are already sufficient for a workable (but not decent) markup language. The design of ἀγορά will
-- start from this.
--
-- | Interlude: An aching depart from S-Expression
--
-- Before settling down to literate programming, I once experimented with a completely different paradigm: modeling the markup as
-- an [S-expression](https://en.wikipedia.org/wiki/S-expression). The genesis design (which forbids backtracking by requiring unique
-- leading symbols) remains entirely valid here. If we replace surface symbols like `*` or `|` with Lisp-style forms, we can maintain
-- the same markup coverage while adopting a strictly nested, minimal syntax.
--
-- The transformation is straightforward. For inlines, plain text is wrapped in a basic group, while semantic markups use a keyword
-- tag like `:b` or `:a`.
--
-- - `(plain text)`
-- - `(:b bold text)`
-- - `(:a (label) (url))`
--
-- For blocks, we adopt a similar structure. A block starts with a keyword like `:p` (paragraph), `:h` (heading), or `:l` (list),
-- followed by its arguments or inline contents.
--
-- - `(:p <inline> ...)`
-- - `(:h <level> <inline> ...)`
-- - `(:l <level> <inline> ...)`
-- - `(:c lang (line 1) (  line 2))`
--
-- Notice how the `:c` form handles code blocks. Instead of quoted strings (which would be tedious to type and require escaping),
-- each code line sits in its own `(...)` group as raw text. This preserves leading spaces organically without introducing a
-- secondary delimiter scheme. Inline forms also use raw text with no quoted string syntax. Prefixing markup forms with `:` cleanly
-- avoids ambiguity against plain text groups.
--
-- Let's prototype this S-expression approach. We will use `Lis*` naming to keep this Lisp branch distinct from the earlier `Gen*`
-- and the upcoming `Lit*` models.
--
-- || Data models for Lisp markup
--
-- We define the AST using the same basic taxonomy: `LisInl` for inlines and `LisBlk` for blocks.

data LisInl
   = LisTxt Text
   | LisBld Text
   | LisLnk Text Text
   deriving (Show, Eq)

data LisBlk
   = LisPgr [LisInl]
   | LisHdg Int [LisInl]
   | LisLst Int [LisInl]
   | LisCde Text [Text]
   deriving (Show, Eq)

-- || Parsers for Lisp markup
--
-- To parse S-expressions easily, we first need a few foundational helpers. S-expressions are heavily dependent on whitespaces and
-- parentheses.
--
-- `lisWsp` consumes optional whitespace characters. It is used around parentheses where spacing doesn't affect semantics.

lisWsp :: GenPar ()
lisWsp = void $ many (char ' ' <|> char '\t' <|> char '\n' <|> char '\r')

-- `lisSep` consumes mandatory whitespace characters. It is used to separate the leading tag from the content (e.g. between `:h`
-- and `1`).

lisSep :: GenPar ()
lisSep = void $ some (char ' ' <|> char '\t' <|> char '\n' <|> char '\r')

-- `lisPrn` is a combinator that wraps another parser `p` in parentheses, absorbing any surrounding padding spaces.

lisPrn :: GenPar a -> GenPar a
lisPrn p = between (lisWsp *> char '(' <* lisWsp) (lisWsp *> char ')' <* lisWsp) p

-- Now we define how to extract raw text content from inside a group. `lisRaw` greedily consumes any character until it hits a
-- closing parenthesis. This effectively removes the need for quotes.

lisRaw :: GenPar Text
lisRaw = T.pack <$> many (satisfy (/= ')'))

-- `lisGrp` is a full plain text group parser. It expects an opening parenthesis, extracts the raw text via `lisRaw`, and consumes
-- the closing parenthesis.

lisGrp :: GenPar Text
lisGrp = lisWsp *> char '(' *> lisRaw <* char ')'

-- For things like heading/list levels, we need to parse integers. `lisInt` reads continuous digit characters and converts them to
-- an `Int`.

lisInt :: GenPar Int
lisInt = read . T.unpack <$> takeWhile1P (Just "integer") (`elem` ("0123456789" :: String))

-- The language label in a code block is parsed as an atom, which is any continuous sequence of non-whitespace, non-parenthesis
-- characters.

lisAtm :: GenPar Text
lisAtm =
   takeWhile1P
      (Just "atom")
      (\c -> c /= '(' && c /= ')' && c /= ' ' && c /= '\t' && c /= '\n' && c /= '\r')

-- `lisTag` is a simple helper to match explicit keywords (like `:h` or `:p`).

lisTag :: Text -> GenPar ()
lisTag t = void $ string t

-- With the helpers in place, we can parse inlines. We use `M.try` for `:b` and `:a` because if they fail to match the tag, we need
-- to backtrack and try parsing them as plain text `(...)` which has no tag.

lisInl :: GenPar LisInl
lisInl = choice [M.try lisBld, M.try lisLnk, lisTxt]
 where
   lisTxt = LisTxt <$> lisGrp
   lisBld = lisPrn $ LisBld <$> (lisTag ":b" *> lisSep *> lisRaw)
   lisLnk = lisPrn $ LisLnk <$> (lisTag ":a" *> lisSep *> lisGrp) <*> (lisSep *> lisGrp)

-- Code lines are just groups of raw text, exactly like plain text inlines.

lisCdeLne :: GenPar Text
lisCdeLne = lisGrp

-- Blocks follow the same tagged S-expression pattern. We try to match specific block tags (`:c`, `:h`, `:l`), and if none match, we
-- fall back to a paragraph (`:p`).

lisBlk :: GenPar LisBlk
lisBlk = choice [M.try lisCde, M.try lisHdg, M.try lisLst, lisPgr]
 where
   lisInls = some lisInl
   lisPgr = lisPrn $ LisPgr <$> (lisTag ":p" *> lisSep *> lisInls)
   lisHdg = lisPrn $ LisHdg <$> (lisTag ":h" *> lisSep *> lisInt) <*> (lisSep *> lisInls)
   lisLst = lisPrn $ LisLst <$> (lisTag ":l" *> lisSep *> lisInt) <*> (lisSep *> lisInls)
   lisCde = lisPrn $ LisCde <$> (lisTag ":c" *> lisSep *> lisAtm) <*> (lisSep *> some lisCdeLne)

-- A whole Lisp document is simply a sequence of blocks surrounded by optional whitespace.

lisDoc :: GenPar [LisBlk]
lisDoc = lisWsp *> many lisBlk <* lisWsp

-- || Testing the Lisp parser
--
-- We can now verify this approach with a comprehensive sample document.

sampleLisDoc :: Text
sampleLisDoc = T.unlines
   [ "(:h 1 (Syntax and Semantics))"
   , "(:h 2 (:b The Role of Delimiters))"
   , "(:h 3 (:a (A minimal case study) (https://example.com/study)))"
   , "(:p (The foundation of any markup language relies on plain text, followed by ) (:b syntactic forms) (, and finally ) (:a (references) (https://example.com/ref)) ( to external domains.))"
   , "(:l 1 (Structural boundaries))"
   , "(:l 2 (:b Lexical analysis))"
   , "(:l 3 (:a (Abstract syntax trees) (https://example.com/ast)))"
   , "(:c haskell (genInl :: GenPar GenInl) (genInl = choice [genBld, genLnk, genTxt]))"
   , "(:c sh (echo \"a minimal invocation\") (echo done))"
   ]

testLisDoc :: IO ()
testLisDoc =
   case parseMaybe (lisDoc <* eof) sampleLisDoc of
      Just blks -> putStrLn . T.unpack $ ppLisDoc blks
      Nothing   -> putStrLn "parse failed"

-- ``````````````````````````````````````````````````````````````````````````````````````````
-- λ> testLisDoc
-- Hdg 1  "Syntax and Semantics"
-- Hdg 2  *The Role of Delimiters*
-- Hdg 3  [A minimal case study](https://example.com/study)
-- Par    "The foundation of any markup language relies on plain text, followed by " *syntactic forms* ", and finally " [references](https://example.com/ref) " to external domains."
-- Lst 1  "Structural boundaries"
-- Lst 2  *Lexical analysis*
-- Lst 3  [Abstract syntax trees](https://example.com/ast)
-- Cod haskell
--    genInl :: GenPar GenInl
--    genInl = choice [genBld, genLnk, genTxt]
--
-- Cod sh
--    echo "a minimal invocation"
--    echo done
-- ``````````````````````````````````````````````````````````````````````````````````````````

ppLisInl :: LisInl -> Text
ppLisInl = \case
   LisTxt txt     -> quote txt
   LisBld txt     -> "*" <> txt <> "*"
   LisLnk lbl url -> "[" <> lbl <> "](" <> url <> ")"

ppLisInls :: [LisInl] -> Text
ppLisInls = T.unwords . map ppLisInl

ppLisBlk :: LisBlk -> Text
ppLisBlk = \case
   LisHdg lvl inls -> "Hdg "    <> T.pack (show lvl) <> "  " <> ppLisInls inls
   LisLst lvl inls -> "Lst "    <> T.pack (show lvl) <> "  " <> ppLisInls inls
   LisPgr inls     -> "Par    " <> ppLisInls inls
   LisCde lng lns  -> "Cod "    <> lng <> "\n" <> indent 3 (T.unlines lns)

ppLisDoc :: [LisBlk] -> Text
ppLisDoc = T.unlines . map ppLisBlk

-- || Why not lisp
--
-- In pure aesthetic terms, lisp markup can be strikingly beautiful. It surfaces the structural reality of the AST directly into
-- the text, eliminating ad-hoc delimiter collisions and escaping rules entirely. Adding a new markup tag is as trivial as picking a
-- new symbol.
--
-- But aesthetics and ergonomics are not always perfectly aligned. The cost of this uniform purity is raw friction in the act of
-- authoring.
--
-- Take a standard mixed-format sentence:
-- `(:p (The foundation of any markup language relies on plain text, followed by ) (:b syntactic forms) (, and finally ) (:a (references) (https://example.com/ref)) ( to external domains.))`
--
-- For a structural engineer or a parser, this is crystal. For a writer in flow, it's a nightmare. You're forced to aggressively
-- slice your continuous thoughts into explicitly segmented nodes, maintaining parenthesis balance manually. What used to be a simple
-- fluid string `plain text, *a claim*, and [evidence]...` becomes a cognitive load of opening, closing, and nesting groups.
--
-- Also, editing existing prose becomes brittle. Deleting a word that happens to cross a boundary forces you to restructure the
-- tree. If you want to bold a phrase that was previously part of a larger `(...)` block, you must fracture that block into three new
-- sibling elements.
--
-- So while Lisp markup solved our parsing rigidity and uniformity problems gracefully, the writing experience was unbearable for
-- long-form prose. This realization pushed me toward the final iteration of ἀγορά.

-- | A leap to literate programming
--
-- Most of my blogs are code-heavy, and it is strictly necessary to guarantee that all the embedded code correctly compiles. In the
-- traditional workflow where we write in a dedicated markup file (e.g., `foo.stoa`), we're forced to build tooling that extracts
-- code blocks and passes them to the underlying compiler (like `ghc`).
--
-- This creates massive friction: compiler errors map to the wrong line numbers, language servers fail to provide inline feedback,
-- formatters break, and editor syntax highlighting struggles.
--
-- This structural pain naturally led me to literate programming. Instead of writing markup that contains code, we write code that
-- contains markup. The source file is a completely valid `foo.hs` or `foo.rkt` file that natively compiles in its ecosystem. The
-- prose resides inside the native comments of that language.
--
-- To keep this parser language-agnostic, the core invariant rules are:
-- - Code blocks are no longer a markup construct.
-- - Prose is written exclusively inside standalone, single-line comments.
-- - A comment block is considered "prose" if it is contiguous and isolated (i.e., bounded by empty lines or other prose lines, but
--   never hugging code directly).
-- - Comments attached directly to code (like a type signature explanation) remain raw code.
--
-- Let's construct this final, stable version of ἀγορά using the `Lit*` prefix.
--
-- || Data models for literate markup
--
-- Because we are back in plain text comments, we can revert to the exact `Gen*` syntax rules (e.g., `*bold*`, `[label](url)`, `|
-- heading`) for our inline elements.

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

-- The block model is also identical to the genesis version, except the code block (`LitCde`) no longer takes a language string. The
-- entire file is assumed to be written in the host language (e.g., Haskell), so we just store the raw code text.

data LitBlk
   = LitHdg Int [LitInl]
   | LitLst Int [LitInl]
   | LitPgr [LitInl]
   | LitCde Text
   deriving (Show, Eq)

-- || Parsing literate blocks
--
-- The parser for the prose block is a simple choice among headings, lists, and paragraphs. Notice there is no code block parser
-- here, because code is handled completely outside the markup parsing logic.

litPrsBlk :: GenPar LitBlk
litPrsBlk = choice [litHdg, litLst, litPgr]
 where
   litLvl c = T.length <$> takeWhile1P (Just "block marker") (== c) <* hspace1
   litInls  = some litInl
   litHdg   = LitHdg <$> litLvl '|' <*> (litInls <* eof)
   litLst   = LitLst <$> litLvl '-' <*> (litInls <* eof)
   litPgr   = LitPgr <$> (litInls <* eof)

-- || Extracting prose from code
--
-- We presume ourselves in a Haskell file, so a single-line comment is prefixed with `--`. In a generalized engine, this prefix would
-- be dynamically loaded based on the file extension.
--
-- `isEmp` checks if a line is completely empty (or just spaces).

isEmp :: Text -> Bool
isEmp = T.null . T.strip

-- `isCmt` checks if a line starts with the host language's comment prefix.

isCmt :: Text -> Bool
isCmt = T.isPrefixOf "--" . T.stripStart

-- The heuristic for isolating prose from code documentation is adjacency. `isPrs` determines if the current comment line is prose
-- by checking its neighbors. A line is prose if it is a comment && both its previous and next lines are either empty lines or other
-- comments. If a comment touches code, it's treated as code (comment).

isPrs :: Maybe Text -> Text -> Maybe Text -> Bool
isPrs prv cur nxt =
 isCmt cur
   && maybe True (\ln -> isEmp ln || isCmt ln) prv
   && maybe True (\ln -> isEmp ln || isCmt ln) nxt

-- Once we've identified a line as prose, we need to extract its content. `strCmt` drops the comment prefix and the single mandatory
-- space following it.

strCmt :: Text -> Text
strCmt ln =
 if isCmt ln
    then T.dropWhile (== ' ') . T.drop 2 . T.stripStart $ ln
    else ln

-- `prsLne` applies our markup parser `litPrsBlk` to the stripped comment body. If the line is empty after stripping, it returns
-- nothing. If the parser fails (which shouldn't happen due to the `LitPgr` fallback), it defaults to a plain text paragraph.

prsLne :: Text -> [LitBlk]
prsLne ln =
  case T.strip bod of
    "" -> []
    _  -> [maybe (LitPgr [LitTxt bod]) id (parseMaybe litPrsBlk bod)]
 where
   bod = strCmt ln

-- || Lexing the literate document
--
-- We lex the whole literate file line-by-line, group lines into contiguous chunks of either prose or code.

-- `toCde` safely bundles a list of accumulated raw code lines into a single `LitCde` block.

toCde :: [Text] -> [LitBlk]
toCde lns =
 case T.intercalate "\n" lns of
   ""  -> []
   txt -> [LitCde txt]

-- `litFromLns` is the core lexer. It steps through the file using a sliding window of three lines (previous, current, next) to
-- satisfy the `isPrs` adjacency rules. It accumulates consecutive code lines into `cde` and parses prose lines instantly into `acc`,
-- flushing the code buffer whenever a prose block begins.

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

-- Finally, `litDoc` ties it all together: it consumes the entire file as raw text, splits it into lines, and feeds it through the
-- lexer.

litDoc :: GenPar [LitBlk]
litDoc = litFromLns . T.lines <$> takeWhileP (Just "source text") (const True)

testLitDoc :: IO ()
testLitDoc =
  case parseMaybe (litDoc <* eof) sampleLitDoc of
    Just blks -> putStrLn . T.unpack $ ppLitDoc blks
    Nothing   -> putStrLn "parse failed"

sampleLitDoc :: Text
sampleLitDoc = T.unlines
   [ "-- | Syntax and Semantics"
   , "--"
   , "-- The foundation of any markup language relies on plain text, followed by *syntactic forms*, and finally [references](https://example.com/ref) to external domains."
   , "-- - Structural boundaries"
   , "-- -- *Lexical analysis*"
   , "-- --- [Abstract syntax trees](https://example.com/ast)"
   , ""
   , "genInl :: GenPar GenInl"
   , "genInl = choice [genBld, genLnk, genTxt]"
   , "-- comment after code: stays in code block"
   , ""
   , "-- comment before code: stays in code block"
   , "echo :: Text"
   , "echo = \"a minimal invocation\""
   ]

-- ````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````
-- λ> testLitDoc
-- Hdg 1  "Syntax and Semantics"
-- Par    "The foundation of any markup language relies on plain text, followed by " *syntactic forms* ", and finally " [references](https://example.com/ref) " to external domains."
-- Lst 1  "Structural boundaries"
-- Lst 2  *Lexical analysis*
-- Lst 3  [Abstract syntax trees](https://example.com/ast)
-- Cod
--    genInl :: GenPar GenInl
--    genInl = choice [genBld, genLnk, genTxt]
--    -- comment after code: stays in code block
--
-- Hdg 1  "The Role of Delimiters"
-- Par    "A parser without " *backtracking* " finds edge cases."
-- Cod
--    -- comment before code: stays in code block
--    echo :: Text
--    echo = "a minimal invocation"
-- ````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````

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


-- | The anatomy of ἀγορά
--
-- Having walked through three approaches to markup: surface syntax, S-expressions, and literate programming, we can now build
-- the actual site generator. The preceding sections established the markup vocabulary: headings (`| heading`, `|| heading`, `|||
-- heading`), paragraphs, lists (`- item`, `-- item`), and literate code blocks. This section implements the full pipeline: from a
-- directory of stoa haskell files (στοές) to a published website with blog pages, a homepage, tag pages, and an RSS feed (πόλις).
--
-- The pipeline has five stages.
-- - A file is scanned to separate isolated comment blocks (prose) from the surrounding code.
-- - The first comment block is parsed as metadata.
-- - The remaining blocks are parsed into an AST.
-- - Code blocks are syntax-highlighted by invoking tree-sitter on the original source file.
-- - The AST is rendered to HTML and written to disk.
--
-- || Inline and block types
--
-- The inline and block types mirror those from the genesis and literate sections, with one structural addition: headings carry a
-- pre-computed element id alongside their content, used both as an HTML anchor and as the identifier for the sidebar outline. Code
-- blocks store a string of line-number placeholders (`{{n}}`) which are later substituted with highlighted HTML by the tree-sitter
-- stage.

data Inl
   = Txt Text
   | Bld Text
   | Lnk Text Text
   deriving (Show, Eq)

data Blk
   = Hdg Int [Inl] Text   -- level, content, element id
   | Par [Inl]
   | Lst Int [Inl]
   | Cod Text             -- highlighted HTML lines (or raw code in dry mode)
   deriving (Show, Eq)

-- Each article page carries a sidebar outline extracted from the document's headings. Only headings at levels one through three are
-- included.

data Out = Out
   { outTxt :: Text
   , outLvl :: Int
   , outId  :: Text
   } deriving (Show, Eq)

-- The metadata lives in the first isolated comment block of every stoa-haskell file, encoded as `key = value` pairs. The description
-- field may span multiple lines.

data Met = Met
   { metTtl :: Text
   , metPub :: Text
   , metTag :: Text
   , metDsc :: Text
   } deriving (Show, Eq)

-- A parsed article combines its metadata, block list, outline, and original file path. The path is kept so that tree-sitter can be
-- invoked on the correct source file.

data Doc = Doc
   { docMet :: Met
   , docBlk :: [Blk]
   , docOut :: [Out]
   , docPth :: FilePath
   } deriving (Show, Eq)

-- Tags aggregate documents under a shared label, used to build per-tag index pages.

data Tag = Tag
   { tagNam :: Text
   , tagDoc :: [Doc]
   } deriving (Show, Eq)

-- || Region extraction
--
-- A stoa-haskell file interleaves prose (isolated comment blocks) and code. The first task is to partition the file into a sorted
-- sequence of regions. A region is either a comment block or a code span.

data Rgn
   = RCom Text Int Int   -- stripped comment text, first line, last line
   | RCod Int Int        --             code span, first line, last line
   deriving (Show, Eq)

-- As described in literate section, a comment block is *isolated* when the line immediately before it and the line immediately after
-- it are both empty (or do not exist). Comments attached directly to code are treated as code.
--
-- `findComs` scans the indexed line list for sequences of comment lines that satisfy the isolation condition.

findComs :: [(Int, Text)] -> [(Text, Int, Int)]
findComs []  = []
findComs all = go all
 where
   go [] = []
   go ((n, l) : rest)
      | isCmt l =
         let (blk, aft) = span (isCmt . snd) ((n, l) : rest)
             befOk      = n == 1 || maybe True isEmp (lookup (n - 1) all)
             aftOk      = case aft of { [] -> True; ((_, h) : _) -> isEmp h }
         in  if befOk && aftOk
             then mkCom blk : go aft
             else go rest
      | otherwise = go rest

-- `mkCom` bundles a contiguous sequence of comment lines into `(strippedText, firstLine, lastLine)`. It strips the `--` prefix and
-- the single space following it from each line.

mkCom :: [(Int, Text)] -> (Text, Int, Int)
mkCom [] = error "mkCom: empty block"
mkCom lns@((b, _) : _) =
   ( T.intercalate "\n" (map (strip1 . snd) lns)
   , b
   , fst (last lns)
   )

-- `strip1` removes the `--` comment prefix and the leading space after it.

strip1 :: Text -> Text
strip1 ln =
   case T.stripPrefix "--" (T.stripStart ln) of
      Just rst -> case T.uncons rst of
         Just (' ', r) -> r
         _             -> rst
      Nothing -> ln

-- `codeGaps` fills in the code regions that exist between and around the comment blocks. It takes the sorted comment block list and
-- the total line count, then generates `RCod` entries for every line range not covered by a comment block.

codeGaps :: [(Text, Int, Int)] -> Int -> [Rgn]
codeGaps [] tot = [RCod 1 tot | tot > 0]
codeGaps all@((_, b0, _) : _) tot = bef ++ gaps ++ aft
 where
   bef   = [RCod 1 (b0 - 1) | b0 > 1]
   gaps  = [ RCod (ea + 1) (bb - 1)
           | ((_, _, ea), (_, bb, _)) <- zip all (drop 1 all)
           , ea + 1 <= bb - 1
           ]
   (_, _, le) = last all
   aft   = [RCod (le + 1) tot | le < tot]

-- `rgnBeg` extracts the start line from a region, used for sorting.

rgnBeg :: Rgn -> Int
rgnBeg (RCom _ b _) = b
rgnBeg (RCod   b _) = b

-- `extLns` is the pure core of extraction. It takes the file's lines, finds all isolated comment blocks, fills in code gaps, and
-- returns a sorted region list.

extLns :: [Text] -> [Rgn]
extLns lns =
   let idx = zip [1..] lns
       tot = length lns
       cms = findComs idx
       cds = codeGaps cms tot
   in  sortBy (comparing rgnBeg) (map toCom cms ++ cds)
 where
   toCom (txt, b, e) = RCom txt b e

-- `ext` is the IO wrapper: it reads the file from disk and delegates to `extLns`.

ext :: FilePath -> IO [Rgn]
ext pth = extLns . T.lines <$> TIO.readFile pth

-- || Metadata parsing
--
-- The first isolated comment block of every stoa-haskell file is its metadata header. It contains four required fields in `key =
-- value` format. The `description` field may continue on subsequent indented lines.
--
-- `pMet` reads a sequence of key-value pairs and folds them into a `Met` record, which is then validated before being returned.

pMet :: GenPar Met
pMet = do
   kvs <- many (pKV <* optional newline)
   eof
   pure (foldl apply (Met "" "" "" "") kvs)

-- `pKV` reads a single `key = value` entry. For the description key it delegates to `pCont` to collect any continuation lines.

pKV :: GenPar (Text, Text)
pKV = do
   key <- T.strip <$> takeWhile1P (Just "key") (/= '=')
   _   <- char '='
   val <- T.strip <$> takeWhileP (Just "value") (/= '\n')
   rst <- if key == "description" then pCont val else pure val
   pure (key, rst)

-- `pCont` greedily consumes continuation lines. A line continues the description if it follows a newline and does not look like a
-- new `key = ...` entry (detected by `pKVLook`).

pCont :: Text -> GenPar Text
pCont acc = do
   nxt <- optional (try (newline *> M.notFollowedBy (eof <|> void newline <|> void pKVLook) *> pLn))
   case nxt of
      Nothing -> pure acc
      Just ln -> pCont (acc <> " " <> T.strip ln)
 where
   pLn     = takeWhileP Nothing (/= '\n')
   pKVLook = takeWhile1P Nothing (\c -> c /= '=' && c /= '\n') *> char '=' *> pure ()

-- `apply` folds a single key-value pair into the `Met` record.

apply :: Met -> (Text, Text) -> Met
apply m ("title",       v) = m { metTtl = v }
apply m ("pubDate",     v) = m { metPub = v }
apply m ("tags",        v) = m { metTag = v }
apply m ("description", v) = m { metDsc = v }
apply m _                  = m

-- `validate` checks that all four required fields are present and that `pubDate` matches `yyyy-mm-dd`.

validate :: Met -> Either String Met
validate m
   | T.null (metTtl m) = Left "missing 'title'"
   | T.null (metPub m) = Left "missing 'pubDate'"
   | not (validDate (metPub m)) = Left "invalid pubDate, expected yyyy-mm-dd"
   | T.null (metTag m) = Left "missing 'tags'"
   | T.null (metDsc m) = Left "missing 'description'"
   | otherwise         = Right m

validDate :: Text -> Bool
validDate t =
   T.length t == 10
   && T.index t 4 == '-'
   && T.index t 7 == '-'

-- `met` is the public entry point: it parses the stripped metadata text and validates the result.

met :: Text -> Either String Met
met txt = case parse pMet "<meta>" txt of
   Left  err -> Left (M.errorBundlePretty err)
   Right m   -> validate m

-- `tags` splits the comma-separated tag string into a list of trimmed tag names.

tags :: Text -> [Text]
tags = filter (not . T.null) . map T.strip . T.splitOn ","

-- `pageId` derives the page identifier from the source file's base name (without extension).

pageId :: FilePath -> Text
pageId = T.pack . takeBaseName

-- || Content parsing
--
-- With metadata extracted, the remaining regions are converted into `Blk` values. Comment regions are parsed line-by-line into
-- headings, lists, and paragraphs. Code regions become `Cod` blocks whose content is a sequence of line-number placeholder strings
-- like `{{42}}\n{{43}}\n...`, later resolved by the highlighting stage.
--
-- `mrk` is the top-level content parser. It drops the first region (the metadata comment) and maps the rest to blocks, then extracts
-- the outline.

mrk :: [Rgn] -> ([Blk], [Out])
mrk rgns =
   let blks = concatMap rgnToBlks rgns
   in  (blks, mkOut blks)

-- `rgnToBlks` dispatches on region type. For comment regions it calls `parseLines` on the stripped text; for code regions it
-- synthesises one `Cod` block whose body is a newline-joined sequence of `{{n}}` placeholders for each line in the range.

rgnToBlks :: Rgn -> [Blk]
rgnToBlks (RCom txt _ _) = parseLines (T.lines txt)
rgnToBlks (RCod s e)     =
   let ph = T.intercalate "\n" ["{{" <> T.pack (show n) <> "}}" | n <- [s..e]]
   in  [Cod ph]

-- `parseLines` iterates lines from a comment block. Blank lines are skipped. Heading and list lines are consumed one at a time.
-- Paragraphs accumulate continuation lines until they hit a blank or a block-start.

parseLines :: [Text] -> [Blk]
parseLines = go
 where
   go [] = []
   go (l : ls)
      | isEmp (T.strip l) = go ls
      | "|" `T.isPrefixOf` ln =
         let (lvl, rst) = prefLvl '|' ln
         in  Hdg lvl (pInls rst) (genId rst) : go ls
      | "-" `T.isPrefixOf` ln =
         let (lvl, rst) = prefLvl '-' ln
         in  Lst lvl (pInls rst) : go ls
      | otherwise =
         let (cont, rest) = span (\x -> not (isEmp (T.strip x)) && not (isBlkStart (T.strip x))) ls
             txt          = T.intercalate " " (ln : map T.strip cont)
         in  Par (pInls txt) : go rest
    where ln = T.strip l

-- `prefLvl` counts leading repetitions of a delimiter character and returns the level alongside the remaining content. For example,
-- `prefLvl '|' "|| heading"` gives `(2, "heading")`.

prefLvl :: Char -> Text -> (Int, Text)
prefLvl c t =
   let n = T.length (T.takeWhile (== c) t)
   in  (n, T.strip (T.drop n t))

-- `isBlkStart` recognises lines that open a new block, so paragraph continuation stops at the right point.

isBlkStart :: Text -> Bool
isBlkStart t = "|" `T.isPrefixOf` t || "-" `T.isPrefixOf` t

-- Inline parsing reuses the zero-backtracking design from the genesis section. `pInls` applies `pInl` repeatedly over a single line
-- of text.

pInls :: Text -> [Inl]
pInls t = case parseMaybe pManyInl t of
   Just is -> is
   Nothing -> [Txt t]

pManyInl :: GenPar [Inl]
pManyInl = many pInl <* eof

pInl :: GenPar Inl
pInl = choice [try pBld, try pLnk, pTxt']

pBld :: GenPar Inl
pBld = Bld <$> between (char '*') (char '*')
   (takeWhile1P (Just "bold text") (`notElem` ("*\n" :: String)))

pLnk :: GenPar Inl
pLnk =
 Lnk
   <$> between (char '[') (string "](") (takeWhile1P (Just "link label") (`notElem` ("]\n" :: String)))
   <*> takeWhile1P (Just "url") (`notElem` (")\n" :: String))
   <* char ')'

pTxt' :: GenPar Inl
pTxt' = Txt . T.singleton <$> anySingle

-- `genId` transforms heading text into a valid HTML id: spaces become dashes, non-alphanumeric characters are dropped, and everything
-- is lowercased.

genId :: Text -> Text
genId = T.pack . go . T.unpack
 where
   go [] = []
   go (c : cs)
      | c == ' ' || c == '\t'              = '-' : go cs
      | isAlphaNum c || c == '-' || c == '_' = toLower c : go cs
      | otherwise                           = go cs

-- `plain` extracts the plain text from a list of inlines, used when generating outline entries.

plain :: [Inl] -> Text
plain = T.concat . map go
 where
   go (Txt t)     = t
   go (Bld t)     = t
   go (Lnk lbl _) = lbl

-- `mkOut` collects headings at levels one through three into the document outline.

mkOut :: [Blk] -> [Out]
mkOut blks =
   [ Out { outTxt = plain cnt, outLvl = lvl, outId = eid }
   | Hdg lvl cnt eid <- blks
   , lvl <= 3
   ]

-- || Syntax highlighting
--
-- The syntax highlighting design works in two passes. During content parsing, every code region becomes a `Cod` block whose body
-- is a newline-separated list of placeholder lines: `{{1}}\n{{2}}\n...{{n}}`. These refer back to the line numbers of the original
-- source file.
--
-- In the second pass, tree-sitter is invoked on the original file with `--css-classes` and HTML output. It returns a table of
-- per-line highlighted HTML. `hlt` reads this table into a `Map Int Text` and substitutes each `{{n}}` placeholder with the
-- corresponding highlighted line.
--
-- Because tree-sitter operates on the whole file, every language construct is highlighted correctly, and the result integrates into
-- the blog page as pre-escaped HTML.
--
-- `parsePh` recognises a `{{n}}` placeholder and extracts the line number.

parsePh :: Text -> Maybe Int
parsePh t
   | "{{" `T.isPrefixOf` t && "}}" `T.isSuffixOf` t =
      readMaybe (T.unpack (T.drop 2 (T.dropEnd 2 t)))
   | otherwise = Nothing

-- `subst` replaces every `{{n}}` line in a `Cod` block with its corresponding entry from the line map.

subst :: Map Int Text -> Blk -> Blk
subst lmp (Cod txt) =
   let lns = T.lines txt
       out = map (\l -> case parsePh l of
                           Just n  -> Map.findWithDefault "" n lmp
                           Nothing -> l) lns
       res = T.dropWhileEnd (== '\n') (T.intercalate "\n" out)
   in  Cod res
subst _ blk = blk

-- `findAfter` advances the cursor past the first occurrence of a needle string, returning the remainder.

findAfter :: Text -> Text -> Maybe Text
findAfter needle hay =
   let (_, aft) = T.breakOn needle hay
   in  if T.null aft then Nothing
       else Just (T.drop (T.length needle) aft)

-- `parseHlt` parses the HTML table emitted by `tree-sitter highlight -H --css-classes` into a line number to highlighted-HTML map.
-- Each row looks like: `<tr><td class=line-number>N</td><td class=line>...highlighted HTML...</td></tr>`

parseHlt :: Text -> Map Int Text
parseHlt = go Map.empty
 where
   trS = "<tr><td class=line-number>"
   trM = "</td><td class=line>"
   trE = "</td></tr>"

   go acc txt
      | T.null txt = acc
      | Just aft <- findAfter trS txt =
         case T.breakOn trM aft of
            (num, rst) | T.null rst -> acc
                       | otherwise  ->
               let cnt = T.drop (T.length trM) rst
               in  case T.breakOn trE cnt of
                     (ln, rst') | T.null rst' -> acc
                                | otherwise   ->
                        case readMaybe (T.unpack (T.strip num)) of
                           Just n  -> go (Map.insert n (T.dropWhileEnd (== '\n') ln) acc)
                                         (T.drop (T.length trE) rst')
                           Nothing -> go acc (T.drop 1 rst')
      | otherwise = acc

-- `treeSit` calls tree-sitter on the source file and returns the parsed line map.

treeSit :: FilePath -> IO (Map Int Text)
treeSit pth = do
   (code, out, _) <- readProcess (proc "tree-sitter" ["highlight", "-H", "--css-classes", pth])
   case code of
      ExitFailure _ -> pure Map.empty
      ExitSuccess   -> pure (parseHlt (TL.toStrict (TLE.decodeUtf8 out)))

-- `hlt` runs the full highlighting pass for a document.

hlt :: FilePath -> [Blk] -> IO [Blk]
hlt pth blks = do
   lmp <- treeSit pth
   pure (map (subst lmp) blks)

-- `hltDry` is a pure variant used in tests. Instead of calling tree-sitter, it resolves `{{n}}` placeholders directly from the
-- source lines, yielding the original (unhighlighted) code text.

hltDry :: [Text] -> [Blk] -> [Blk]
hltDry lns blks =
   let lmp = Map.fromList (zip [1..] lns)
   in  map (go lmp) blks
 where
   go lmp (Cod txt) =
      let out = map (\l -> case parsePh l of
                              Just n  -> Map.findWithDefault "" n lmp
                              Nothing -> l) (T.lines txt)
      in  Cod (T.intercalate "\n" out)
   go _ blk = blk

-- || HTML generation
--
-- HTML is generated as plain `Text` using string concatenation. This keeps the implementation self-contained and avoids coupling the
-- blog post to a template library.
--
-- `escHtm` escapes the five characters that are significant in HTML: `&`, `<`, `>`, `"`, and `'`. It is applied to all user-supplied
-- text before it is embedded in HTML.

escHtm :: Text -> Text
escHtm = T.concatMap go
 where
   go '&'  = "&amp;"
   go '<'  = "&lt;"
   go '>'  = "&gt;"
   go '"'  = "&quot;"
   go '\'' = "&#39;"
   go c    = T.singleton c

-- `inlHtm` renders a list of inline elements to HTML. Bold becomes `<strong>`, links become `<a href=...>`.

inlHtm :: [Inl] -> Text
inlHtm = T.concat . map go
 where
   go (Txt t)     = escHtm t
   go (Bld t)     = "<strong>" <> escHtm t <> "</strong>"
   go (Lnk lbl u) = "<a href=\"" <> escHtm u <> "\">" <> escHtm lbl <> "</a>"

-- `blkHtm` renders a single block. Code blocks emit pre-escaped HTML (already processed by tree-sitter); all other content goes
-- through `escHtm`.

blkHtm :: Blk -> Text
blkHtm (Hdg lvl cnt eid) = hN lvl eid (inlHtm cnt)
blkHtm (Par cnt)          = "<p>" <> inlHtm cnt <> "</p>\n"
blkHtm (Lst lvl cnt)      = "<p class=\"l" <> T.pack (show lvl) <> "-list\">"
                          <> inlHtm cnt <> "</p>\n"
blkHtm (Cod txt)          = "<pre><code>" <> txt <> "</code></pre>\n"

-- `hN` wraps content in the appropriate heading tag with an `id` attribute.

hN :: Int -> Text -> Text -> Text
hN n eid cnt =
   let tag = "h" <> T.pack (show (min n 6))
   in  "<" <> tag <> " id=\"" <> eid <> "\">" <> cnt <> "</" <> tag <> ">\n"

-- `page` is the full HTML shell. The `css` parameter is the relative path to the stylesheet directory, which differs between blog
-- pages (`../styles/`) and the home page (`styles/`).

page :: Text -> Text -> Text -> Text
page css ttl bdy = T.concat
   [ "<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n"
   , "<meta charset=\"UTF-8\">\n"
   , "<title>" <> escHtm ttl <> "</title>\n"
   , "<link rel=\"stylesheet\" href=\"" <> css <> "prima.css\">\n"
   , "</head>\n<body>\n"
   , bdy
   , "</body>\n</html>\n"
   ]

-- `outItem` renders a single outline entry as a navigation link.

outItem :: Out -> Text
outItem o =
   "<a href=\"#" <> outId o <> "\" class=\"h" <> T.pack (show (outLvl o))
   <> "\">" <> escHtm (outTxt o) <> "</a>\n"

-- `tagLinks` renders a comma-separated list of tag links given a path prefix.

tagLinks :: Text -> [Text] -> Text
tagLinks pfx tgs =
   T.intercalate ", "
      (map (\t -> "<a href=\"" <> pfx <> t <> ".html\">" <> escHtm t <> "</a>") tgs)

-- `blogPage` assembles the full article page: an aside with the document outline and a main section with the title, publication
-- date, tags, description, and rendered blocks.

blogPage :: Doc -> [Text] -> Text
blogPage doc tgs = page "../styles/" (metTtl m) bdy
 where
   m   = docMet doc
   bdy = T.concat
      [ "<aside class=\"outline-sidebar\">\n"
      , "<nav class=\"outline\">\n"
      , T.concat (map outItem (docOut doc))
      , "</nav>\n</aside>\n"
      , "<main class=\"main-content\">\n"
      , "<h1>" <> escHtm (metTtl m) <> "</h1>\n"
      , "<div class=\"metadata\">"
      , "<span class=\"date\">" <> metPub m <> "</span> "
      , "<span class=\"tags\">" <> tagLinks "../tags/" tgs <> "</span>"
      , "</div>\n"
      , "<div class=\"description\">" <> escHtm (metDsc m) <> "</div>\n"
      , T.concat (map blkHtm (docBlk doc))
      , "</main>\n"
      ]

-- `blogEntry` renders the summary card for a post as it appears on the home page and tag pages.

blogEntry :: Text -> Text -> Doc -> [Text] -> Text -> Text
blogEntry blgPfx tagPfx doc tgs pid = T.concat
   [ "<article class=\"blog-entry\">\n"
   , "<h2><a href=\"" <> blgPfx <> pid <> ".html\">"
   , escHtm (metTtl (docMet doc)) <> "</a></h2>\n"
   , "<div class=\"date\">" <> metPub (docMet doc) <> "</div>\n"
   , "<div class=\"description\">" <> escHtm (metDsc (docMet doc)) <> "</div>\n"
   , "<div class=\"tags\">" <> tagLinks tagPfx tgs <> "</div>\n"
   , "</article>\n"
   ]

-- `homePage` lists all posts in reverse-chronological order.

homePage :: [(Doc, [Text], Text)] -> Text
homePage docs = page "styles/" "home" bdy
 where
   bdy = "<main class=\"main-content\">\n<h1>all posts</h1>\n"
      <> T.concat (map (\(d, t, p) -> blogEntry "blogs/" "tags/" d t p) docs)
      <> "</main>\n"

-- `tagPage` lists all posts sharing a given tag.

tagPage :: Tag -> [(Doc, [Text], Text)] -> Text
tagPage tag docs = page "../styles/" ("tag: " <> tagNam tag) bdy
 where
   bdy = "<main class=\"main-content\">\n<h1>tag: "
      <> escHtm (tagNam tag) <> "</h1>\n"
      <> T.concat (map (\(d, t, p) -> blogEntry "../blogs/" "../tags/" d t p) docs)
      <> "</main>\n"

-- || RSS feed
--
-- The RSS feed lists every article as an `<item>` element. The description is wrapped in a `<![CDATA[...]]>` section so it can
-- contain arbitrary text without needing XML escaping.
--
-- `escXml` escapes the five XML-significant characters, used in title and link attributes.

escXml :: Text -> Text
escXml = T.concatMap go
 where
   go '&'  = "&amp;"
   go '<'  = "&lt;"
   go '>'  = "&gt;"
   go '"'  = "&quot;"
   go '\'' = "&#39;"
   go c    = T.singleton c

-- `rssItem` renders a single feed entry.

rssItem :: Text -> (Doc, [Text], Text) -> Text
rssItem url (doc, _, pid) = T.concat
   [ "<item>\n"
   , "<title>" <> escXml (metTtl m) <> "</title>\n"
   , "<link>" <> escXml url <> "/blogs/" <> pid <> ".html</link>\n"
   , "<pubDate>" <> metPub m <> "</pubDate>\n"
   , "<description><![CDATA[" <> metDsc m <> "]]></description>\n"
   , "</item>\n"
   ]
 where m = docMet doc

-- `rss` assembles the full feed document.

rss :: Text -> [(Doc, [Text], Text)] -> Text
rss url docs = T.concat
   [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
   , "<rss version=\"2.0\">\n<channel>\n"
   , "<title>blog</title>\n"
   , "<link>" <> escXml url <> "</link>\n"
   , T.concat (map (rssItem url) docs)
   , "</channel>\n</rss>\n"
   ]

-- || Site builder
--
-- The site builder composites the full pipeline. `scn` finds all `.hs` files in a source directory recursively. `processDoc` runs
-- the five-stage pipeline on a single file. Finally `bld` coordinates everything.
--
-- `scn` traverses the source directory and collects all `.hs` files.

scn :: FilePath -> IO [FilePath]
scn dir = do
   ent <- listDirectory dir
   fmap concat . mapM go $ map (dir </>) ent
 where
   go pth = do
      isD <- doesDirectoryExist pth
      if isD then scn pth
      else pure [pth | takeExtension pth == ".hs"]

-- `mkEntry` bundles a `Doc` with its parsed tag list and page id, forming the tuple used throughout the rendering stage.

mkEntry :: Doc -> (Doc, [Text], Text)
mkEntry doc = (doc, tags (metTag (docMet doc)), pageId (docPth doc))

-- `processDoc` is the per-file pipeline: extract regions, parse metadata, parse content, highlight.

processDoc :: FilePath -> IO Doc
processDoc pth = do
   raw <- TIO.readFile pth
   let lns  = T.lines raw
       rgns = extLns lns
   (txt, rest) <- case rgns of
      (RCom t _ _ : rs) -> pure (t, rs)
      _                 -> ioError (userError (pth <> ": no metadata block"))
   m <- case met txt of
      Left  err -> ioError (userError err)
      Right m   -> pure m
   let (blks, ots) = mrk rest
   blks' <- hlt pth blks
   pure Doc { docMet = m, docBlk = blks', docOut = ots, docPth = pth }

-- `buildTagMap` groups documents by tag, building a map from tag name to `Tag`.

buildTagMap :: [(Doc, [Text], Text)] -> Map Text Tag
buildTagMap = foldl go Map.empty
 where
   go acc (doc, tgs, _) = foldl (addTag doc) acc tgs
   addTag doc acc t =
      Map.alter (\case
         Nothing -> Just (Tag t [doc])
         Just tg -> Just (tg { tagDoc = tagDoc tg ++ [doc] })
      ) t acc

-- `writeTags` generates one HTML page per tag plus a tag listing page (skipped here per the plan).

writeTags :: FilePath -> [(Doc, [Text], Text)] -> IO ()
writeTags out entries = do
   let tgm = buildTagMap entries
       tgs = Map.elems tgm
   mapM_ (\tg ->
      TIO.writeFile (out </> "tags" </> T.unpack (tagNam tg) <> ".html")
         (tagPage tg (map mkEntry (tagDoc tg)))
      ) tgs

-- `bld` is the main entry point. It scans the source directory, processes each file, sorts the results by publication date (newest
-- first), writes all output files, and reports progress to stdout.

bld :: FilePath -> FilePath -> Text -> IO ()
bld src out url = do
   putStrLn "scanning source files"
   fps <- scn src
   when (null fps) $ error "no .hs source files found"
   putStrLn $ "found " <> show (length fps) <> " files"

   putStrLn "processing files"
   docs <- mapM processDoc fps

   let sorted  = sortBy (comparing (Down . metPub . docMet)) docs
       entries = map mkEntry sorted

   putStrLn "writing output"
   createDirectoryIfMissing True (out </> "blogs")
   createDirectoryIfMissing True (out </> "tags")
   createDirectoryIfMissing True (out </> "styles")

   mapM_ (\(doc, tgs, pid) ->
      TIO.writeFile (out </> "blogs" </> T.unpack pid <> ".html") (blogPage doc tgs)
      ) entries
   TIO.writeFile (out </> "index.html") (homePage entries)
   writeTags out entries
   TIO.writeFile (out </> "feed.xml") (rss url entries)

   putStrLn "done"

-- || Sample documents
--
-- To test the pipeline, we define three complete stoa-haskell documents as `Text` values. Each has a metadata block, content
-- headings (levels 1-3), paragraphs, lists, and code sections. The code sections are trivial Haskell so they parse cleanly as `Cod`
-- blocks.

sampleDoc1 :: Text
sampleDoc1 = T.unlines
   [ "-- title       = A small parsing exercise"
   , "-- pubDate     = 2026-01-15"
   , "-- tags        = haskell, parsing"
   , "-- description = Walking through a small parser for comma-separated values in Haskell."
   , ""
   , "import Data.Text (Text)"
   , "import qualified Data.Text as T"
   , ""
   , "-- | Parsing CSV"
   , "--"
   , "-- A comma-separated value file is one of the simplest formats to parse. Each row is"
   , "-- a newline-terminated sequence of fields separated by a comma."
   , ""
   , "-- || The field parser"
   , "--"
   , "-- We build from the bottom up. A field is any text without a comma."
   , "-- - Split on commas to obtain the fields."
   , "-- - Trim leading and trailing whitespace from each one."
   , ""
   , "parseRow :: Text -> [Text]"
   , "parseRow = map T.strip . T.splitOn \",\""
   , ""
   , "-- || Testing"
   , "--"
   , "-- A quick sanity check confirms the parser handles basic input."
   , ""
   , "testParse :: Bool"
   , "testParse = parseRow \"alpha, beta, gamma\" == [\"alpha\", \"beta\", \"gamma\"]"
   ]

sampleDoc2 :: Text
sampleDoc2 = T.unlines
   [ "-- title       = Minimal typeclasses"
   , "-- pubDate     = 2026-02-10"
   , "-- tags        = haskell, design"
   , "-- description = How typeclasses replace explicit dispatch tables."
   , ""
   , "-- | What is a typeclass"
   , "--"
   , "-- A typeclass in Haskell is an interface for overloading operations across types. Unlike"
   , "-- object-oriented interfaces, instances are declared outside the type definition, which"
   , "-- cleanly decouples the two."
   , ""
   , "-- || Defining a typeclass"
   , "--"
   , "-- A simple typeclass for pretty-printing illustrates the structure."
   , "-- - The class declares a single method `pp`."
   , "-- - Instances provide the implementation."
   , "-- - Any type has at most one canonical instance."
   , ""
   , "class Pretty a where"
   , "   pp :: a -> String"
   , ""
   , "-- || Instances"
   , "--"
   , "-- Two simple instances show how dispatch resolves at compile time."
   , ""
   , "instance Pretty Int where"
   , "   pp n = \"Int: \" ++ show n"
   , ""
   , "instance Pretty Bool where"
   , "   pp b = if b then \"yes\" else \"no\""
   ]

sampleDoc3 :: Text
sampleDoc3 = T.unlines
   [ "-- title       = On immutable data"
   , "-- pubDate     = 2026-02-28"
   , "-- tags        = haskell, design, 2026"
   , "-- description = Persistent data structures eliminate a large class of bugs by forbidding mutation."
   , ""
   , "-- | Persistence by default"
   , "--"
   , "-- In Haskell, all data is immutable. A function that appears to modify a list"
   , "-- actually produces a new one, sharing the unchanged tail with the original."
   , ""
   , "-- || What sharing looks like"
   , "--"
   , "-- Consider a simple prepend operation on a linked list."
   , "-- - The new list points to the same tail as the old."
   , "-- - The old list is unaffected and still accessible."
   , "-- - No copying occurs: only a new head node is allocated."
   , ""
   , "prepend :: a -> [a] -> [a]"
   , "prepend x xs = x : xs"
   , ""
   , "-- ||| A concrete example"
   , "--"
   , "-- With persistence, both `xs` and `ys` coexist in memory without conflict."
   , ""
   , "example :: ([Int], [Int])"
   , "example ="
   , "   let xs = [1, 2, 3]"
   , "       ys = prepend 0 xs"
   , "   in  (xs, ys)"
   ]

-- || Testing the anatomy pipeline
--
-- `docPure` runs the full pipeline in pure mode. It uses `hltDry` in place of `hlt`, resolving `{{n}}` placeholders directly from
-- the source lines rather than calling tree-sitter. This makes the test self-contained and runnable without any external tools.

docPure :: FilePath -> Text -> Either String Doc
docPure pth raw = do
   let lns  = T.lines raw
       rgns = extLns lns
   (txt, rest) <- case rgns of
      (RCom t _ _ : rs) -> Right (t, rs)
      _                 -> Left (pth ++ ": first region is not a comment")
   m <- met txt
   let (blks, ots) = mrk rest
       blks'       = hltDry lns blks
   pure (Doc m blks' ots pth)

-- `testAnatomy` processes all three sample documents and prints the key results: each document's metadata and outline, a truncated
-- blog page for inspection, the homepage listing, and the RSS feed.

testAnatomy :: IO ()
testAnatomy = do
   let samples = [ ("sample1.hs", sampleDoc1)
                 , ("sample2.hs", sampleDoc2)
                 , ("sample3.hs", sampleDoc3)
                 ]
   docs <- mapM (\(pth, raw) -> case docPure pth raw of
      Left  err -> ioError (userError err)
      Right d   -> pure d) samples

   putStrLn "=== Metadata ==="
   mapM_ (\d -> do
      let m = docMet d
      putStrLn $ T.unpack (metTtl m)
             ++ "  [" ++ T.unpack (metPub m) ++ "]"
             ++ "  tags: " ++ T.unpack (metTag m)
      ) docs

   putStrLn "\n=== Outlines ==="
   mapM_ (\d -> do
      putStrLn $ "-- " ++ T.unpack (metTtl (docMet d))
      mapM_ (\o -> putStrLn $ replicate (outLvl o * 2) ' ' ++ T.unpack (outTxt o)) (docOut d)
      ) docs

   let entries = map mkEntry docs

   putStrLn "\n=== Blog Page (sample1, truncated) ==="
   case docs of
      (d : _) -> TIO.putStrLn (T.take 400 (blogPage d (tags (metTag (docMet d))))) >> putStrLn "..."
      []      -> pure ()

   putStrLn "\n=== Home Page (truncated) ==="
   TIO.putStrLn (T.take 400 (homePage entries))
   putStrLn "..."

   putStrLn "\n=== RSS Feed (truncated) ==="
   TIO.putStrLn (T.take 400 (rss "https://example.com" entries))
   putStrLn "..."

-- ``````````````````````````````````````````````````````````````````````````````````````````
-- λ> testAnatomy
-- === Metadata ===
-- A small parsing exercise  [2026-01-15]  tags: haskell, parsing
-- Minimal typeclasses  [2026-02-10]  tags: haskell, design
-- On immutable data  [2026-02-28]  tags: haskell, design, 2026
--
-- === Outlines ===
-- -- A small parsing exercise
--   Parsing CSV
--     The field parser
--     Testing
-- -- Minimal typeclasses
--   What is a typeclass
--     Defining a typeclass
--     Instances
-- -- On immutable data
--   Persistence by default
--     What sharing looks like
--       A concrete example
--
-- === Blog Page (sample1, truncated) ===
-- <!DOCTYPE html>
-- <html lang="en">
-- <head>
-- <meta charset="UTF-8">
-- <title>A small parsing exercise</title>
-- <link rel="stylesheet" href="../styles/prima.css">
-- </head>
-- <body>
-- <aside class="outline-sidebar">
-- <nav class="outline">
-- <a href="#parsing-csv" class="h1">Parsing CSV</a>
-- <a href="#the-field-parser" class="h2">The field parser</a>
-- <a href="#testing" class="h2">Testing</a>
-- </nav>
-- </aside>
-- <main class="main-content">
-- <h1>A small parsing exercise</h1>
-- ...
--
-- === Home Page (truncated) ===
-- <!DOCTYPE html>
-- <html lang="en">
-- ...
-- <h2><a href="blogs/sample1.html">A small parsing exercise</a></h2>
-- <div class="date">2026-01-15</div>
-- ...
--
-- === RSS Feed (truncated) ===
-- <?xml version="1.0" encoding="UTF-8"?>
-- <rss version="2.0">
-- <channel>
-- <title>blog</title>
-- <link>https://example.com</link>
-- <item>
-- <title>A small parsing exercise</title>
-- <link>https://example.com/blogs/sample1.html</link>
-- <pubDate>2026-01-15</pubDate>
-- <description><![CDATA[Walking through a small parser...]]></description>
-- </item>
-- ...
-- ``````````````````````````````````````````````````````````````````````````````````````````

-- || Final step: CLI with optparse-applicative
--
-- [optparse-applicative](https://hackage.haskell.org/package/optparse-applicative) provides a declarative interface for building
-- command-line argument parsers. The site tool takes three positional arguments: the source directory of stoa-haskell files, the
-- output directory, and the base URL of the published site.

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

main :: IO ()
main = do
   o <- execParser opt
   bld (optSrc o) (optOut o) (T.pack (optUrl o))

-- | Reflexions
--
-- Eleven months is a long time to spend on an SSG. The honest accounting is: one month prototyping στοά in Nim, two months getting
-- a first blog published, one month with S-expressions, three months rebuilding as literate Haskell, four months stabilizing. A
-- weekend with [mdbook](https://github.com/rust-lang/mdBook) would have produced a working blog back in april.2025. I chose not to
-- do that because I wanted the writing environment to be mine in every detail.
--
-- The three iterations look wasteful in retrospect but yet still, each one clarified something. στοά proved that custom delimiters
-- and zero-backtracking parsers were viable and fast. The S-expression experiment proved that structural purity and authoring
-- fluency pull in opposite directions. Literate programming resolved both: the parser stays simple (isolated comments, code gaps),
-- the writing stays fluid (plain comment syntax), and the code stays real (the compiler sees it directly). None of these conclusions
-- were available before building and discarding the earlier versions.
--
-- I do not expect anyone else to use ἀγορά. The warning at the top of this essay is genuine. This tool encodes my preferences so
-- precisely that it would be actively confusing for someone who doesn't share them. Whether the eleven months were worth it depends
-- entirely on whether the writing produced in this environment turns out to be good.
