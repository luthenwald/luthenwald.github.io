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
                                       some, satisfy, takeWhile1P,
                                       takeWhileP, (<|>) )
import qualified Text.Megaparsec      as M
import           Text.Megaparsec.Char ( char, eol, hspace1, newline, string )

-- TODO: use the same sampleDoc for every section, but adapted to each's syntax
--
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
-- The first attempt to actually build my own SSG began in April 2025. It was written in NimLang, named *stoa/στοά*. To be precise,
-- στοά is the surface markup language for ἀγορά. The directory where στοά files are located is called *stoae/στοές* (the plural of
-- stoa), and *stoac* is the compiler that transforms στοές into the *polis/πόλις* (the website). Other aliases follow this central
-- Greek theme.
--
-- || Why not markdown
--
-- It is fundamentally an aesthetic matter. For example, I find `|` visually prettier than `#` as the delimiter for headings. As I've
-- mentioned earlier, I believe we are more likely to create something of value when operating in a medium we find most comfortable
-- with. If I consider `|` prettier than `#`, this single aesthetic deviation is sufficient justification to design an entirely new
-- markup language. From there, I am free to redefine every delimiter and keep a strictly ideomatic set of markup types.
--
-- Let's construct a tiny implementation to capture this initial design. We will use the `Gen*` prefix so this genesis vocabulary
-- stays distinct from the later formal definitions.
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

-- By default, `megaparsec` does not backtrack automatically. If a parser consumes any input before failing, alternative branches
-- (`<|>`) are not tried. While you can force backtracking using the `try` combinator, this drastically slows down the parsing
-- process by keeping arbitrary input in memory.
--
-- We can treat this limitation as a rigid design constraint: every inline and block markup must possess a globally unique starting
-- delimiter.
--
-- For instance, Markdown delimits bold with `**` and italic with `*`. A parser seeing an `*` doesn't immediately know which branch
-- to commit to, which requires backtracking. In στοά, we avoid this entirely: delimit bold with `*` and italic with `/` (similar
-- to Neorg). Because the first character strictly defines the node type, our parser becomes incredibly fast and structurally
-- predictable.
--
-- With this zero-backtracking rule in hand, we can implement the lexer.
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
-- For simplicity, we won't implement indented lists here, meaning blocks will always span a single physical line (except for raw
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
-- an S-expression. The genesis design (which forbids backtracking by requiring unique leading symbols) remains entirely valid here.
-- If we replace surface symbols like `*` or `|` with Lisp-style forms, we can maintain the same markup coverage while adopting a
-- strictly nested, minimal syntax.
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
-- sibling elements. Text editing is fundamentally linear and fluid, not structured and hierarchical.
--
-- So while Lisp markup solved our parsing rigidity and uniformity problems gracefully, the writing experience was unbearable for
-- long-form prose. This realization pushed me toward the final iteration of ἀγορά: leaning into literate programming, where code is
-- code and prose is written mostly linearly.

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
-- prose is housed inside the native comments of that language.
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


-- | The anatomy of agora
--
-- We are now eligible to formally illustrate the architecture of `stoa` by actually defining the data models.
--
-- Besides parser, we will also build the html generator, we will finally test agora with some sample haskell files
-- to prove it works.
--
-- The stoa markup language is the same as that of literate programming section.
-- But we now need to record line numbers because we need to run tree-sitter highlight on the extracted code blocks,
-- and insert them back to the generated html

data Inl

data Blk

-- we have an outline for each article page, which will reside on the sidebar of each article page.

data Out = Out
   { outTxt :: Text
   , outLvl :: Int
   , outId  :: Text
   } deriving (Show, Eq)

-- and the metadata of each article, which will reside below the title of the article page, the metadata will
-- also be used in homepage and tagpages

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

-- || Syntax highlighting
--
-- Note we want to syntax highlight the extracted code block

-- || Final step: CLI with optparse-applicative
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

main :: IO ()
main = do
   o <- execParser opt
   bld (optSrc o) (optOut o) (optUrl o)

-- | Style guide of πόλις
--
-- || Law of 1.44
--
-- || Tufte css but no sidenotes



-- | Reflexions
