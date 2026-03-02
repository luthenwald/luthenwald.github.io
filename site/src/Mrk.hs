module Mrk (mrk, inl, genId) where

import           Data.Char            ( isAlphaNum, isDigit, toLower )
import           Data.List            ( sortOn )
import           Data.Text            ( Text )
import qualified Data.Text            as T
import           Data.Void            ( Void )

import           Ext                  ( Com (..), Rgn (..) )

import           System.FilePath      ( takeDirectory, (</>) )

import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Read            ( readMaybe )

import           Typ

type Par = Parsec Void Text

mrk :: FilePath -> [Rgn] -> ([Blk], [Out])
mrk pth rgns =
   let sorted = sortOn rgnOrd rgns
       (blks, ftns) = foldl (go pth) ([], []) sorted
   in  (blks ++ ftns, outline blks)
 where
   rgnOrd (RCom c)   = comBeg c
   rgnOrd (RCod s _) = s

go :: FilePath -> ([Blk], [Blk]) -> Rgn -> ([Blk], [Blk])
go pth (acc, ftn) (RCom c) =
   let lns = T.lines (comTxt c)
       (bs, fs) = parseLines pth lns
   in  (acc ++ bs, ftn ++ fs)
go _ (acc, ftn) (RCod s e) =
   let ph = T.intercalate "\n"
            [ "{{" <> T.pack (show n) <> "}}" | n <- [s..e] ]
   in  (acc ++ [Cod ph], ftn)

-- block-level line parsing

parseLines :: FilePath -> [Text] -> ([Blk], [Blk])
parseLines pth = go' [] []
 where
   go' bs fs []     = (reverse bs, reverse fs)
   go' bs fs (l:ls)
      | T.null (T.strip l) = go' bs fs ls
      | isVerbatim l =
         let (cnt, rest) = spanVerbatim ls
         in  go' (Vrb cnt : bs) fs rest
      | "@insert" `T.isPrefixOf` ln =
         let arg = T.strip (T.drop 7 ln)
             fp  = takeDirectory pth </> T.unpack arg
         in  go' (Ins fp : bs) fs ls
      | "@img" `T.isPrefixOf` ln =
         let arg = T.strip (T.drop 4 ln)
             fp  = takeDirectory pth </> T.unpack arg
         in  go' (Img fp : bs) fs ls
      | "^" `T.isPrefixOf` ln, validFtn ln =
         let (full, rest) = consumeCont ls
             txt = ln <> " " <> full
         in  case parseFtn txt of
               Just f  -> go' bs (f : fs) rest
               Nothing -> go' (Par (inl ln) : bs) fs ls
      | "|" `T.isPrefixOf` ln =
         let (lvl, rst) = prefixLvl '|' ln
             cnt = inl rst
             eid = genId rst
         in  go' (Hdg lvl cnt eid : bs) fs ls
      | "-" `T.isPrefixOf` ln =
         let (full, rest) = consumeCont ls
             txt = ln <> " " <> full
             (lvl, rst) = prefixLvl '-' txt
         in  go' (Lst lvl (inl rst) : bs) fs rest
      | ">" `T.isPrefixOf` ln =
         let (full, rest) = consumeCont ls
             txt = T.strip (T.drop 1 ln) <> " " <> full
         in  go' (Cal (inl txt) : bs) fs rest
      | otherwise =
         let (para, rest) = spanPara ls
             txt = T.intercalate " " (ln : para)
         in  go' (Par (inl txt) : bs) fs rest
    where ln = T.strip l

spanVerbatim :: [Text] -> (Text, [Text])
spanVerbatim ls =
   let (cnt, rest) = break isVerbatim ls
   in  (T.intercalate "\n" cnt, drop 1 rest)

isVerbatim :: Text -> Bool
isVerbatim t =
   let n = T.length (T.takeWhile (== '`') (T.strip t))
   in  n >= 6

spanPara :: [Text] -> ([Text], [Text])
spanPara []     = ([], [])
spanPara (l:ls)
   | T.null (T.strip l) = ([], ls)
   | isBlkStart (T.strip l) = ([], l:ls)
   | otherwise = let (r, rs) = spanPara ls in (T.strip l : r, rs)

consumeCont :: [Text] -> (Text, [Text])
consumeCont = go' []
 where
   go' acc [] = (T.intercalate " " (reverse acc), [])
   go' acc (l:ls)
      | T.null (T.strip l) = (T.intercalate " " (reverse acc), ls)
      | isIndented l && not (isBlkStart (T.strip l)) =
         go' (T.strip l : acc) ls
      | otherwise = (T.intercalate " " (reverse acc), l:ls)

isIndented :: Text -> Bool
isIndented t = case T.uncons t of
   Just (' ',  _) -> True
   Just ('\t', _) -> True
   _              -> False

isBlkStart :: Text -> Bool
isBlkStart t
   | "-"       `T.isPrefixOf` t = True
   | ">"       `T.isPrefixOf` t = True
   | "|"       `T.isPrefixOf` t = True
   | "^"       `T.isPrefixOf` t = validFtn t
   | "@insert" `T.isPrefixOf` t = True
   | "@img"    `T.isPrefixOf` t = True
   | otherwise = False

validFtn :: Text -> Bool
validFtn t =
   case T.uncons (T.drop 1 t) of
      Nothing -> False
      Just _  ->
         let aft = T.drop 1 t
             (ds, rst) = T.span isDigit aft
         in  not (T.null ds) && case T.uncons (T.stripStart rst) of
               Just ('.', _) -> True
               _             -> False

parseFtn :: Text -> Maybe Blk
parseFtn t = do
   let aft  = T.drop 1 t
       (ds, rst) = T.span isDigit aft
   n <- readMaybe (T.unpack ds)
   let txt = T.strip $ T.drop 1 (T.stripStart rst)
   Just $ Ftn n (inl txt)

prefixLvl :: Char -> Text -> (Int, Text)
prefixLvl ch t =
   let n = T.length (T.takeWhile (== ch) t)
   in  (n, T.strip (T.drop n t))

-- inline element parsing

inl :: Text -> [Inl]
inl t = case parse pInls "" t of
   Left  _ -> [Txt t]
   Right r -> r

pInls :: Par [Inl]
pInls = many pInl <* eof

pInl :: Par Inl
pInl = choice
   [ try pLnk
   , try pRef
   , try pBld
   , try pCde
   , pTxt
   ]

pLnk :: Par Inl
pLnk = do
   _ <- char '['
   txt <- takeWhile1P Nothing (/= ']')
   _ <- char ']'
   _ <- char '('
   url <- pUrl
   _ <- char ')'
   pure $ Lnk txt url

pUrl :: Par Text
pUrl = T.pack <$> goU (0 :: Int)
 where
   goU d = do
      mc <- optional (lookAhead anySingle)
      case mc of
         Nothing              -> pure []
         Just ')' | d == 0    -> pure []
         Just c -> do
            _ <- anySingle
            case c of
               '(' -> (c :) <$> goU (d + 1)
               ')' -> (c :) <$> goU (d - 1)
               _   -> (c :) <$> goU d

pRef :: Par Inl
pRef = do
   _ <- string "[^"
   ds <- takeWhile1P Nothing isDigit
   _ <- char ']'
   case readMaybe (T.unpack ds) of
      Just n  -> pure $ Ref n
      Nothing -> fail "invalid footnote ref"

pBld :: Par Inl
pBld = do
   _ <- char '*'
   txt <- takeWhile1P Nothing (/= '*')
   _ <- char '*'
   pure $ Bld txt

pCde :: Par Inl
pCde = do
   _ <- char '`'
   txt <- takeWhile1P Nothing (/= '`')
   _ <- char '`'
   pure $ Cde txt

pTxt :: Par Inl
pTxt = Txt . T.singleton <$> anySingle

-- heading ID & outline

genId :: Text -> Text
genId = T.pack . goI . T.unpack
 where
   goI [] = []
   goI (c:cs)
      | c == ' ' || c == '\t' = '-' : goI cs
      | isAlphaNum c || c == '-' || c == '_' = toLower c : goI cs
      | otherwise = goI cs

outline :: [Blk] -> [Out]
outline [] = []
outline (Hdg lvl cnt eid : bs) =
   Out { outTxt = plainTxt cnt, outLvl = lvl, outId = eid } : outline bs
outline (_ : bs) = outline bs

plainTxt :: [Inl] -> Text
plainTxt = T.concat . map goP
 where
   goP (Txt t)   = t
   goP (Lnk t _) = t
   goP (Bld t)   = t
   goP (Cde t)   = t
   goP (Ref _)   = ""
