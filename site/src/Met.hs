module Met (met, tags, pageId) where

import           Control.Monad        ( void )

import           Data.Text            ( Text )
import qualified Data.Text            as T
import           Data.Void            ( Void )

import           System.FilePath      ( takeBaseName )

import           Text.Megaparsec
import           Text.Megaparsec.Char

import           Typ                  ( Met (..) )

type Par = Parsec Void Text

met :: FilePath -> Text -> Either String Met
met pth raw = case parse pMet pth raw of
   Left  err -> Left (errorBundlePretty err)
   Right m   -> validate pth m

pMet :: Par Met
pMet = do
   kvs <- many (pKV <* optional newline)
   eof
   pure $ foldl apply (Met "" "" "" "") kvs

pKV :: Par (Text, Text)
pKV = do
   key <- T.strip <$> takeWhile1P (Just "key") (/= '=')
   _ <- char '='
   val <- T.strip <$> takeWhileP (Just "value") (/= '\n')
   rest <- if key == "description" then pContinuation val else pure val
   pure (key, rest)

pContinuation :: Text -> Par Text
pContinuation acc = do
   nxt <- optional (try (newline *> notFollowedBy (eof <|> void newline <|> void pKVLookahead) *> pLine))
   case nxt of
      Nothing -> pure acc
      Just ln -> pContinuation (acc <> " " <> T.strip ln)
 where
   pLine = takeWhileP Nothing (/= '\n')
   pKVLookahead :: Par ()
   pKVLookahead = do
      _ <- takeWhile1P Nothing (\c -> c /= '=' && c /= '\n')
      _ <- char '='
      pure ()

apply :: Met -> (Text, Text) -> Met
apply m ("title",       v) = m { metTtl = v }
apply m ("pubDate",     v) = m { metPub = v }
apply m ("tags",        v) = m { metTag = v }
apply m ("description", v) = m { metDsc = v }
apply m _                  = m

validate :: FilePath -> Met -> Either String Met
validate pth m
   | T.null (metTtl m) = Left $ pth ++ ": missing 'title'"
   | T.null (metPub m) = Left $ pth ++ ": missing 'pubDate'"
   | not (validDate (metPub m)) = Left $ pth ++ ": invalid pubDate format, expected yyyy-mm-dd"
   | T.null (metTag m) = Left $ pth ++ ": missing 'tags'"
   | T.null (metDsc m) = Left $ pth ++ ": missing 'description'"
   | otherwise = Right m

validDate :: Text -> Bool
validDate t =
   T.length t == 10
   && T.index t 4 == '-'
   && T.index t 7 == '-'
   && T.all isD (T.take 4 t)
   && T.all isD (T.take 2 (T.drop 5 t))
   && T.all isD (T.drop 8 t)
 where isD c = c >= '0' && c <= '9'

tags :: Text -> [Text]
tags = map T.strip . filter (not . T.null) . T.splitOn ","

pageId :: FilePath -> Text
pageId = T.pack . takeBaseName
