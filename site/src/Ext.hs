module Ext (Com(..), Rgn(..), ext) where

import           Cfg          ( Lng (..) )

import           Data.List    ( sortOn )
import           Data.Text    ( Text )
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO

data Com = Com
   { comTxt :: Text
   , comBeg :: Int
   , comEnd :: Int
   } deriving (Show, Eq)

data Rgn
   = RCom Com
   | RCod Int Int
   deriving (Show, Eq)

ext :: Lng -> FilePath -> IO [Rgn]
ext lng pth = do
   raw <- TIO.readFile pth
   let lns = T.lines raw
       tot = length lns
       pfx = lngPfx lng
       cms = findBlocks pfx (zip [1..] lns)
       cds = codeGaps cms tot
   pure $ sortOn rgnBeg (map RCom cms ++ cds)

findBlocks :: Text -> [(Int, Text)] -> [Com]
findBlocks _   [] = []
findBlocks pfx ls = go ls
 where
   go [] = []
   go ((n, l) : rest)
      | isCom pfx l =
         let (blk, aft) = span (isCom pfx . snd) ((n, l) : rest)
             bef_ok     = n == 1 || prevEmpty (n - 1)
             aft_ok     = case aft of [] -> True; (h:_) -> isEmpty (snd h)
         in  if bef_ok && aft_ok
             then mkCom pfx blk : go aft
             else go rest
      | otherwise = go rest

   prevEmpty k = case lookup k (zip (map fst ls) (map snd ls)) of
      Just t  -> isEmpty t
      Nothing -> True

isCom :: Text -> Text -> Bool
isCom pfx ln = pfx `T.isPrefixOf` T.stripStart ln

isEmpty :: Text -> Bool
isEmpty = T.null . T.strip

mkCom :: Text -> [(Int, Text)] -> Com
mkCom _ [] = error "mkCom: empty list"
mkCom pfx lns@((b,_):_) = Com
   { comTxt = T.intercalate "\n" (map (strip1 pfx . snd) lns)
   , comBeg = b
   , comEnd = fst (last lns)
   }

strip1 :: Text -> Text -> Text
strip1 pfx ln =
   case T.stripPrefix pfx (T.stripStart ln) of
      Just rst -> case T.uncons rst of
         Just (' ', r) -> r
         _             -> rst
      Nothing -> ln

codeGaps :: [Com] -> Int -> [Rgn]
codeGaps [] tot
   | tot > 0   = [RCod 1 tot]
   | otherwise  = []
codeGaps (c:cs) tot = bef ++ gaps ++ aft
 where
   bef  = [RCod 1 (comBeg c - 1) | comBeg c > 1]
   gaps = [ RCod (comEnd a + 1) (comBeg b - 1)
          | (a, b) <- zip (c:cs) cs
          , comEnd a + 1 <= comBeg b - 1
          ]
   aft  = [RCod (comEnd (last (c:cs)) + 1) tot | comEnd (last (c:cs)) < tot]

rgnBeg :: Rgn -> Int
rgnBeg (RCom c)   = comBeg c
rgnBeg (RCod s _) = s
