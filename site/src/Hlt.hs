module Hlt (hlt) where

import           Data.Map.Strict         ( Map )
import qualified Data.Map.Strict         as Map
import           Data.Text               ( Text )
import qualified Data.Text               as T
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TLE

import           System.Exit             ( ExitCode (..) )
import           System.Process.Typed    ( proc, readProcess )

import           Text.Read               ( readMaybe )

import           Typ                     ( Blk (..) )

hlt :: FilePath -> [Blk] -> IO [Blk]
hlt pth blks = do
   lmp <- treeSitter pth
   pure $ concatMap (subst lmp) blks

subst :: Map Int Text -> Blk -> [Blk]
subst lmp (Cod txt) =
   let lns = T.lines txt
       out = [ case parsePlaceholder l of
                  Just n  -> Map.findWithDefault "" n lmp
                  Nothing -> l
             | l <- lns
             ]
       has = any (not . T.null . T.strip) out
       res = T.dropWhile (== '\n') $ T.dropWhileEnd (== '\n') $ T.intercalate "\n" out
   in  [Cod res | has]
subst _ blk = [blk]

parsePlaceholder :: Text -> Maybe Int
parsePlaceholder t
   | "{{" `T.isPrefixOf` t && "}}" `T.isSuffixOf` t =
      readMaybe (T.unpack (T.drop 2 (T.dropEnd 2 t)))
   | otherwise = Nothing

treeSitter :: FilePath -> IO (Map Int Text)
treeSitter pth = do
   (code, out, err) <- readProcess
      (proc "tree-sitter" ["highlight", "-H", "--css-classes", pth])
   case code of
      ExitFailure _ -> do
         let msg = TL.toStrict (TLE.decodeUtf8 err)
         putStrLn $ "tree-sitter error: " ++ T.unpack msg
         pure Map.empty
      ExitSuccess ->
         pure $ parseOutput (TL.toStrict (TLE.decodeUtf8 out))

parseOutput :: Text -> Map Int Text
parseOutput = go Map.empty
 where
   trS = "<tr><td class=line-number>"
   trM = "</td><td class=line>"
   trE = "</td></tr>"

   go acc txt
      | T.null txt = acc
      | Just aft <- findAfter trS txt =
         case T.breakOn trM aft of
            (num, rst)
               | T.null rst -> acc
               | otherwise ->
                  let rst' = T.drop (T.length trM) rst
                  in  case T.breakOn trE rst' of
                        (cnt, rst'')
                           | T.null rst'' -> acc
                           | otherwise ->
                              case readMaybe (T.unpack (T.strip num)) of
                                 Just n  ->
                                    let cnt' = T.stripSuffix "\n" cnt
                                                 `orElse` cnt
                                    in  go (Map.insert n cnt' acc)
                                           (T.drop (T.length trE) rst'')
                                 Nothing -> go acc (T.drop 1 rst'')
      | otherwise = acc

findAfter :: Text -> Text -> Maybe Text
findAfter needle hay =
   let (_, aft) = T.breakOn needle hay
   in  if T.null aft then Nothing
       else Just (T.drop (T.length needle) aft)

orElse :: Maybe a -> a -> a
orElse (Just x) _ = x
orElse Nothing  y = y
