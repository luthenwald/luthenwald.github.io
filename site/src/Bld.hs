module Bld (bld) where

import           Control.Monad          ( when )

import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base64 as B64
import           Data.List              ( sortBy )
import           Data.Map.Strict        ( Map )
import qualified Data.Map.Strict        as Map
import           Data.Ord               ( Down (..), comparing )
import           Data.Text              ( Text )
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE
import qualified Data.Text.IO           as TIO

import           Ext                    ( Com (..), Rgn (..), ext )

import           Hlt                    ( hlt )

import qualified Htm

import qualified Met

import           Mrk                    ( mrk )

import           Paths_site             ( getDataFileName )

import qualified Rss

import           Scn                    ( Src (..), scn )

import           System.Directory       ( createDirectoryIfMissing )
import           System.FilePath        ( takeExtension, (</>) )

import           Typ

bld :: FilePath -> FilePath -> Text -> IO ()
bld src out url = do
   putStrLn "scanning src"
   files <- scn src
   when (null files) $ error "no source files found"
   putStrLn $ "found " ++ show (length files) ++ " source files"

   putStrLn "processing source files"
   docs <- mapM processFile files

   let sorted = sortBy (comparing (Down . metPub . docMet)) docs

   putStrLn "creating output directories"
   createDirectoryIfMissing True (out </> "blogs")
   createDirectoryIfMissing True (out </> "tags")
   createDirectoryIfMissing True (out </> "styles")

   let entries = map mkEntry sorted

   putStrLn "generating HTML pages"
   mapM_ (writeBlog out) entries
   writeHome out entries
   writeTags out entries
   writeRss out url entries

   putStrLn "copying CSS"
   writeCss out

   putStrLn "build complete!"

mkEntry :: Doc -> (Doc, [Text], Text)
mkEntry doc = (doc, Met.tags (metTag (docMet doc)), Met.pageId (docPth doc))

processFile :: Src -> IO Doc
processFile src = do
   putStrLn $ "   " ++ srcPth src
   rgns <- ext (srcLng src) (srcPth src)

   let coms = [ c | RCom c <- rgns ]
       metCom = case coms of
          []    -> error $ srcPth src ++ ": no comment blocks"
          (c:_) -> c
   meta <- case Met.met (srcPth src) (comTxt metCom) of
      Left  err -> error err
      Right m   -> pure m

   let contentRgns = filter (not . isFirstCom metCom) rgns
       (blks, out) = mrk (srcPth src) contentRgns

   highlighted <- hlt (srcPth src) blks
   resolved    <- resolve highlighted

   pure Doc
      { docMet = meta
      , docBlk = resolved
      , docOut = out
      , docPth = srcPth src
      }

isFirstCom :: Com -> Rgn -> Bool
isFirstCom c (RCom c') = comBeg c == comBeg c'
isFirstCom _ _         = False

resolve :: [Blk] -> IO [Blk]
resolve = mapM go
 where
   go (Ins pth) = do
      cnt <- TIO.readFile pth
      pure $ Raw ("<div class=\"insert-container\">" <> cnt <> "</div>")
   go (Img pth) = do
      raw <- BS.readFile pth
      let enc  = TE.decodeUtf8 (B64.encode raw)
          mime = mimeOf pth
          uri  = "data:" <> mime <> ";base64," <> enc
      pure $ Raw ("<img src=\"" <> uri <> "\">")
   go blk = pure blk

mimeOf :: FilePath -> Text
mimeOf pth = case takeExtension pth of
   ".png"  -> "image/png"
   ".jpg"  -> "image/jpeg"
   ".jpeg" -> "image/jpeg"
   ".gif"  -> "image/gif"
   ".webp" -> "image/webp"
   ".svg"  -> "image/svg+xml"
   _       -> "application/octet-stream"

writeBlog :: FilePath -> (Doc, [Text], Text) -> IO ()
writeBlog out (doc, tgs, pid) =
   TIO.writeFile (out </> "blogs" </> T.unpack pid ++ ".html")
      (Htm.render (Htm.blogPage doc tgs))

writeHome :: FilePath -> [(Doc, [Text], Text)] -> IO ()
writeHome out entries =
   TIO.writeFile (out </> "index.html")
      (Htm.render (Htm.homePage entries))

writeTags :: FilePath -> [(Doc, [Text], Text)] -> IO ()
writeTags out entries = do
   let tgm = buildTagMap entries
       tgs = sortBy (comparing tagNam) (Map.elems tgm)
   TIO.writeFile (out </> "tags" </> "tagcloud.html")
      (Htm.render (Htm.tagCloud tgs))
   mapM_ (\tag -> do
      let es  = map mkEntry (tagDoc tag)
          pth = out </> "tags" </> T.unpack (tagNam tag) ++ ".html"
      TIO.writeFile pth (Htm.render (Htm.tagPage tag es))
      ) tgs

buildTagMap :: [(Doc, [Text], Text)] -> Map Text Tag
buildTagMap = foldl go Map.empty
 where
   go acc (doc, tgs, _) = foldl (addTag doc) acc tgs
   addTag doc acc tag =
      Map.alter (\case
         Nothing -> Just (Tag tag [doc])
         Just t  -> Just (t { tagDoc = tagDoc t ++ [doc] })
      ) tag acc

writeRss :: FilePath -> Text -> [(Doc, [Text], Text)] -> IO ()
writeRss out url entries = do
   xml <- Rss.rss url entries
   TIO.writeFile (out </> "feed.xml") xml

writeCss :: FilePath -> IO ()
writeCss out = do
   rst <- getDataFileName "assets/reset.css" >>= BS.readFile
   prm <- getDataFileName "assets/prima.css" >>= BS.readFile
   BS.writeFile (out </> "styles" </> "reset.css") rst
   BS.writeFile (out </> "styles" </> "prima.css") prm
