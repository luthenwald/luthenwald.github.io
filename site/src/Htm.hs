module Htm (blogPage, homePage, tagCloud, tagPage, render) where

import           Control.Monad                 ( forM_, when )

import           Data.Text                     ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as TL

import           Text.Blaze.Html.Renderer.Text ( renderHtml )
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5              ( Html, preEscapedText,
                                                 textValue, toHtml, (!) )
import qualified Text.Blaze.Html5.Attributes   as A

import           Typ

data Nav = Root | InTags | InBlogs

render :: Html -> Text
render = TL.toStrict . renderHtml

-- public page generators

blogPage :: Doc -> [Text] -> Html
blogPage doc tgs = page (metTtl (docMet doc)) "../styles/" "blog-page" $ do
   H.aside ! A.class_ "outline-sidebar" $ do
      H.nav ! A.class_ "outline" $
         mapM_ outItem (docOut doc)
      botNav InBlogs
   H.main ! A.class_ "main-content" $ do
      H.h1 $ toHtml (metTtl (docMet doc))
      H.div ! A.class_ "metadata" $ do
         H.span ! A.class_ "date" $ toHtml (metPub (docMet doc))
         H.span ! A.class_ "tags" $ tagLinks "../tags/" tgs
      H.div ! A.class_ "description" $ toHtml (metDsc (docMet doc))
      mapM_ blkHtm (docBlk doc)

homePage :: [(Doc, [Text], Text)] -> Html
homePage docs = page "home" "styles/" "home-page" $ do
   H.aside ! A.class_ "outline-sidebar" $ do
      H.div ! A.class_ "spacer" $ mempty
      botNav Root
   H.main ! A.class_ "main-content" $ do
      H.h1 "all posts"
      mapM_ (\(d, t, p) -> blogEntry "blogs/" "tags/" d t p) docs

tagCloud :: [Tag] -> Html
tagCloud tgs = page "tags" "../styles/" "tagcloud-page" $ do
   H.aside ! A.class_ "outline-sidebar" $ do
      H.div ! A.class_ "spacer" $ mempty
      botNav InTags
   H.main ! A.class_ "main-content" $ do
      H.h1 "tag cloud"
      H.div ! A.class_ "tag-cloud" $
         mapM_ tagItem tgs

tagPage :: Tag -> [(Doc, [Text], Text)] -> Html
tagPage tag entries = page ("tag: " <> tagNam tag) "../styles/" "tag-page" $ do
   H.aside ! A.class_ "outline-sidebar" $ do
      H.div ! A.class_ "spacer" $ mempty
      botNav InTags
   H.main ! A.class_ "main-content" $ do
      H.h1 $ toHtml ("tag: " <> tagNam tag)
      mapM_ (\(d, t, p) -> blogEntry "../blogs/" "../tags/" d t p) entries

-- page shell

page :: Text -> Text -> Text -> Html -> Html
page ttl css cls bdy = do
   H.docType
   H.html ! A.lang "en" ! H.customAttribute "data-theme" "light" $ do
      H.head $ do
         H.meta ! A.charset "UTF-8"
         H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1.0"
         H.title (toHtml ttl)
         H.link ! A.rel "stylesheet" ! A.href (textValue (css <> "reset.css"))
         H.link ! A.rel "stylesheet" ! A.href (textValue (css <> "prima.css"))
      H.body ! A.class_ (textValue cls) $ do
         bdy
         themeScript

-- navigation

botNav :: Nav -> Html
botNav nav = H.nav ! A.class_ "bottom-nav" $ do
   let (hm, tg, fd) = navHrefs nav
   H.a ! A.href (textValue hm) $ "Home"
   H.a ! A.href (textValue tg) $ "Tags"
   H.a ! A.href (textValue fd) $ "Feed"
   H.a ! A.href "#" ! A.id "theme-toggle" ! A.title "toggle theme" $ "Theme"

navHrefs :: Nav -> (Text, Text, Text)
navHrefs Root    = ("index.html",    "tags/tagcloud.html",    "feed.xml")
navHrefs InTags  = ("../index.html", "tagcloud.html",         "../feed.xml")
navHrefs InBlogs = ("../index.html", "../tags/tagcloud.html", "../feed.xml")

outItem :: Out -> Html
outItem o =
   H.a ! A.href (textValue ("#" <> outId o))
       ! A.class_ (textValue ("h" <> T.pack (show (outLvl o))))
       $ toHtml (outTxt o)

tagLinks :: Text -> [Text] -> Html
tagLinks pfx tgs = forM_ (zip [0 :: Int ..] tgs) $ \(i, t) -> do
   when (i > 0) $ toHtml (", " :: Text)
   H.a ! A.href (textValue (pfx <> t <> ".html")) $ toHtml t

blogEntry :: Text -> Text -> Doc -> [Text] -> Text -> Html
blogEntry blgPfx tagPfx doc tgs pid = H.article ! A.class_ "blog-entry" $ do
   H.h1 $ H.a ! A.href (textValue (blgPfx <> pid <> ".html")) $ toHtml (metTtl (docMet doc))
   H.div ! A.class_ "date" $ toHtml (metPub (docMet doc))
   H.div ! A.class_ "description" $ toHtml (metDsc (docMet doc))
   H.div ! A.class_ "tags" $ tagLinks tagPfx tgs

tagItem :: Tag -> Html
tagItem tag =
   H.a ! A.href (textValue (tagNam tag <> ".html")) $
      toHtml (tagNam tag <> "(" <> T.pack (show (length (tagDoc tag))) <> ")")

-- block rendering

blkHtm :: Blk -> Html
blkHtm (Hdg lvl cnt eid) = hN lvl ! A.id (textValue eid) $ inlHtm cnt
blkHtm (Par cnt)          = H.p $ inlHtm cnt
blkHtm (Lst lvl cnt)      = H.p ! A.class_ (textValue ("l" <> T.pack (show lvl) <> "-list")) $ inlHtm cnt
blkHtm (Cal cnt)          = H.blockquote ! A.class_ "callout" $ inlHtm cnt
blkHtm (Ftn n cnt)        = H.div ! A.class_ "footnote" ! A.id (textValue ("fn-" <> T.pack (show n))) $ do
                                H.sup $ toHtml (T.pack (show n))
                                " "
                                inlHtm cnt
blkHtm (Cod txt)          = H.pre $ H.code $ preEscapedText txt
blkHtm (Vrb txt)          = H.pre ! A.class_ "verbatim" $ H.code $ toHtml txt
blkHtm (Raw txt)          = H.div ! A.class_ "raw-block" $ preEscapedText txt
blkHtm (Ins _)             = mempty
blkHtm (Img _)             = mempty

hN :: Int -> Html -> Html
hN 1 = H.h1
hN 2 = H.h2
hN 3 = H.h3
hN 4 = H.h4
hN 5 = H.h5
hN _ = H.h6

-- inline rendering

inlHtm :: [Inl] -> Html
inlHtm = mapM_ go
 where
   go (Txt t)   = toHtml t
   go (Lnk t u) = H.a ! A.href (textValue u) $ toHtml t
   go (Bld t)   = H.strong $ toHtml t
   go (Cde t)   = H.code ! A.class_ "inlinecode" $ toHtml t
   go (Ref n)   = H.sup $ H.a ! A.href (textValue ("#fn-" <> T.pack (show n))) $
                     toHtml (T.pack (show n))

-- theme script

themeScript :: Html
themeScript = H.script $ preEscapedText
   "document.addEventListener('DOMContentLoaded', function() {\
   \  var themeToggle = document.getElementById('theme-toggle');\
   \  var html = document.documentElement;\
   \  var savedTheme = localStorage.getItem('theme') || 'light';\
   \  html.setAttribute('data-theme', savedTheme);\
   \  themeToggle.addEventListener('click', function(e) {\
   \    e.preventDefault();\
   \    var currentTheme = html.getAttribute('data-theme');\
   \    var newTheme = currentTheme === 'light' ? 'dark' : 'light';\
   \    html.setAttribute('data-theme', newTheme);\
   \    localStorage.setItem('theme', newTheme);\
   \  });\
   \});"
