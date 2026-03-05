-- core models for the site
module Typ where

import           Data.Text ( Text )

-- inline fragments
data Inl
   = Txt Text            -- text
   | Lnk Text Text       -- link: desc, url
   | Bld Text            -- bold
   | Cde Text            -- inline code
   | Ref Int             -- footnote ref
   deriving (Show, Eq)

-- block-level nodes
data Blk
   = Hdg Int [Inl] Text   -- heading: level, content, id
   | Par [Inl]            -- paragraph
   | Lst Int [Inl]        -- unordered list: level, content
   | Cal [Inl]            -- callout
   | Ftn Int [Inl]        -- footnote number, content
   | Cod Text             -- highlighted HTML lines
   | Vrb Text             -- verbatim
   | Raw Text             -- raw HTML (inserts, images) -- TODO: we already have raw, do we still need Ins, Img?
   | Ins FilePath         -- insert
   | Img FilePath         -- image
   deriving (Show, Eq)

-- per-document outline
data Out = Out
   { outTxt :: Text
   , outLvl :: Int
   , outId  :: Text
   } deriving (Show, Eq)

-- per-document metadata
data Met = Met
   { metTtl :: Text       -- title
   , metPub :: Text       -- pubData
   , metTag :: Text       -- tags
   , metDsc :: Text       -- description
   } deriving (Show, Eq)

-- a blog page
data Doc = Doc
   { docMet :: Met
   , docBlk :: [Blk]
   , docOut :: [Out]
   , docPth :: FilePath
   } deriving (Show, Eq)

-- a tag page
data Tag = Tag
   { tagNam :: Text
   , tagDoc :: [Doc]
   } deriving (Show, Eq)
