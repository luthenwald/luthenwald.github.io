module Typ where

import           Data.Text ( Text )

data Inl
   = Txt Text
   | Lnk Text Text    -- text, url
   | Bld Text          -- bold
   | Cde Text          -- inline code
   | Ref Int           -- footnote ref
   deriving (Show, Eq)

data Blk
   = Hdg Int [Inl] Text  -- level, content, id
   | Par [Inl]
   | Lst Int [Inl]        -- level, content
   | Cal [Inl]            -- callout
   | Ftn Int [Inl]        -- footnote number, content
   | Cod Text             -- highlighted HTML lines
   | Vrb Text             -- verbatim
   | Raw Text             -- raw HTML (inserts, images)
   | Ins FilePath
   | Img FilePath
   deriving (Show, Eq)

data Out = Out
   { outTxt :: Text
   , outLvl :: Int
   , outId  :: Text
   } deriving (Show, Eq)

data Met = Met
   { metTtl :: Text
   , metPub :: Text
   , metTag :: Text
   , metDsc :: Text
   } deriving (Show, Eq)

data Doc = Doc
   { docMet :: Met
   , docBlk :: [Blk]
   , docOut :: [Out]
   , docPth :: FilePath
   } deriving (Show, Eq)

data Tag = Tag
   { tagNam :: Text
   , tagDoc :: [Doc]
   } deriving (Show, Eq)
