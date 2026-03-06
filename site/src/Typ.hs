-- core models for the site
module Typ where

import           Data.Text ( Text )

data Inl
   = Txt Text
   | Lnk Text Text
   | Bld Text
   | Cde Text
   | Ref Int
   deriving (Show, Eq)

data Blk
   = Hdg Int [Inl] Text
   | Par [Inl]
   | Lst Int [Inl]
   | Cal [Inl]
   | Ftn Int [Inl]
   | Cod Text
   | Vrb Text
   | Raw Text            -- TODO: we already have raw, do we still need Ins, Img?
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
