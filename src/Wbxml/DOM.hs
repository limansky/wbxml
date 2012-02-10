----------------------------------------------------------------------
-- |
-- Module       : WBXML.DOM
-- Copyright    : Mike Limansky, 2012
-- Licencse     : BSD3
--
-- Implementation of DOM model for WBXML documents
--
----------------------------------------------------------------------

module Wbxml.DOM where

import Wbxml.Types
import Wbxml.DomBuilder
import qualified Data.ByteString as B

data Document = Document WbxmlHeader Tag

data Tag = Tag { tagInfo ::TagInfo
               , tagContent :: [Content]
               }
    deriving (Show)

data Content = CTag Tag
             | CString String
             | CBinary B.ByteString
    deriving (Show)

configDom = DomBuilderConfig Tag CTag CString

buildTree = wbxmlRoot configDom
