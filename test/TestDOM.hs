----------------------------------------------------------------------
-- |
-- Module       : TestDOM
-- Copyright    : Mike Limansky, 2012
-- Licencse     : BSD3
--
-- Unit tests for Wbxml DOM model
--
----------------------------------------------------------------------

module TestDOM
(
    tst_build_sax
)
where

import Test.HUnit
import Wbxml.SAX
import Wbxml.Types
import Wbxml.DOM

tag1 = TagInfo 0 5 "Tag1" []
tag2 = TagInfo 1 5 "Tag2" []
text2 = "And now, the text!"

dom = Tag tag1 [CTag $ Tag tag2 [CString text2 ]]
events = [ StartTag tag1 False
         , StartTag tag2 False
         , Text text2
         , EndTag tag2
         , EndTag tag1]

tst_build_sax = buildTree events @=? dom
