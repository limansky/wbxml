module TestBindingHaXml 
( 
    tst_inner
)
where

import Test.HUnit
import Text.XML.HaXml.Types

import Wbxml.SAX
import Wbxml.Types
import Wbxml.Bindings.HaXml

inner_events = [ StartTag tag1 False
               , StartTag tag21 False
               , StartText "Text21"
               , EndTag tag21
               , StartTag tag22 False
               , StartText "Text22"
               , EndTag tag22
               , EndTag tag1
    ]
    where tag1 = TagInfo 0 5 "tag1" []
          tag21 = TagInfo 0 6 "tag21" []
          tag22 = TagInfo 0 6 "tag22" []

inner_result = Elem (N "tag1") [] 
    [ CElem (Elem (N "tag21") [] [ CString False "Text21" () ]) ()
    , CElem (Elem (N "tag22") [] [ CString False "Text22" () ]) ()
    ]

tst_inner = assertBool "Failed!" $ wbxmlRoot inner_events == inner_result
