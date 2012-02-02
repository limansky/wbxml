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

instance Eq Element where
    (Elem n1 _ c1) == (Elem n2 _ c2) = n1 == n2 && c1 == c2

instance Eq Content where
    (CElem e1) == (CElem e2) = e1 == e2
    (CString wss1 s1) == (CString wss2 s2) = wss1 == wss2 && s1 == s2

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

inner_result = Elem "tag1" [] 
    [ CElem $ Elem "tag21" []
        [ CString False "Text21" ]
    , CElem $ Elem "tag22" []
        [ CString False "Text22" ]
    ]

tst_inner = assertBool "Failed!" $ wbxmlRoot inner_events == inner_result
