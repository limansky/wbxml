----------------------------------------------------------------------
-- |
-- Module       : Wbxml.Bindings.HaXml
-- Copyright    : Mike Limansky, 2012
-- Licencse     : BSD3
--
-- Wbxml library bindings to HaXml library
--
----------------------------------------------------------------------

module Wbxml.Bindings.HaXml where

import Wbxml.SAX
import Text.XML.HaXml.Types
import Wbxml.Types
import Wbxml.DomBuilder

-- wbxmlToXML :: [ParseEvent] -> Document i
wbxmlToXML events = Document (Prolog (Just decl) [] doctype []) [] element []
    where decl = XMLDecl ("1.0") (Just $ EncodingDecl "utf-8") Nothing
          doctype = wbxmlDoctype h
          element = makeHaXmlTree c
          (h, c) = break isContent events

isContent (StartTag _ _) = True
isContent _ = False

wbxmlDoctype h = case [ (x, r, d) | (StartDoctype x r d) <- h] of
    [] -> Nothing
    [(pid, root, sid)] -> Just $ DTD (N root) (Just $ PUBLIC (PubidLiteral pid) (SystemLiteral sid)) []

configHaXml = DomBuilderConfig makeElem (flip CElem ()) (\s -> CString False s ())

makeHaXmlTree = wbxmlRoot configHaXml

makeElem (TagInfo _ _ n as) = Elem (N n) attrs
    where attrs = map (\a -> (N $ attrName a, val $ attrValue a)) as
          val (AttrValueString s) = AttValue [Left s]
