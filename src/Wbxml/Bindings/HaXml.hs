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

import Text.XML.HaXml.Types
import Wbxml.Types
import Wbxml.DomBuilder

configHaXml = DomBuilderConfig makeDoc makeElem (flip CElem ()) (\s -> CString False s ())

makeDoc h pid root sid e = Document (Prolog (Just decl) [] doctype []) [] e []
    where decl = XMLDecl ("1.0") (Just . EncodingDecl . show $ documentCharset h) Nothing
          doctype = Just $ DTD (N root) (Just $ PUBLIC (PubidLiteral pid) (SystemLiteral sid)) []

buildHaXmlTree = buildRoot configHaXml
buildHaXmlDocument = buildDocument configHaXml

makeElem (TagInfo _ _ n as) = Elem (N n) attrs
    where attrs = map (\a -> (N $ attrName a, val $ attrValue a)) as
          val (AttrValueString s) = AttValue [Left s]
