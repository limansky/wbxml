module Wbxml.Bindings.HaXml where

import Wbxml.SAX
import Text.XML.HaXml.Types
import Wbxml.Types

wbxmlToXML :: [ParseEvent] -> Document
wbxmlToXML events = Document (Prolog (Just decl) [] doctype []) [] element []
    where decl = XMLDecl ("1.0") (Just $ EncodingDecl "utf-8") Nothing
          doctype = wbxmlDoctype h
          element = wbxmlRoot c
          (h, c) = break isContent events

isContent (StartTag _ _) = True
isContent _ = False

wbxmlDoctype h = case [ (x, r, d) | (StartDoctype x r d) <- h] of
    [] -> Nothing
    [(pid, root, sid)] -> Just $ DTD root (Just $ PUBLIC (PubidLiteral pid) (SystemLiteral sid)) []

wbxmlRoot (e:es) = makeElem tag (makeContent es)
    where (StartTag tag _) = e

makeElem (TagInfo _ _ n as) = Elem n attrs
    where attrs = map (\a -> (attrName a, val $ attrValue a)) as
          val (AttrValueString s) = AttValue [Left s]

makeContent c = fst $ mC [] c

-- TODO: extract common part in the "DOM.Helper" module.
mC c []                            = (c, [])
mC c ((StartTag tag True):es)      = let c' = c ++ [(CElem $ makeElem tag [])] in mC c' es
mC c ((StartTag tag False):es)     = let c' = c ++ [(CElem $ makeElem tag cont)] in mC c' rest
    where (cont, rest) = mC [] es
mC c ((EndTag _):es)               = (c, es)
mC c ((StartText s):es)            = let c' = c ++ [CString False s] in mC c' es
