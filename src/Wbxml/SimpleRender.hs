----------------------------------------------------------------------
-- |
-- Module       : WBXML.SimpleRenderer
-- Copyright    : Mike Limansky, 2011
-- Licencse     : BSD3
--
-- Simple renderer to XML string, without any external dependencies.
--
----------------------------------------------------------------------

module Wbxml.SimpleRender where

import Wbxml.Tables
import Wbxml.Types
import Data.List (intercalate)
import Data.Maybe (fromMaybe, fromJust)
import qualified Data.ByteString as B

renderWbxml :: WbxmlDocument -> Either String String
renderWbxml d = case findTables d of
    Nothing -> Left $ "Table not found: pid=" ++ show pid
    Just (_, _, _, _, t, a, av) -> Right $ renderWbxmlWithTable d t a av
    where pid = documentPublicId . documentHeader $ d

renderWbxmlWithTable :: WbxmlDocument -> WbxmlTagTable -> WbxmlAttrTable -> WbxmlAttrValueTable -> String
renderWbxmlWithTable d t a av = (renderHeader d) ++ renderWbxmlTree (documentRoot d) t a av 0

renderHeader d = "<?xml version=\"1.0\"?>\n" ++ (showDocType . fromJust $ findTables d)
    where showDocType (i, xid, root, dtd, _, _, _) = "<!DOCTYPE " ++ root ++ " PUBLIC \"" ++ xid ++ "\" \"" ++ dtd ++"\"!>\n"

renderWbxmlTree tag@(WbxmlTag _ _ _ [] "") t ta tav n = (replicate n ' ') ++ fst (renderName t ta tav tag True) ++ "\n"
renderWbxmlTree tag@(WbxmlTag _ _ _ [] v ) t ta tav n = (replicate n ' ') ++ open ++ v ++ close ++ "\n"
    where (open, close) = renderName t ta tav tag False
renderWbxmlTree tag@(WbxmlTag _ _ _ ch "") t ta tav n = indent ++ open ++ "\n"
                                        ++ concat (map (\x -> renderWbxmlTree x t ta tav (n + 1)) ch) 
                                        ++ indent ++ close ++ "\n"
    where (open, close) = renderName t ta tav tag False
          indent = replicate n ' '

renderName t ta tav (WbxmlTag p c a _ _) closed = (open, close)
    where open  = "<" ++ name ++ attrs ++ r
          close = if closed then ""   else "</" ++ name ++ ">"
          r     = if closed then "/>" else ">"
          attrs = if null a then ""   else " " ++ (renderAttrs ta tav a)
          name  = tagName t p c

renderAttrs ta tav attrs = intercalate " " $ map (renderAttr ta tav) attrs 

renderAttr ta tav (KnownAttribute p c values) = name ++ "=\"" ++ v ++ "\""
    where (name, vstart) = attrName ta p c
          v = vstart ++ (concat $ map (renderValue tav) values)

renderValue _ (AttrValueString s) = s
renderValue t (AttrValueKnown p c) = attrValue t p c
renderValue _ (AttrValueOpaque d) = "Opaque: size=" ++ (show $ B.length d)

tagName t p c = fromMaybe ("Tag(" ++ (show p) ++ ", " ++ (show c) ++")") (findTag t p c)
attrName t p c = fromMaybe (("attr(" ++ (show p) ++ ", " ++ (show c) ++")"), "") (findAttr t p c)
attrValue t p c = fromMaybe ("val(" ++ (show p) ++ ", " ++ (show c) ++")") (findValue t p c)
