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

import Wbxml.SAX
import Wbxml.Types
import Data.List (mapAccumL, intercalate)
import Data.Maybe (fromMaybe, fromJust)
import qualified Data.ByteString as B

renderWbxml :: [ParseEvent] -> String
renderWbxml e = unlines (xmlHeader : (snd $ mapAccumL renderEvent 0 e))
xmlHeader = "<?xml version=\"1.0\" encoding=\"utf-8\"?>"

renderEvent n (StartTag (TagInfo _ _ name a c)) =
    if c then (n,     (replicate n ' ') ++ "<" ++ name ++ (renderAttrs a) ++ "/>")
         else (n + 1, (replicate n ' ') ++ "<" ++ name ++ (renderAttrs a) ++ ">")
renderEvent n (EndTag (TagInfo _ _ name _ _)) = 
              (n - 1, (replicate (n - 1) ' ') ++ "</" ++ name ++ ">")
renderEvent n (StartText s) = (n, (replicate n ' ') ++ s)
renderEvent n (StartDoctype i r d) = (n, "<!DOCTYPE " ++ r 
                                         ++ " PUBLIC \"" ++ i ++ "\" \"" ++ d ++ "\"!>")
renderEvent n _ = (n, "")

renderAttrs [] = ""
renderAttrs a  = " " ++ (intercalate " " $ map renderAttr a)

renderAttr a = attrName a ++ "=\"" ++ (renderAttrValue $ attrValue a) ++ "\""

renderAttrValue (AttrValueString s) = s
renderAttrValue (AttrValueBinary b) = "binary size=" ++ (show $ B.length b)
