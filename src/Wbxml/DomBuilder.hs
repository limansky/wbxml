----------------------------------------------------------------------
-- |
-- Module       : WBXML.DomBuilder
-- Copyright    : Mike Limansky, 2012
-- Licencse     : BSD3
--
-- Helper module for building different DOM documents
--
----------------------------------------------------------------------

module Wbxml.DomBuilder where

import Wbxml.SAX
import Wbxml.Types

data DomBuilderConfig d t c = DomBuilderConfig { makeDocument :: (WbxmlHeader -> String -> String -> String -> t -> d)
                                             , makeTag :: (TagInfo -> [c] -> t)
                                             , makeContentTag :: (t -> c)
                                             , makeContentString :: (String -> c)
                                             }

buildDocument conf ((Document h):(Doctype p r s):es) = Just $ makeDocument conf h p r s (buildRoot conf es)
buildDocument _ _ = Nothing

buildRoot conf (e:es) = (makeTag conf) tag (buildContent conf es)
    where (StartTag tag _) = e

buildContent conf c = fst $ mC [] c
    where
        mC c []                            = (c, [])
        mC c ((StartTag tag True):es)      = let c' = c ++ [makeContentTag conf (makeTag conf tag [])] in mC c' es
        mC c ((StartTag tag False):es)     = let c' = c ++ [makeContentTag conf (makeTag conf tag cont)] in mC c' rest
            where (cont, rest) = mC [] es
        mC c ((EndTag _):es)               = (c, es)
        mC c ((Text s):es)            = let c' = c ++ [makeContentString conf s] in mC c' es
