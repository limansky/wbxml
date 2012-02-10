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

data DomBuilderConfig a b = DomBuilderConfig { makeTag :: (TagInfo -> [b] -> a)
                                             , makeContentTag :: (a -> b)
                                             , makeContentString :: (String -> b)
                                             }

wbxmlRoot conf (e:es) = (makeTag conf) tag (makeContent conf es)
    where (StartTag tag _) = e

makeContent conf c = fst $ mC [] c
    where
        mC c []                            = (c, [])
        mC c ((StartTag tag True):es)      = let c' = c ++ [makeContentTag conf (makeTag conf tag [])] in mC c' es
        mC c ((StartTag tag False):es)     = let c' = c ++ [makeContentTag conf (makeTag conf tag cont)] in mC c' rest
            where (cont, rest) = mC [] es
        mC c ((EndTag _):es)               = (c, es)
        mC c ((StartText s):es)            = let c' = c ++ [makeContentString conf s] in mC c' es
