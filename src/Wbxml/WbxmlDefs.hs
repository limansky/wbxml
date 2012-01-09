----------------------------------------------------------------------
-- |
-- Module       : WBXML.WbxmlDefs
-- Copyright    : Mike Limansky, 2011
-- Licencse     : BSD3
--
-- This module contains definition of different contstants defined in
-- WBXML standard.
--
----------------------------------------------------------------------

module Wbxml.WbxmlDefs where

import Data.Word (Word8)

tokenSwitchPage = 0x0
tokenEnd = 0x1
tokenEntity = 0x2
tokenStrI = 0x3
tokenLiteral = 0x4
tokenExtI0 = 0x40
tokenExtI1 = 0x41
tokenExtI2 = 0x42
tokenPI = 0x43
tokenLiteralC = 0x44
tokenExtT0 = 0x80
tokenExtT1 = 0x81
tokenExtT2 = 0x82
tokenStrT = 0x83
tokenLiteralA = 0x84
tokenExt0 = 0xC0
tokenExt1 = 0xC1
tokenExt2 = 0xC2
tokenOpaque = 0xC3
tokenLiteralAc = 0xC4

controlTokens :: [ Word8 ]
controlTokens = [ tokenSwitchPage, tokenEnd, tokenEntity, tokenStrI, tokenLiteral
                , tokenExtI0, tokenExtI1, tokenExtI2, tokenPI, tokenLiteralC
                , tokenExtT0, tokenExtT1, tokenExtT2, tokenStrT, tokenLiteralA
                , tokenExt0, tokenExt1, tokenExt2, tokenOpaque, tokenLiteralAc]

