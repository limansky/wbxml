----------------------------------------------------------------------
-- |
-- Module       : WBXML.Parser
-- Copyright    : Mike Limansky, 2011
-- Licencse     : BSD3
--
-- Implementation of the WBXML parser using Data.Attoparsec
--
----------------------------------------------------------------------

module Wbxml.Parser where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Attoparsec as A
import Control.Monad (liftM, liftM2, when)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.State
import Control.Applicative ((<|>))
import Data.Word
import Data.Char (chr)
import Data.Bits
import Wbxml.Types

newtype ParseState = ParseState {
      codePage :: Word8
    }

type WbxmlParser = StateT ParseState Parser

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

controlTokens = [ tokenSwitchPage, tokenEnd, tokenEntity, tokenStrI, tokenLiteral
                , tokenExtI0, tokenExtI1, tokenExtI2, tokenPI, tokenLiteralC
                , tokenExtT0, tokenExtT1, tokenExtT2, tokenStrT, tokenLiteralA
                , tokenExt0, tokenExt1, tokenExt2, tokenOpaque, tokenLiteralAc]

knownCharsets = [ (106, UTF8) ]

parseWbxml s = parse (runStateT parseDocument (ParseState 0) >>= return . fst) s

anyMultiByteWord :: Parser Word32
anyMultiByteWord = do
    bytes <- liftM2 B.snoc (A.takeWhile (\x -> x .&. 0x80 /= 0)) anyWord8
    return $ B.foldl combine 0 bytes
        where combine x y = (x `shiftL` 7) .|. ((fromIntegral y) .&. 0x7f)

parseNonControlToken = satisfy (not . flip elem controlTokens)

parseIString = do
    word8 tokenStrI
    val <- A.takeWhile (/=0)
    word8 0
    return $ map (chr . fromIntegral) (B.unpack val)

parseDocument :: WbxmlParser WbxmlDocument
parseDocument = do
    header <- lift parseHeader
    (Tag tag) <- parseBody
    return $ WbxmlDocument header tag

-- start        = version publicid charset strtbl
parseHeader = do
    version <- parseVersion
    publicId <- anyMultiByteWord
    publicIndex <- if publicId == 0 then anyMultiByteWord else return 0
    charset <- parseCharset
    table <- parseTable
    return $ WbxmlHeader version publicId publicIndex charset (C.unpack table)

parseVersion = liftM (toEnum . fromIntegral) (satisfy (\v -> v `elem` supportedVersions)
                <?> "Unsupported version")
    where supportedVersions = [2, 3]

parseCharset = do
    charsetId <-anyMultiByteWord
    case lookup charsetId knownCharsets of
        Just c    -> return c
        otherwise -> fail "Unsupported charset"

parseTable = do
    length <- anyMultiByteWord
    A.take $ fromIntegral length

-- TODO: implement attributes parser
-- body     = *pi element *pi
parseBody = parseElement

-- element      = stag [ 1*attribute END ] [ *content END ]
parseElement = do
    skipMany parseSwitchPage
    parseTag

parseSwitchPage = do
    lift $ word8 tokenSwitchPage
    page <- lift anyWord8
    st <- get
    put $ st { codePage = fromIntegral page }
    return ()

parseTag = do
    (ParseState page) <- get
    code <- lift $ parseNonControlToken
    attrs <- if (code .&. 0x80 /= 0) then lift $ parseAttrs page else return []
    content <- if (code .&. 0x40 /= 0) then do
                                            c <- (many $ choice [ lift parseStringContent
                                                                , parseElement ])
                                            lift $ skip (==tokenEnd)
                                            return c
                                       else return []
    let chld = [ x | Tag x <- content ]
        val  = concat $ [ x | Str x <- content ]
    return . Tag $ WbxmlTag page (code .&. 0x3f) attrs chld val

parseStringContent = parseIString >>= return . Str

parseAttrs page = do
    attrs <- many $ parseAttr page
    word8 tokenEnd
    return attrs

--  attribute   = attrStart *attrValue
parseAttr page = do
    attr <- parseAttrStart
    value <- parseAttrValue page
    return $ KnownAttribute page attr value

--  attrStart   = ATTRSTART | ( LITERAL index )
parseAttrStart = satisfy (\c -> c < 128 && not (c `elem` controlTokens)) -- FIXME unknown attributes are not supported

--  attrValue   = ATTRVALUE | string | extension | entity
parseAttrValue page = many $ parseStringAttrValue 
                         <|> parseOpaqueAttrValue
                         <|> parseKnownAttrValue page

parseKnownAttrValue page = satisfy (\c -> c >= 128 && not (c `elem` controlTokens)) >>= \c -> return $ AttrValueKnown page c
parseStringAttrValue = parseIString >>= return . AttrValueString
parseOpaqueAttrValue = parseOpaqueData >>= return . AttrValueOpaque

parseOpaqueData = do
    word8 tokenOpaque
    len <- anyMultiByteWord
    A.take $ fromIntegral len
