----------------------------------------------------------------------
-- |
-- Module       : Wbxml.SAX
-- Copyright    : Mike Limansky, 2012
-- Licencse     : BSD3
--
-- Implementation of the SAX WBXML parser using Data.Attoparsec
--
----------------------------------------------------------------------

module Wbxml.SAX where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Attoparsec as A
import Control.Monad (liftM, liftM2, when)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.State
import Control.Applicative ((<|>), many)
import Data.Word
import Data.Char (chr)
import Data.Bits
import Wbxml.Types
import Wbxml.WbxmlDefs
import Wbxml.Tables

data ParseState = ParseState {
      tagCodePage :: Word8
    , attrCodePage :: Word8
    , tagStack :: [TagInfo]
    }
    deriving (Show)

type WbxmlParser = StateT ParseState Parser

data ParseEvent = StartTag TagInfo Bool
                | EndTag TagInfo
                | StartText String
                | StartBinary B.ByteString
                | StartDoctype String String String
                deriving (Show)

parseWbxml s = parseOnly (runStateT parseDocument (ParseState 0 0 []) >>= return . fst) s

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
    return $ C.unpack val

parseTString table = do
    word8 tokenStrT
    idx <- anyMultiByteWord
    return $ getStringFromTable (fromIntegral idx) table


parseDocument :: WbxmlParser [ParseEvent]
parseDocument = do
    header <- lift parseHeader
    let (Just t@(_, xid, root, dtd, _, _, _)) = findTables header
    body <- parseBody t (documentTable header)
    return $ (StartDoctype xid root dtd) : body

-- start        = version publicid charset strtbl
parseHeader = do
    version <- parseVersion
    publicId <- parsePublicId
    charset <- parseCharset
    table <- parseTable
    return $ WbxmlHeader version publicId charset (C.unpack table)

-- publicid = mb_u_int32 | ( zero index )
parsePublicId = do
    id <- anyMultiByteWord
    if id /= 0 then return $ KnownPublicId id
               else anyMultiByteWord >>= return . StringPublicId

-- version      = u_int8 containing WBXML version number
parseVersion = liftM (toEnum . fromIntegral) (satisfy (\v -> v `elem` supportedVersions)
                <?> "Unsupported version")
    where supportedVersions = [2, 3]

-- charset      = mb_u_int32
parseCharset = do
    charsetId <- anyMultiByteWord
    case lookup charsetId knownCharsets of
        Just c    -> return c
        otherwise -> fail "Unsupported charset"

-- strtbl = length *byte
parseTable = do
    length <- anyMultiByteWord
    A.take $ fromIntegral length

-- body     = *pi element *pi
parseBody :: WbxmlTableDef -> String -> WbxmlParser [ParseEvent]
parseBody t strTable = do
    e@(StartTag _ c) <- parseElement t strTable
    if c then return [e]
         else liftM (e:) (parseContents t strTable)

parseContents :: WbxmlTableDef -> String -> WbxmlParser [ParseEvent]
parseContents t s = do
    c <- parseContent t s
    (ParseState _ _ tags) <- get
    if null tags then return [c] 
                 else liftM (c:) (parseContents t s)

-- element      = stag [ 1*attribute END ] [ *content END ]
parseElement t s = do
    skipMany parseTagSwitchPage
    parseTag t s

parseTagSwitchPage = parseSwitchPage (\page state -> state { tagCodePage = fromIntegral page})

parseAttrSwitchPage = parseSwitchPage (\page state -> state { attrCodePage = fromIntegral page})

parseSwitchPage f = do
    lift $ word8 tokenSwitchPage
    page <- lift anyWord8
    modify (f page)
    return ()

parseTag t s = do
    token <- lift $ parseNonControlToken
    (ParseState page apage st) <- get
    attrs <- if (token .&. 0x80 /= 0) then parseAttrs t s else return []
    let code = token .&. 0x3f
        closed = token .&. 0x40 == 0
    case findTag t page code of
        Just name -> do
                        let tag = TagInfo page code name attrs
                        when (not closed) (put $ ParseState page apage (tag:st))
                        return $ StartTag tag closed
        Nothing   -> fail $ "Unknown tag: {" ++ (show page) ++ ", " ++ (show code) ++ "}"

parseEndTag = do
    lift $ word8 tokenEnd
    (ParseState p ap (t:ts)) <- get
    put $ ParseState p ap ts
    return $ EndTag t

-- content      = element | string | extension | entity | pi | opaque
parseContent t s = parseElement t s
             <|> parseEndTag
             <|> (lift $ parseStringContent s)
             <|> lift parseOpaqueContent

parseStringContent s = parseIString <|> parseTString s >>= return . StartText

parseOpaqueContent = parseOpaqueData >>= return . StartBinary

parseAttrs t s = do
    attrs <- many1 $ parseAttr t s
    lift $ word8 tokenEnd
    return attrs

--  attribute   = attrStart *attrValue
parseAttr t s = do
    skipMany parseAttrSwitchPage
    (ParseState _ page _) <- get
    attr <- lift $ parseAttrStart
    value <- lift $ parseAttrValue t s page
    case findAttr t page attr of
        Just (name, val) ->
            let v = case value of 
                        AttrValueString s -> AttrValueString (val ++ s)
                        _                 -> value
            in return $ KnownAttribute page attr name v
        Nothing          -> fail $ "Unknown attribute: {" ++ (show page) ++ ", " ++ (show attr) ++ "}"

--  attrStart   = ATTRSTART | ( LITERAL index )
parseAttrStart = satisfy (\c -> c < 128 && not (c `elem` controlTokens)) -- FIXME unknown attributes are not supported

--  attrValue   = ATTRVALUE | string | extension | entity
parseAttrValue t s page = parseBinaryAttrValue <|> (parseStringAttrValue t s page)

parseStringAttrValue t s page = liftM concat (many $ parseIString
                                                    <|> parseTString s
                                                    <|> (parseKnownAttrValue t page))
                              >>= return . AttrValueString

parseKnownAttrValue t page = do 
    token <- satisfy (\c -> c >= 128 && not (c `elem` controlTokens))
    case findValue t page token of
        Just v  -> return v
        Nothing -> fail $ "Unknown attribute value: {" ++ (show page) ++ ", " ++ (show t) ++ "}"

parseBinaryAttrValue = parseOpaqueData >>= return . AttrValueBinary

parseOpaqueData = do
    word8 tokenOpaque
    len <- anyMultiByteWord
    A.take $ fromIntegral len
