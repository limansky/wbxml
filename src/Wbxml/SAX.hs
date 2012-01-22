----------------------------------------------------------------------
-- |
-- Module       : WBXML.Parser
-- Copyright    : Mike Limansky, 2011
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
import Control.Applicative ((<|>))
import Data.Word
import Data.Char (chr)
import Data.Bits
import Wbxml.Types
import Wbxml.WbxmlDefs
import Wbxml.Tables

data ParseState = ParseState {
      codePage :: Word8
    , tagStack :: [TagInfo]
    }
    deriving (Show)

type WbxmlParser = StateT ParseState Parser

data ParseEvent = StartTag TagInfo
                | EndTag TagInfo
                | StartText String
                | StartBinary B.ByteString
                | StartDoctype
                deriving (Show)

parseWbxml s = parseOnly (runStateT parseDocument (ParseState 0 []) >>= return . fst) s

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

parseDocument :: WbxmlParser [ParseEvent]
parseDocument = do
    header <- lift parseHeader
    let (Just t) = findTables header
    body <- parseBody t
    return $ StartDoctype : body

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
parseBody :: WbxmlTableDef -> WbxmlParser [ParseEvent]
parseBody t = do
    e@(StartTag (TagInfo _ _ _ _ c)) <- parseElement t
    if c then return [e]
         else liftM (e:) (many (parseContent t))

-- element      = stag [ 1*attribute END ] [ *content END ]
parseElement t = do
    skipMany parseSwitchPage
    parseTag t

parseSwitchPage = do
    lift $ word8 tokenSwitchPage
    page <- lift anyWord8
    st <- get
    put $ st { codePage = fromIntegral page }
    return ()

parseTag t = do
    token <- lift $ parseNonControlToken
    (ParseState page st) <- get
    attrs <- if (token .&. 0x80 /= 0) then lift $ parseAttrs t page else return []
    let code = token .&. 0x3f
        closed = token .&. 0x40 == 0
    case findTag t page code of
        Just name -> do
                        let tag = TagInfo page code attrs name closed
                        when (not closed) (put $ ParseState page (tag:st))
                        return $ StartTag tag
        Nothing   -> fail $ "Unknown tag: {" ++ (show page) ++ ", " ++ (show code) ++ "}"

parseEndTag = do
    lift $ word8 tokenEnd
    (ParseState p (t:ts)) <- get
    put $ ParseState p ts
    return $ EndTag t

-- content      = element | string | extension | entity | pi | opaque
parseContent t = parseElement t
             <|> parseEndTag
             <|> lift parseStringContent
             <|> lift parseOpaqueContent

parseStringContent = parseIString >>= return . StartText

parseOpaqueContent = parseOpaqueData >>= return . StartBinary

parseAttrs t page = do
    attrs <- many $ parseAttr t page
    word8 tokenEnd
    return attrs

--  attribute   = attrStart *attrValue
parseAttr t page = do
    attr <- parseAttrStart
    value <- parseAttrValue t page
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
parseAttrValue t page = parseBinaryAttrValue <|> (parseStringAttrValue t page)

parseStringAttrValue t page = liftM concat (many $ parseIString <|> (parseKnownAttrValue t page)) >>= return . AttrValueString 

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
