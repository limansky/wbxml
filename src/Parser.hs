----------------------------------------------------------------------
-- |
-- Module : WBXML.Tables
-- Copyright : Mike Limansky, 2011
--
-- Definition of WBXML Tables
--
----------------------------------------------------------------------

module Parser where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Attoparsec as A
import Control.Monad (liftM, liftM2)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.State
import Data.Word
import Data.Bits

data WbxmlVersion = Version1_0 | Version1_1 | Version1_2 | Version1_3
    deriving (Show, Eq, Enum)

data WbxmlCharset = UTF8
    deriving (Show, Eq)

data WbxmlHeader = WbxmlHeader {
          documentVersion :: WbxmlVersion
        , documentPublicId :: Word32
        , documentPublicIndex :: Word32
        , documentCharset :: WbxmlCharset
        , documentTable :: String
    } deriving (Show)

data WbxmlDocument = WbxmlDocument {
      documentHeader :: WbxmlHeader
    } deriving (Show)

data ParseState = ParseState {
      codePage :: Int
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

knownCharsets = [ (106, UTF8) ]

parseWbxml s = parse (runStateT parseDocument (ParseState 0) >>= return . fst) s

anyMultiByteWord :: Parser Word32
anyMultiByteWord = do
    bytes <- liftM2 B.snoc (A.takeWhile (\x -> x .&. 0x80 /= 0)) anyWord8
    return $ B.foldl combine 0 bytes
        where combine x y = (x `shiftL` 7) .|. ((fromIntegral y) .&. 0x7f)
              
parseDocument :: WbxmlParser WbxmlDocument
parseDocument = do
    header <- lift parseHeader 
    return $ WbxmlDocument header

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
parseElement = parseSwitchPage

parseSwitchPage :: WbxmlParser ()
parseSwitchPage = do
    lift $ word8 tokenSwitchPage
    page <- lift anyWord8
    st <- get
    put $ st { codePage = fromIntegral page }
    return ()

