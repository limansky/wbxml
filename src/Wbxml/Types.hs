----------------------------------------------------------------------
-- |
-- Module       : WBXML.Types
-- Copyright    : Mike Limansky, 2011
-- Licencse     : BSD3
--
-- Definition of types, represents parsed WBXML tree. This types
-- represents low level document structure.
--
----------------------------------------------------------------------

module Wbxml.Types where

import Data.Word
import qualified Data.ByteString as B
import Numeric (showHex)

data WbxmlVersion = Version1_0 | Version1_1 | Version1_2 | Version1_3
    deriving (Show, Eq, Enum)

data WbxmlCharset = UTF8
    deriving (Eq)

instance Show WbxmlCharset where
    show UTF8 = "utf-8"

knownCharsets :: [ (Word32, WbxmlCharset) ]
knownCharsets = [ (106, UTF8) ]

data WbxmlPublicId = KnownPublicId Word32 | StringPublicId Word32
    deriving (Show)

data WbxmlHeader = WbxmlHeader {
          documentVersion :: WbxmlVersion
        , documentPublicId :: WbxmlPublicId
        , documentCharset :: WbxmlCharset
        , documentTable :: String
    } deriving (Show)

getStringFromTable pos hdr = takeWhile ( /= '\NUL') . drop pos . documentTable $ hdr

data TagInfo = TagInfo {
          tagPage :: Word8
        , tagCode :: Word8
        , tagName :: String
        , tagAttrs :: [WbxmlAttribute]
    }

data WbxmlAttribute = KnownAttribute {
          attrPage :: Word8
        , attrCode :: Word8
        , attrName :: String
        , attrValue :: WbxmlAttributeValue
    }
                    | UnknownAttrute -- TBD
    deriving (Show)

data WbxmlAttributeValue = AttrValueString String
                         | AttrValueBinary B.ByteString
    deriving (Show)

data RawAttributeValue = RawValueString String
                       | RawValueKnown Word8 Word8
                       | RawValueOpaque B.ByteString

instance Show TagInfo where
    show (TagInfo p c n a) = "{" ++ n ++ " 0x" ++ (hex p) ++ ", 0x" ++ (hex c)
                                ++ (showIf (not . null $ a) (", attrs =" ++ show a))
                                ++ "}"
        where hex x = showHex x ""
              showIf True v  = v
              showIf False _ = ""

{-data TagContent = Tag WbxmlTag 
                | Str String 
                deriving (Show)
-}
{-data WbxmlDocument = WbxmlDocument {
      documentHeader :: WbxmlHeader
    , documentRoot :: WbxmlTag
    } deriving (Show)-}
