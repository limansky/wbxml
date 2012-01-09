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
    deriving (Show, Eq)

knownCharsets :: [ (Word32, WbxmlCharset) ]
knownCharsets = [ (106, UTF8) ]

data WbxmlHeader = WbxmlHeader {
          documentVersion :: WbxmlVersion
        , documentPublicId :: Word32
        , documentPublicIndex :: Word32
        , documentCharset :: WbxmlCharset
        , documentTable :: String
    } deriving (Show)

data WbxmlTag = WbxmlTag {
          tagPage :: Word8
        , tagCode :: Word8
        , tagAttrs :: [WbxmlAttribute]
        , tagChildren :: [WbxmlTag]
        , tagValue :: String
    }

data WbxmlAttribute = KnownAttribute {
          attrPage :: Word8
        , attrCode :: Word8
        , attrValues :: [WbxmlAttributeValue]
    }
                    | UnknownAttrute -- TBD
    deriving (Show)

data WbxmlAttributeValue = AttrValueString String
                         | AttrValueKnown Word8 Word8
                         | AttrValueOpaque B.ByteString
    deriving (Show)

data TagContent = Tag WbxmlTag 
                | Str String 
                deriving (Show)

instance Show WbxmlTag where
    show (WbxmlTag p c a ch v) = "{0x" ++ (hex p) ++ ", 0x" ++ (hex c)
                                ++ (showIf (not . null $ a) (", attrs =" ++ show a))
                                ++ (showIf (not . null $ v) (", value: \"" ++ v ++ "\""))
                                ++ (showIf (not . null $ ch) (", " ++ show ch))
                                ++ "}"
        where hex x = showHex x ""
              showIf True v  = v
              showIf False _ = ""

data WbxmlDocument = WbxmlDocument {
      documentHeader :: WbxmlHeader
    , documentRoot :: WbxmlTag
    } deriving (Show)
