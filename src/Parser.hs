----------------------------------------------------------------------
-- |
-- Module : WBXML.Tables
-- Copyright : Mike Limansky, 2011
--
-- Definition of WBXML Tables
--
----------------------------------------------------------------------

module Parser where

import qualified Data.ByteString.Lazy as L
import Data.Attoparsec
import Data.Attoparsec.Binary (anyWord32be)
import Control.Monad (liftM)
import Data.Word

data WbxmlVersion = Version1_0 | Version1_1 | Version1_2 | Version1_3
    deriving (Show, Eq, Enum)

data WbxmlDocument = WbxmlDocument {
          documentVersion :: WbxmlVersion
        , documentPublicId :: Word32
        , documentPublicIndex :: Word32
    } deriving (Show)

parseDocument = do
    version <- parseVersion
    publicId <- anyWord32be
    publicIndex <- if publicId == 0 then anyWord32be else return 0
    return $ WbxmlDocument version publicId publicIndex

parseVersion = liftM (toEnum . fromIntegral) (satisfy supportedVersion
                <?> "Unsupported version")
    where supportedVersion w = 0 <= w && w <= 3
