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
import Control.Monad (liftM)

data WbxmlVersion = Version1_0 | Version1_1 | Version1_2 | Version1_3
    deriving (Show, Eq, Enum)

data WbxmlDocument = WbxmlDocument {
          documentVersion :: WbxmlVersion
        , decumentVersionId :: Int
    } deriving (Show)

parseDocument = do
    version <- parseVersion
    return $ WbxmlDocument version 0

parseVersion = liftM (toEnum . fromIntegral) (satisfy supportedVersion
                <?> "Unsupported version")
    where supportedVersion w = 0 <= w && w <= 3
