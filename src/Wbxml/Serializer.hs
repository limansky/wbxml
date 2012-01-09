module Wbxml.Serializer where

import Data.Binary.Put
import Data.Bits
import Data.Word
import Data.List (find)
import Control.Monad (when)
import Data.Maybe (fromJust)
import qualified Data.ByteString.Char8 as C

import Wbxml.Types

-- Multibyte word32
putWord32mb :: Word32 -> Put
putWord32mb d = putWord32mb' d 0
    where putWord32mb' v m = when (v >= 0x80) (putWord32mb' (v `shiftR` 7) 0x80) >>
                             (putWord8 $ fromIntegral (v .&. 0x7f .|. m))
    

writeWbxml :: WbxmlDocument -> Put
writeWbxml (WbxmlDocument h t) = do
    writeHeader h

writeHeader (WbxmlHeader v pid pix c t) = do
    putWord8 $ fromIntegral . fromEnum $ v
    putWord32mb pid
    when (pid == 0) (putWord32mb pix)
    putWord32mb (fst . fromJust $ find (\x -> snd x == c) knownCharsets)
    let strblen = length t
    putWord32mb $ fromIntegral strblen
    when (strblen > 0) (putByteString $ C.pack t)
