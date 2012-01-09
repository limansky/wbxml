module Wbxml.Serializer where

import Data.Binary.Put
import Data.Bits
import Data.Word
import Data.List (find)
import Control.Monad (when)
import Data.Maybe (fromJust)
import qualified Data.ByteString.Char8 as C
import Control.Monad.Trans.State
import Control.Monad.Trans (lift)
import Wbxml.Types
import Wbxml.WbxmlDefs

newtype SerializerState = SerializerState {
    currentPage :: Word8
    }

type PutS = StateT SerializerState PutM()

-- Multibyte word32
putWord32mb :: Word32 -> Put
putWord32mb d = putWord32mb' d 0
    where putWord32mb' v m = when (v >= 0x80) (putWord32mb' (v `shiftR` 7) 0x80) >>
                             (putWord8 $ fromIntegral (v .&. 0x7f .|. m))


writeWbxml :: WbxmlDocument -> PutS
writeWbxml (WbxmlDocument h t) = do
    lift $ writeHeader h
    writePage $ tagPage t
    writeTag t

writeHeader (WbxmlHeader v pid pix c t) = do
    putWord8 $ fromIntegral . fromEnum $ v
    putWord32mb pid
    when (pid == 0) (putWord32mb pix)
    putWord32mb (fst . fromJust $ find (\x -> snd x == c) knownCharsets)
    let strblen = length t
    putWord32mb $ fromIntegral strblen
    when (strblen > 0) (putByteString $ C.pack t)

writeTag (WbxmlTag p c a ch v) = do
    (SerializerState cp) <- get
    when (p /= cp) (writePage p)
    let code = c .|. (if null a then 0 else 0x80) .|. (if null ch && null v then 0 else 0x40)
    lift $ putWord8 code
    when (not . null $ a) (lift $ writeAttrs a)
    when (not . null $ ch) (mapM_ writeTag ch)
    when (not . null $ v) (lift $ writeValue)
    when ((not . null $ ch) || (not . null $ v)) (lift $ putWord8 tokenEnd)

writePage :: Word8 -> PutS
writePage p = do
    st <- get
    lift $ putWord8 tokenSwitchPage >> putWord8 p
    put $ st { currentPage = p }
    return ()

writeAttrs = undefined
writeValue = undefined
