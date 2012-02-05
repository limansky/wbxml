module TestSAX
(
    prop_istring,
    prop_word32mb,
    prop_opaque
)
where
import Test.QuickCheck hiding ((.&.))
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as B
import Data.Attoparsec (parse, IResult(..))
import Wbxml.SAX
import Wbxml.WbxmlDefs
import Data.Word
import Data.Bits
import Control.Monad (liftM)

prop_istring str = (not $ '\NUL' `elem` str) ==> parsedStr == str
    where (Done _ parsedStr) = parse parseIString (tokenStrI `B.cons` (C.pack str) `B.snoc` 0)

prop_word32mb w = parsedWord == w
    where (Done _ parsedWord) = parse anyMultiByteWord $ makeMBWord w

prop_opaque b = b == parsedData
    where (Done _ parsedData) = parse parseOpaqueData (tokenOpaque `B.cons` (makeMBWord . fromIntegral $ B.length b) `B.append` b)

makeMBWord :: Word32 -> B.ByteString
makeMBWord w = B.pack . reverse $ makeMBWord' w 0
    where makeMBWord' v m
              | v >= 0x80 = v' : makeMBWord' (v `shiftR` 7) 0x80
              | otherwise = [ v' ]
              where v' = fromIntegral $ v .&. 0x7f .|. m

instance Arbitrary Word32 where
    arbitrary = do
        let mx, mn :: Integer
            mx = fromIntegral (maxBound :: Word32)
            mn = fromIntegral (minBound :: Word32)
        c <- choose (mn, mx)
        return $ fromIntegral c

instance Arbitrary Word8 where
    arbitrary = do
        let mx, mn :: Integer
            mx = fromIntegral (maxBound :: Word8)
            mn = fromIntegral (minBound :: Word8)
        c <- choose (mn, mx)
        return $ fromIntegral c

instance Arbitrary B.ByteString where
    arbitrary = liftM B.pack arbitrary
