module TestSAX where
import Test.QuickCheck
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as B
import Data.Attoparsec (parse, Result(..))
import Wbxml.SAX
import Wbxml.WbxmlDefs

prop_istring str = (not $ '\NUL' `elem` str) ==> parsedStr == str
    where (Done _ parsedStr) = parse parseIString (tokenStrI `B.cons` (C.pack str) `B.snoc` 0)
