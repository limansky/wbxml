import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import TestSAX
import TestBindingHaXml

main = defaultMain tests

tests = [ testGroup "SAX parser tests" 
            [ testProperty "Test IString parser" prop_istring
            ]
        , testGroup "HaXML binding tests"
            [ testCase "Test inner elements" tst_inner
            ]
        ]
