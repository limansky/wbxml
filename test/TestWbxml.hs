----------------------------------------------------------------------
-- |
-- Module       : TestWbxml
-- Copyright    : Mike Limansky, 2012
-- Licencse     : BSD3
--
-- Main module of Wbxml unit tests.
--
----------------------------------------------------------------------

{-# LANGUAGE CPP #-}

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import TestSAX
import TestDOM

#ifdef WithHaXml
import TestBindingHaXml
#endif 

main = defaultMain tests

tests = [ testGroup "SAX parser tests" 
            [ testProperty "Test IString parser" prop_istring
            , testProperty "Test multibyte words parser" prop_word32mb
            , testProperty "Test opaque data parser" prop_opaque
            ]
        , testGroup "Internal DOM model tests"
            [ testCase "Test creation from SAX" tst_build_sax
            ]
#ifdef WithHaXml
        , testGroup "HaXML binding tests"
            [ testCase "Test inner elements" tst_inner
            ]
#endif
        ]
