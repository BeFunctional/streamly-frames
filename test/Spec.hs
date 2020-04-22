module Main
    ( main
    )
where
import           Streamly.Vinyl.Interpolate     ( propLengthsTheSame
                                                , propOrigValesTheSame
                                                )
import           Streamly.Vinyl.PathSignature   ( unit_serialize_roundtrip
                                                , unit_sig_correct
                                                )
import           Test.QuickCheck.Monadic
import           Test.Tasty
import           Test.Tasty.HUnit              as HU
                                         hiding ( assert )
import           Test.Tasty.QuickCheck         as QC

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [units, properties]

units :: TestTree
units = testGroup
    "Unit tests"
    [ HU.testCase "That the path signature caclulation is correct"
        $ HU.assertBool "Signatures were not correct!" unit_sig_correct
    , HU.testCase "That path signatures can be round trip through serialize"
        $ HU.assertBool "Round trip was not correct!" unit_serialize_roundtrip
    ]

properties :: TestTree
properties = testGroup
    "Property tests (checked by QuickCheck)"
    [interpolateLengthsTheSame, interpolateOrigUnchanged]

interpolateLengthsTheSame :: TestTree
interpolateLengthsTheSame = QC.testProperty "That lengts of interpolated lists are the same " $ \s ->
    (monadicIO $ do
        a <- run (propLengthsTheSame s)
        assert a
    )

interpolateOrigUnchanged :: TestTree
interpolateOrigUnchanged = QC.testProperty "That the uninterpolated values of an interpolated list remain the same " $ \s ->
    (monadicIO $ do
        a <- run (propOrigValesTheSame s)
        assert a
    )
