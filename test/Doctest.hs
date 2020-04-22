module Main
    ( main
    )
    where
import Test.DocTest

main :: IO ()
main = doctest
  [ "-XOverloadedStrings"
  , "-XDataKinds"
  , "-XScopedTypeVariables"
  , "-XTypeFamilies"
  , "-XTypeApplications"
  , "-isrc"
  , "src/Streamly/Vinyl/PathSignature"
  , "src/Streamly/Vinyl/LagLead"
  , "src/Streamly/Vinyl/Interpolate"
  ]