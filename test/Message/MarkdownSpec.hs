module Message.MarkdownSpec
    ( main
    , spec
    ) where

import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           Message.Markdown

-- ----------------------------------------------

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Message.Markdown.(ln|uln)" $

    it "uln . ln = id (prop)" $
      property $ \s -> s == (uln . ln) s
