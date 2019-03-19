module Parsing.PrimitivesSpec where

import           Parsing.Primitives
import           Data.Char
import           Test.Hspec

itemSpec =
  describe "Parsing.Primitives.item" $ do

    context "when the input stream is not empty" $ do
      it "succeeds by consuming exactly one item from the input stream" $ do
        parse item "some text" `shouldBe` (Right 's', "ome text")

    context "when the input stream is empty (cannot produce an item)" $ do
      it "fails with the error reason \"no data\"" $ do
        parse item "" `shouldBe` (Left "no data", "")

unitSpec =
  describe "Parsing.Primitives.unit" $ do

    context "when the input stream is not empty" $ do
      it "succeeds without consuming any item from the input stream" $ do
        parse (unit 'z') "some text" `shouldBe` (Right 'z', "some text")

    context "when the input stream is empty" $ do
      it "succeeds without consuming any item from the input stream" $ do
        parse (unit 'z') "" `shouldBe` (Right 'z', "")

failureChar :: Parser Char
failureChar = failure

failureSpec =
  describe "Parsing.Primitives.failure" $ do

    context "when the input stream is not empty" $ do
      it "fails without consuming any item from the input stream" $ do
        parse failureChar "some text" `shouldBe` (Left "failure", "some text")

    context "when the input stream is empty" $ do
      it "fails without consuming any item from the input stream" $ do
        parse failureChar "" `shouldBe` (Left "failure", "")

fmapParserSpec =
  describe "Parsing.Primitives.Parser's fmap" $ do

    context "when the given parser succeeds on the given input stream" $ do
      it "succeeds by applying the given function to the parser result" $ do
        parse (fmap toUpper item) "some text" `shouldBe` (Right 'S', "ome text")

    context "when the input stream is empty" $ do
      it "fails without consuming any item from the input stream" $ do
        parse (fmap toUpper failure) "some text" `shouldBe` (Left "failure", "some text")

errorContextSpec =
  describe "Parsing.Primitives.<??>" $ do

    context "when the given parser succeeds on the given input stream" $ do
      it "succeeds by returning the parser's result unchanged" $ do
        parse (item <??> "cannot read a character") "some text" `shouldBe` (Right 's', "ome text")

    context "when the given parser fails with an error reason" $ do
      it "fails by decorating the error reason with the given message" $ do
        parse (item <??> "cannot read a character") "" `shouldBe` (Left "cannot read a character: no data", "")

errorReplaceSpec =
  describe "Parsing.Primitives.<?>" $ do

    context "when the given parser succeeds on the given input stream" $ do
      it "succeeds by returning the parser's result unchanged" $ do
        parse (item <?> "cannot read a character") "some text" `shouldBe` (Right 's', "ome text")

    context "when the given parser fails with an error reason" $ do
      it "fails by replacing the error reason with the given message" $ do
        parse (item <?> "cannot read a character") "" `shouldBe` (Left "cannot read a character", "")

spec :: Spec
spec = do
  itemSpec
  unitSpec
  failureSpec
  fmapParserSpec
  errorContextSpec
  errorReplaceSpec
