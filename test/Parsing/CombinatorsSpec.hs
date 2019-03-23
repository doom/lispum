module Parsing.CombinatorsSpec where

import           Control.Applicative
import           Data.Char
import           Parsing.Primitives
import           Parsing.Combinators
import           Test.Hspec

satisfySpec =
  describe "Parsing.Combinators.satisfy" $ do

    context "when the first item of the input stream matches the given predicate" $ do
      it "succeeds by consuming a single item from the input stream" $ do
        parse (satisfy $ \c -> elem c "stuv") "some text" `shouldBe` (Right 's', "ome text")

    context "when the first item of the input stream does not match the given predicate" $ do
      it "fails by without consuming any input" $ do
        parse (satisfy $ \c -> elem c "abcd") "some text" `shouldBe` (Left "failure", "some text")

    context "when the input stream is empty (cannot produce an item)" $ do
      it "fails with \"no data\" as an error reason" $ do
        parse (satisfy $ \c -> elem c "abcd") "" `shouldBe` (Left "no data", "")

oneOfSpec :: Spec
oneOfSpec =
  describe "Parsing.Combinators.oneOf" $ do

    context "when the first item of the input stream is contained in the given list" $ do
      it "succeeds by consuming a single item from the input stream" $ do
        parse (oneOf "stuv") "some text" `shouldBe` (Right 's', "ome text")

    context "when the input stream is empty (cannot produce an item)" $ do
      it "fails with \"no data\" as an error reason" $ do
        parse (oneOf "abcd") "" `shouldBe` (Left "no data", "")

noneOfSpec :: Spec
noneOfSpec =
  describe "Parsing.Combinators.noneOf" $ do

    context "when the first item of the input stream is not contained in the given list" $ do
      it "succeeds by consuming a single item from the input stream" $ do
        parse (noneOf "abcd") "some text" `shouldBe` (Right 's', "ome text")

    context "when the input stream is empty (cannot produce an item)" $ do
      it "fails with \"no data\" as an error reason" $ do
        parse (noneOf "abcd") "" `shouldBe` (Left "no data", "")

notFollowedBySpec :: Spec
notFollowedBySpec =
  describe "Parsing.Combinators.notFollowedBy" $ do

    context "when the given parser would succeed on the input stream" $ do
      it "fails without consuming any input" $ do
        parse (notFollowedBy item) "some text" `shouldBe` (Left "failure", "some text")

    context "when the given parser would fail on the input stream" $ do
      it "succeeds without consuming any input" $ do
        parse (notFollowedBy $ notFollowedBy  item) "some text" `shouldBe` (Right (), "some text")

parseInteger :: Parser Integer
parseInteger = do
  digits <- some (satisfy isDigit)
  return $ read digits

data AddExpr
  = Plus AddExpr
         AddExpr
  | Number Integer
  deriving (Show, Eq)

parseNumber :: Parser AddExpr
parseNumber = do
  int <- parseInteger
  return $ Number int

parsePlus :: Parser (AddExpr -> AddExpr -> AddExpr)
parsePlus = do
  satisfy ('+' ==)
  return Plus

parseAddExpr :: Parser AddExpr
parseAddExpr = (chainl1 parseNumber parsePlus) <?> "expected expression"

chainl1Spec :: Spec
chainl1Spec =
  describe "Parsing.Combinators.chainl1" $ do

    context "when the \"p\" parser succeeds, then the \"sep\" parser, then the \"p\" parser again" $ do
      it "succeeds by consuming the accepted input" $ do
        parse parseAddExpr "1+2" `shouldBe` (Right $ Plus (Number 1) (Number 2), "")

    context "when the \"p\" parser succeeds, then the \"sep\" parser, but the \"p\" parser then fails" $ do
      it "fails by consuming the inputs consumed by the successful parsers" $ do
        parse parseAddExpr "1+" `shouldBe` (Left "expected expression", "")

    context "when the first application of the \"p\" parser fails" $ do
      it "fails by consuming the input consumed by the first application of the \"p\" parser" $ do
        parse parseAddExpr "zabc+2" `shouldBe` (Left "expected expression", "zabc+2")

parseCommaSeparatedIntList :: Parser [Integer]
parseCommaSeparatedIntList = sepBy parseInteger (satisfy (',' ==)) <?> "expected integer list"

sepBySpec :: Spec
sepBySpec =
  describe "Parsing.Combinators.sepBy" $ do

    context "when the \"p\" parser succeeds, then the \"sep\" parser, then the \"p\" parser again, etc" $ do
      it "succeeds by consuming the accepted input" $ do
        parse parseCommaSeparatedIntList "0,1,2,3" `shouldBe` (Right [0, 1, 2, 3], "")

    context "when the first application of the \"p\" parser fails" $ do
      it "succeeds without consuming any input" $ do
        parse parseCommaSeparatedIntList "abcde" `shouldBe` (Right [], "abcde")

parseDoubleQuotedString :: Parser String
parseDoubleQuotedString =
  (between (satisfy ('"' ==)) (some (noneOf "\"")) (satisfy ('"' ==))) <?> "expected double quoted string"

betweenSpec :: Spec
betweenSpec =
  describe "Parsing.Combinators.between" $ do
    context "when the \"open\" parser fails" $ do
      it "fails by consuming the input consumed by the first application of the \"open\" parser" $ do
        parse parseDoubleQuotedString "(not quoted)" `shouldBe` (Left "expected double quoted string", "(not quoted)")

    context "when the \"close\" parser fails" $ do
      it "fails by consuming the input consumed by the successful parsers" $ do
        parse parseDoubleQuotedString "\"mismatch" `shouldBe` (Left "expected double quoted string", "")

spec :: Spec
spec = do
  satisfySpec
  oneOfSpec
  noneOfSpec
  notFollowedBySpec
  chainl1Spec
  sepBySpec
  betweenSpec

