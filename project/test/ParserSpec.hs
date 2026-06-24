module ParserSpec (spec) where

import Test.Hspec
import UiLayoutLang.Parser
import UiLayoutLang.Types

spec :: Spec
spec = describe "Parser" $ do

  -- -------------------------------------------------------------------
  -- Window parsing
  -- -------------------------------------------------------------------
  describe "window declarations" $ do

    it "parses a simple window with a single box" $ do
      let input = unlines
            [ "window \"Test\" 800 x 600 {"
            , "  box { width: 100%, height: 100%, color: red }"
            , "}"
            ]
      case parseLayout input of
        Left err -> expectationFailure (show err)
        Right w  -> do
          windowName w   `shouldBe` "Test"
          windowWidth w  `shouldBe` 800
          windowHeight w `shouldBe` 600

    it "parses window name with spaces" $ do
      let input = "window \"My App\" 1024 x 768 { box { width: 100%, height: 100% } }"
      case parseLayout input of
        Left err -> expectationFailure (show err)
        Right w  -> windowName w `shouldBe` "My App"

  -- -------------------------------------------------------------------
  -- Row / Col containers
  -- -------------------------------------------------------------------
  describe "row and col containers" $ do

    it "parses a row container with children" $ do
      let input = unlines
            [ "window \"T\" 400 x 300 {"
            , "  row {"
            , "    box { width: 50%, height: 100% }"
            , "    box { width: 50%, height: 100% }"
            , "  }"
            , "}"
            ]
      case parseLayout input of
        Left err -> expectationFailure (show err)
        Right w  -> case windowLayout w of
          Box props children -> do
            dir props `shouldBe` Row
            length children `shouldBe` 2

    it "parses a col container" $ do
      let input = unlines
            [ "window \"T\" 400 x 300 {"
            , "  col {"
            , "    box { width: 100%, height: 50% }"
            , "    box { width: 100%, height: 50% }"
            , "  }"
            , "}"
            ]
      case parseLayout input of
        Left err -> expectationFailure (show err)
        Right w  -> case windowLayout w of
          Box props _ -> dir props `shouldBe` Col

  -- -------------------------------------------------------------------
  -- Box properties
  -- -------------------------------------------------------------------
  describe "box properties" $ do

    it "parses pixel sizes" $ do
      let input = "window \"T\" 800 x 600 { box { width: 200px, height: 150px } }"
      case parseLayout input of
        Left err -> expectationFailure (show err)
        Right w  -> case windowLayout w of
          Box props _ -> do
            width props  `shouldBe` Px 200
            height props `shouldBe` Px 150

    it "parses percentage sizes" $ do
      let input = "window \"T\" 800 x 600 { box { width: 75%, height: 50% } }"
      case parseLayout input of
        Left err -> expectationFailure (show err)
        Right w  -> case windowLayout w of
          Box props _ -> do
            width props  `shouldBe` Pct 0.75
            height props `shouldBe` Pct 0.50

    it "parses color property" $ do
      let input = "window \"T\" 800 x 600 { box { width: 100%, height: 100%, color: blue } }"
      case parseLayout input of
        Left err -> expectationFailure (show err)
        Right w  -> case windowLayout w of
          Box props _ -> color props `shouldBe` Just "blue"

    it "parses pixel sizes without px suffix" $ do
      let input = "window \"T\" 800 x 600 { box { width: 200, height: 150 } }"
      case parseLayout input of
        Left err -> expectationFailure (show err)
        Right w  -> case windowLayout w of
          Box props _ -> do
            width props  `shouldBe` Px 200
            height props `shouldBe` Px 150

  -- -------------------------------------------------------------------
  -- Comments
  -- -------------------------------------------------------------------
  describe "comments" $ do

    it "ignores line comments" $ do
      let input = unlines
            [ "// This is a comment"
            , "window \"T\" 800 x 600 {"
            , "  // Another comment"
            , "  box { width: 100%, height: 100% }"
            , "}"
            ]
      case parseLayout input of
        Left err -> expectationFailure (show err)
        Right _  -> pure ()

  -- -------------------------------------------------------------------
  -- Error handling
  -- -------------------------------------------------------------------
  describe "error reporting" $ do

    it "rejects malformed input" $ do
      let input = "not a valid layout"
      case parseLayout input of
        Left _  -> pure ()  -- expected
        Right _ -> expectationFailure "Expected parse error"

    it "rejects missing closing brace" $ do
      let input = "window \"T\" 800 x 600 { box { width: 100% }"
      case parseLayout input of
        Left _  -> pure ()
        Right _ -> expectationFailure "Expected parse error"

  -- -------------------------------------------------------------------
  -- Nested layouts
  -- -------------------------------------------------------------------
  describe "nested layouts" $ do

    it "parses deeply nested layouts" $ do
      let input = unlines
            [ "window \"Nested\" 800 x 600 {"
            , "  col {"
            , "    row {"
            , "      box { width: 50%, height: 100%, color: red }"
            , "      box { width: 50%, height: 100%, color: blue }"
            , "    }"
            , "    row {"
            , "      box { width: 33%, height: 100%, color: green }"
            , "      box { width: 33%, height: 100%, color: yellow }"
            , "      box { width: 34%, height: 100%, color: orange }"
            , "    }"
            , "  }"
            , "}"
            ]
      case parseLayout input of
        Left err -> expectationFailure (show err)
        Right w  -> case windowLayout w of
          Box _ children -> length children `shouldBe` 2
