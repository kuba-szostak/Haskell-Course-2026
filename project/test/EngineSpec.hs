{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module EngineSpec (spec) where

import Test.Hspec
import UiLayoutLang.Types
import UiLayoutLang.Engine
import UiLayoutLang.Parser (parseLayout)

spec :: Spec
spec = describe "Layout Engine" $ do

  -- -------------------------------------------------------------------
  -- resolveSize
  -- -------------------------------------------------------------------
  describe "resolveSize" $ do

    it "resolves pixel size unchanged" $ do
      resolveSize (Px 200) 800 `shouldBe` 200

    it "resolves 100% to parent size" $ do
      resolveSize (Pct 1.0) 800 `shouldBe` 800

    it "resolves 50% to half the parent" $ do
      resolveSize (Pct 0.5) 800 `shouldBe` 400

    it "resolves 0% to zero" $ do
      resolveSize (Pct 0.0) 800 `shouldBe` 0

    it "clamps negative pixel values to zero" $ do
      resolveSize (Px (-10)) 800 `shouldBe` 0

  -- -------------------------------------------------------------------
  -- Single box
  -- -------------------------------------------------------------------
  describe "single box" $ do

    it "a 100% box fills the parent" $ do
      let layout = Box (defaultProps { width = Pct 1.0, height = Pct 1.0 }) []
          window = Window "T" 800 600 layout
          r = resolve window
      rw r `shouldBe` 800
      rh r `shouldBe` 600
      rx r `shouldBe` 0
      ry r `shouldBe` 0

    it "a 50% box takes half the parent" $ do
      let layout = Box (defaultProps { width = Pct 0.5, height = Pct 0.5 }) []
          window = Window "T" 800 600 layout
          r = resolve window
      rw r `shouldBe` 400
      rh r `shouldBe` 300

    it "a pixel-sized box is independent of parent size" $ do
      let layout = Box (defaultProps { width = Px 200, height = Px 150 }) []
          window = Window "T" 800 600 layout
          r = resolve window
      rw r `shouldBe` 200
      rh r `shouldBe` 150

  -- -------------------------------------------------------------------
  -- Row layout
  -- -------------------------------------------------------------------
  describe "row layout" $ do

    it "two 50% children split a row exactly in half" $ do
      let child1 = Box (defaultProps { width = Pct 0.5, height = Pct 1.0 }) []
          child2 = Box (defaultProps { width = Pct 0.5, height = Pct 1.0 }) []
          layout = Box (defaultProps { dir = Row }) [child1, child2]
          window = Window "T" 800 600 layout
          r = resolve window
      length (rChildren r) `shouldBe` 2
      let [c1, c2] = rChildren r
      -- First child: x=0, w=400
      rx c1 `shouldBe` 0
      rw c1 `shouldBe` 400
      -- Second child (last): x=400, w=400
      rx c2 `shouldBe` 400
      rw c2 `shouldBe` 400

    it "a 20%/80% split matches the example from the spec" $ do
      let child1 = Box (defaultProps { width = Pct 0.20, height = Pct 1.0
                                     , color = Just "red" }) []
          child2 = Box (defaultProps { width = Pct 0.80, height = Pct 1.0
                                     , color = Just "blue" }) []
          layout = Box (defaultProps { dir = Row }) [child1, child2]
          window = Window "T" 800 600 layout
          r = resolve window
      let [c1, c2] = rChildren r
      rx c1 `shouldBe` 0
      rw c1 `shouldBe` 160
      rh c1 `shouldBe` 600
      rx c2 `shouldBe` 160
      rw c2 `shouldBe` 640
      rh c2 `shouldBe` 600

    it "gives leftover space to the last child" $ do
      let child1 = Box (defaultProps { width = Pct 0.3, height = Pct 1.0 }) []
          child2 = Box (defaultProps { width = Pct 0.3, height = Pct 1.0 }) []
          layout = Box (defaultProps { dir = Row }) [child1, child2]
          window = Window "T" 800 600 layout
          r = resolve window
      let [c1, c2] = rChildren r
      rw c1 `shouldBe` 240   -- 30% of 800
      rw c2 `shouldBe` 560   -- gets the remaining 800 - 240 = 560

  -- -------------------------------------------------------------------
  -- Col layout
  -- -------------------------------------------------------------------
  describe "col layout" $ do

    it "two 50% children split a column exactly in half" $ do
      let child1 = Box (defaultProps { width = Pct 1.0, height = Pct 0.5 }) []
          child2 = Box (defaultProps { width = Pct 1.0, height = Pct 0.5 }) []
          layout = Box (defaultProps { dir = Col }) [child1, child2]
          window = Window "T" 800 600 layout
          r = resolve window
      let [c1, c2] = rChildren r
      ry c1 `shouldBe` 0
      rh c1 `shouldBe` 300
      ry c2 `shouldBe` 300
      rh c2 `shouldBe` 300

  -- -------------------------------------------------------------------
  -- Overflow (clamping)
  -- -------------------------------------------------------------------
  describe "overflow clamping" $ do

    it "clamps children that exceed the parent" $ do
      let child1 = Box (defaultProps { width = Pct 0.7, height = Pct 1.0 }) []
          child2 = Box (defaultProps { width = Pct 0.7, height = Pct 1.0 }) []
          layout = Box (defaultProps { dir = Row }) [child1, child2]
          window = Window "T" 800 600 layout
          r = resolve window
      let [c1, c2] = rChildren r
      -- c1 gets 70% = 560
      rw c1 `shouldBe` 560
      -- c2 is clamped to remaining: 800 - 560 = 240
      rw c2 `shouldBe` 240
      -- Both start and end within parent
      rx c1 + rw c1 `shouldSatisfy` (<= 800)
      rx c2 + rw c2 `shouldSatisfy` (<= 800)

    it "a box larger than the parent is clamped" $ do
      let layout = Box (defaultProps { width = Px 1000, height = Px 800 }) []
          window = Window "T" 800 600 layout
          r = resolve window
      rw r `shouldBe` 800
      rh r `shouldBe` 600

  -- -------------------------------------------------------------------
  -- Mixed sizes
  -- -------------------------------------------------------------------
  describe "mixed pixel and percentage sizes" $ do

    it "handles a mix of px and pct children" $ do
      let child1 = Box (defaultProps { width = Px 200, height = Pct 1.0 }) []
          child2 = Box (defaultProps { width = Pct 0.5, height = Pct 1.0 }) []
          layout = Box (defaultProps { dir = Row }) [child1, child2]
          window = Window "T" 800 600 layout
          r = resolve window
      let [c1, c2] = rChildren r
      rw c1 `shouldBe` 200
      -- c2: 50% of remaining 600 = 300, but last child gets rest = 600
      rw c2 `shouldBe` 600

  -- -------------------------------------------------------------------
  -- End-to-end: parse -> resolve
  -- -------------------------------------------------------------------
  describe "end-to-end (parse -> resolve)" $ do

    it "resolves the example layout from the spec" $ do
      let input = unlines
            [ "window \"Main\" 800 x 600 {"
            , "  row {"
            , "    box { width: 20%, height: 100%, color: red }"
            , "    box { width: 80%, height: 100%, color: blue }"
            , "  }"
            , "}"
            ]
      case parseLayout input of
        Left err -> expectationFailure (show err)
        Right w  -> do
          let r = resolve w
          rw r `shouldBe` 800
          rh r `shouldBe` 600
          -- The root IS the row container; its children are red and blue
          length (rChildren r) `shouldBe` 2
          let [red, blue] = rChildren r
          -- Red: 20% of 800 = 160
          rx red  `shouldBe` 0
          rw red  `shouldBe` 160
          rh red  `shouldBe` 600
          -- Blue: last child gets remaining = 640
          rx blue `shouldBe` 160
          rw blue `shouldBe` 640
          rh blue `shouldBe` 600

    it "resolves a nested col-in-row layout" $ do
      let input = unlines
            [ "window \"Nested\" 400 x 400 {"
            , "  row {"
            , "    col {"
            , "      box { width: 100%, height: 50% }"
            , "      box { width: 100%, height: 50% }"
            , "    }"
            , "    col {"
            , "      box { width: 100%, height: 50% }"
            , "      box { width: 100%, height: 50% }"
            , "    }"
            , "  }"
            , "}"
            ]
      case parseLayout input of
        Left err -> expectationFailure (show err)
        Right w  -> do
          let r = resolve w
          rw r `shouldBe` 400
          rh r `shouldBe` 400
