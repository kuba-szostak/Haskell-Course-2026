{-# OPTIONS_GHC -Wno-orphans #-}

module PropertySpec (spec) where

import Test.Hspec
import Test.QuickCheck

import UiLayoutLang.Types
import UiLayoutLang.Engine (resolve)

------------------------------------------------------------------------
-- Arbitrary instances for generating random layouts
------------------------------------------------------------------------

instance Arbitrary Size where
  arbitrary = oneof
    [ Px  <$> choose (0, 500)
    , Pct <$> choose (0.0, 1.0)
    ]

instance Arbitrary Direction where
  arbitrary = elements [Row, Col]

instance Arbitrary Props where
  arbitrary = Props
    <$> arbitrary          -- width
    <*> arbitrary          -- height
    <*> arbitrary          -- dir
    <*> pure Nothing       -- color (keep simple for tests)

instance Arbitrary Layout where
  arbitrary = sized genLayout
    where
      genLayout 0 = Box <$> arbitrary <*> pure []
      genLayout n = do
        numChildren <- choose (0, 3)
        let childSize = max 0 (n `div` (numChildren + 1))
        children <- vectorOf numChildren (resize childSize arbitrary)
        props <- arbitrary
        pure (Box props children)

  shrink (Box props children) =
    -- Shrink by dropping children or shrinking individual children
    [ Box props cs | cs <- shrinkList shrink children ]

-- | Generate a random window with reasonable dimensions.
genWindow :: Gen Window
genWindow = do
  w <- choose (100, 2000)
  h <- choose (100, 2000)
  layout <- arbitrary
  pure (Window "Test" w h layout)

------------------------------------------------------------------------
-- Helper: check invariants on a resolved tree
------------------------------------------------------------------------

-- | Check that every child's bounding box is contained within
-- its parent's bounding box.
containmentHolds :: Resolved -> Bool
containmentHolds parent =
  all (childContained parent) (rChildren parent)
  && all containmentHolds (rChildren parent)

-- | Check that a single child is inside the parent.
childContained :: Resolved -> Resolved -> Bool
childContained parent child =
  rx child >= rx parent
  && ry child >= ry parent
  && rx child + rw child <= rx parent + rw parent
  && ry child + rh child <= ry parent + rh parent

-- | Check that the sum of children's sizes along the layout axis
-- does not exceed the parent's size along that axis.
axisSumHolds :: Direction -> Resolved -> Bool
axisSumHolds dir' parent =
  let childSizes = case dir' of
        Row -> map rw (rChildren parent)
        Col -> map rh (rChildren parent)
      parentSize = case dir' of
        Row -> rw parent
        Col -> rh parent
  in sum childSizes <= parentSize

-- | Recursively check the axis-sum invariant.  We need the direction
-- from the original layout tree, so we thread it through.
axisSumHoldsRecursive :: Layout -> Resolved -> Bool
axisSumHoldsRecursive (Box props children) resolved =
  axisSumHolds (dir props) resolved
  && and (zipWith axisSumHoldsRecursive children (rChildren resolved))

------------------------------------------------------------------------
-- Properties
------------------------------------------------------------------------

spec :: Spec
spec = describe "Property-based tests" $ do

  describe "containment invariant" $ do
    it "every child's bounding box lies inside its parent's" $
      property $ forAll genWindow $ \w ->
        let r = resolve w
        in containmentHolds r

  describe "axis-sum invariant" $ do
    it "sum of children along the layout axis <= parent's size" $
      property $ forAll genWindow $ \w ->
        let r = resolve w
        in axisSumHoldsRecursive (windowLayout w) r

  describe "determinism" $ do
    it "same input always produces the same output" $
      property $ forAll genWindow $ \w ->
        resolve w == resolve w

  describe "non-negative dimensions" $ do
    it "all resolved boxes have non-negative width and height" $
      property $ forAll genWindow $ \w ->
        let r = resolve w
        in allNonNegative r

-- | Check that all resolved boxes have non-negative dimensions.
allNonNegative :: Resolved -> Bool
allNonNegative r =
  rw r >= 0
  && rh r >= 0
  && all allNonNegative (rChildren r)
