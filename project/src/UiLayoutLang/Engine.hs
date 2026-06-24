-- | Layout engine: resolves a 'Layout' tree into absolute screen
-- coordinates given the window dimensions.
--
-- == Design decisions
--
-- * __Leftover space__: when children's sizes sum to less than the
--   parent along the layout axis, the remaining space is given to
--   the last child.
--
-- * __Overflow (clamping)__: when children's sizes exceed the parent,
--   each child is clipped to the remaining space along the layout axis.
--   This guarantees the invariant: no child sticks out of its parent.
--
-- * __Cross-axis__: each child gets the parent's full size along the
--   cross-axis (clamped to the parent).
module UiLayoutLang.Engine
  ( resolve
  , resolveLayout
  , resolveSize
  ) where

import UiLayoutLang.Types

-- | Resolve a 'Window' into a fully-resolved layout tree.
resolve :: Window -> Resolved
resolve (Window _ ww wh layout) = resolveLayout 0 0 ww wh layout

-- | Resolve a single 'Layout' node within the given bounding box
-- @(x, y, w, h)@.
resolveLayout :: Int -> Int -> Int -> Int -> Layout -> Resolved
resolveLayout x y w h (Box props children) =
  let
    -- Resolve this box's own dimensions (clamped to parent)
    boxW = min w (resolveSize (width props)  w)
    boxH = min h (resolveSize (height props) h)

    -- Resolve children within this box
    resolvedChildren = resolveChildren (dir props) x y boxW boxH children
  in
    Resolved
      { rx        = x
      , ry        = y
      , rw        = boxW
      , rh        = boxH
      , rColor    = color props
      , rChildren = resolvedChildren
      }

-- | Resolve a 'Size' value against a parent dimension.
resolveSize :: Size -> Int -> Int
resolveSize (Px n)  _      = max 0 n
resolveSize (Pct p) parent = max 0 (round (p * fromIntegral parent))

-- | Resolve a list of children within a parent bounding box,
-- laying them out along the given 'Direction'.
--
-- Each child is allocated space along the layout axis and clamped
-- to the remaining space.  The last child receives any leftover
-- space along the layout axis.
resolveChildren :: Direction -> Int -> Int -> Int -> Int -> [Layout] -> [Resolved]
resolveChildren _   _ _ _ _ []       = []
resolveChildren dir' px py pw ph layouts =
  go px py pw ph layouts
  where
    go _ _ _ _ [] = []
    go cx cy remainW remainH [lastChild] =
      -- Last child gets all remaining space (leftover distribution)
      [resolveLastChild dir' cx cy remainW remainH lastChild]
    go cx cy remainW remainH (child : rest) =
      let
        resolved = resolveChildClamped dir' cx cy remainW remainH child
        (nextX, nextY, newRemainW, newRemainH) = advance dir' cx cy remainW remainH resolved
      in
        resolved : go nextX nextY newRemainW newRemainH rest

-- | Resolve a child, clamping its size along the layout axis
-- to the remaining space.
resolveChildClamped :: Direction -> Int -> Int -> Int -> Int -> Layout -> Resolved
resolveChildClamped dir' cx cy remainW remainH (Box props children) =
  let
    -- Compute the child's requested size
    childReqW = resolveSize (width props)  remainW
    childReqH = resolveSize (height props) remainH

    -- Clamp to remaining space
    (childW, childH) = case dir' of
      Row -> (min childReqW remainW, min childReqH remainH)
      Col -> (min childReqW remainW, min childReqH remainH)

    -- Resolve grandchildren
    resolvedChildren = resolveChildren (dir props) cx cy childW childH children
  in
    Resolved
      { rx        = cx
      , ry        = cy
      , rw        = childW
      , rh        = childH
      , rColor    = color props
      , rChildren = resolvedChildren
      }

-- | Resolve the last child in a container, giving it all remaining space
-- along the layout axis.
resolveLastChild :: Direction -> Int -> Int -> Int -> Int -> Layout -> Resolved
resolveLastChild dir' cx cy remainW remainH (Box props children) =
  let
    -- The last child gets all remaining space along the layout axis
    childReqW = resolveSize (width props) remainW
    childReqH = resolveSize (height props) remainH

    (childW, childH) = case dir' of
      Row -> (max (min childReqW remainW) remainW, min childReqH remainH)
      Col -> (min childReqW remainW, max (min childReqH remainH) remainH)

    -- Resolve grandchildren
    resolvedChildren = resolveChildren (dir props) cx cy childW childH children
  in
    Resolved
      { rx        = cx
      , ry        = cy
      , rw        = childW
      , rh        = childH
      , rColor    = color props
      , rChildren = resolvedChildren
      }

-- | Compute the next position and remaining space after placing a child.
advance :: Direction -> Int -> Int -> Int -> Int -> Resolved -> (Int, Int, Int, Int)
advance Row _cx cy remainW remainH child =
  let nextX     = rx child + rw child
      newRemain = max 0 (remainW - rw child)
  in  (nextX, cy, newRemain, remainH)
advance Col cx _cy remainW remainH child =
  let nextY     = ry child + rh child
      newRemain = max 0 (remainH - rh child)
  in  (cx, nextY, remainW, newRemain)
