-- | Core data types for the UiLayoutLang DSL.
--
-- A layout is a tree of nested boxes, each with size and direction properties.
-- The layout engine resolves this tree into absolute screen coordinates.
module UiLayoutLang.Types
  ( Layout(..)
  , Props(..)
  , Size(..)
  , Direction(..)
  , Resolved(..)
  , Window(..)
  , defaultProps
  ) where

-- | A layout is a box with properties and zero or more children.
data Layout = Box Props [Layout]
  deriving (Show, Eq)

-- | Properties that control how a box is sized and how its children
-- are arranged.
data Props = Props
  { width  :: Size           -- ^ Width of this box
  , height :: Size           -- ^ Height of this box
  , dir    :: Direction      -- ^ Layout direction for children
  , color  :: Maybe String   -- ^ Optional fill color
  } deriving (Show, Eq)

-- | A size can be specified in absolute pixels or as a percentage
-- (fraction 0.0–1.0) of the parent's corresponding dimension.
data Size
  = Px Int       -- ^ Absolute pixel size
  | Pct Double   -- ^ Percentage of parent (0.0 to 1.0)
  deriving (Show, Eq)

-- | Direction in which children are laid out.
data Direction
  = Row   -- ^ Children are placed left-to-right
  | Col   -- ^ Children are placed top-to-bottom
  deriving (Show, Eq)

-- | The result of layout resolution.  Every box gets absolute
-- coordinates and a list of resolved children.
data Resolved = Resolved
  { rx        :: Int             -- ^ Absolute X position
  , ry        :: Int             -- ^ Absolute Y position
  , rw        :: Int             -- ^ Resolved width
  , rh        :: Int             -- ^ Resolved height
  , rColor    :: Maybe String    -- ^ Fill color (if any)
  , rChildren :: [Resolved]      -- ^ Resolved children
  } deriving (Show, Eq)

-- | A top-level window declaration that provides the root
-- bounding box for layout resolution.
data Window = Window
  { windowName   :: String    -- ^ Window title
  , windowWidth  :: Int       -- ^ Window width in pixels
  , windowHeight :: Int       -- ^ Window height in pixels
  , windowLayout :: Layout    -- ^ Root layout tree
  } deriving (Show, Eq)

-- | Sensible default properties: 100% width and height, row direction,
-- no color.
defaultProps :: Props
defaultProps = Props
  { width  = Pct 1.0
  , height = Pct 1.0
  , dir    = Row
  , color  = Nothing
  }
