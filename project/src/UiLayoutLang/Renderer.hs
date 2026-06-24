-- | SVG renderer for resolved layout trees.
--
-- Converts a 'Resolved' tree into an SVG string that can be written
-- to a file and opened in any web browser for visual inspection.
module UiLayoutLang.Renderer
  ( renderSvg
  , renderSvgToFile
  ) where

import UiLayoutLang.Types

-- | Default color palette for boxes without an explicit color.
-- Cycles through these colors for visual distinction.
defaultPalette :: [String]
defaultPalette =
  [ "#4A90D9"   -- steel blue
  , "#D94A4A"   -- soft red
  , "#4AD97A"   -- emerald
  , "#D9B44A"   -- gold
  , "#9B59B6"   -- amethyst
  , "#1ABC9C"   -- turquoise
  , "#E67E22"   -- carrot
  , "#2C3E50"   -- midnight blue
  ]

-- | Render a 'Resolved' tree as an SVG string.
--
-- The SVG includes:
--   * A white background
--   * Coloured rectangles for each box
--   * 1px dark stroke borders for visibility
renderSvg :: Int -> Int -> Resolved -> String
renderSvg viewW viewH resolved = unlines
  [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
  , "<svg xmlns=\"http://www.w3.org/2000/svg\""
  , "     width=\"" ++ show viewW ++ "\" height=\"" ++ show viewH ++ "\""
  , "     viewBox=\"0 0 " ++ show viewW ++ " " ++ show viewH ++ "\">"
  , ""
  , "  <!-- Background -->"
  , "  <rect width=\"" ++ show viewW ++ "\" height=\"" ++ show viewH
    ++ "\" fill=\"#f0f0f0\" />"
  , ""
  , renderResolved 0 resolved
  , "</svg>"
  ]

-- | Render a single resolved box and its children as SVG elements.
renderResolved :: Int -> Resolved -> String
renderResolved depth resolved =
  let
    fillColor = case rColor resolved of
      Just c  -> c
      Nothing -> defaultPalette !! (depth `mod` length defaultPalette)

    indent = replicate (2 * (depth + 1)) ' '

    rect = indent ++ "<rect"
      ++ " x=\"" ++ show (rx resolved) ++ "\""
      ++ " y=\"" ++ show (ry resolved) ++ "\""
      ++ " width=\"" ++ show (rw resolved) ++ "\""
      ++ " height=\"" ++ show (rh resolved) ++ "\""
      ++ " fill=\"" ++ fillColor ++ "\""
      ++ " stroke=\"#333\" stroke-width=\"1\""
      ++ " opacity=\"0.85\""
      ++ " />"

    childSvgs = map (renderResolved (depth + 1)) (rChildren resolved)
  in
    unlines (rect : childSvgs)

-- | Render a resolved layout to an SVG file.
renderSvgToFile :: FilePath -> Int -> Int -> Resolved -> IO ()
renderSvgToFile path viewW viewH resolved =
  writeFile path (renderSvg viewW viewH resolved)
