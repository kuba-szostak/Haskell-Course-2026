-- | CLI entry point for the UiLayoutLang tool.
--
-- Usage:
--   ui-layout-lang <input.uilayout> [output.svg]
--
-- If no output file is given, SVG is written to stdout.
module Main where

import System.Environment (getArgs)
import System.Exit        (exitFailure)
import System.IO          (hPutStrLn, stderr)

import UiLayoutLang.Parser   (parseLayout')
import UiLayoutLang.Engine   (resolve)
import UiLayoutLang.Renderer (renderSvg)
import UiLayoutLang.Types    (windowWidth, windowHeight)

main :: IO ()
main = do
  args <- getArgs
  case args of
    []          -> usage
    (inFile:rest) -> do
      src <- readFile inFile
      case parseLayout' inFile src of
        Left err -> do
          hPutStrLn stderr ("Parse error:\n" ++ show err)
          exitFailure
        Right window -> do
          let resolved = resolve window
              ww       = windowWidth window
              wh       = windowHeight window
              svg      = renderSvg ww wh resolved
          case rest of
            (outFile:_) -> do
              writeFile outFile svg
              putStrLn ("Written to " ++ outFile)
            [] -> putStr svg

usage :: IO ()
usage = do
  putStrLn "UiLayoutLang — Declarative UI Layout Engine"
  putStrLn ""
  putStrLn "Usage: ui-layout-lang <input.uilayout> [output.svg]"
  putStrLn ""
  putStrLn "  Parses a .uilayout file, computes absolute box positions,"
  putStrLn "  and renders the result as SVG."
  putStrLn ""
  putStrLn "  If no output file is given, SVG is printed to stdout."
