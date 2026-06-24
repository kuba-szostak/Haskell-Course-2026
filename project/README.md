# UiLayoutLang

A declarative UI layout DSL and engine implemented in Haskell.

## Overview

UiLayoutLang is a small declarative language for describing UI layouts as nested boxes — a stripped-down Flexbox. Programs declare a tree of boxes with size and direction properties; the engine computes the absolute position and size of every box for a given window size, and can render the result as SVG for visual inspection.

## Project Structure

```
ui-layout-lang/
├── src/
│   ├── Main.hs                  -- CLI entry point
│   └── UiLayoutLang/
│       ├── Types.hs             -- Core data types
│       ├── Parser.hs            -- Megaparsec-based parser
│       ├── Engine.hs            -- Layout resolution engine
│       └── Renderer.hs          -- SVG renderer
├── test/
│   ├── Spec.hs                  -- Test runner (hspec-discover)
│   ├── ParserSpec.hs            -- Parser unit tests
│   ├── EngineSpec.hs            -- Engine unit + end-to-end tests
│   └── PropertySpec.hs          -- QuickCheck property-based tests
├── examples/
│   ├── simple.uilayout          -- Two-column layout
│   ├── nested.uilayout          -- Dashboard with header/sidebar
│   └── overflow.uilayout        -- Overflow clamping demo
├── package.yaml                 -- Build configuration (hpack)
└── stack.yaml                   -- Stack resolver
```

## DSL Syntax

```
window "name" <width> x <height> {
  row {                              // children laid out left-to-right
    box { width: 20%, height: 100%, color: red }
    box { width: 80%, height: 100%, color: blue }
  }
  col {                              // children laid out top-to-bottom
    box { width: 100%, height: 50px }
    box { width: 100%, height: 200px, color: #3498DB }
  }
}
```

### Sizes
- **Pixels**: `200px` or `200`
- **Percentage**: `50%` (fraction of parent dimension)

### Containers
- `row { ... }` — children are placed left-to-right
- `col { ... }` — children are placed top-to-bottom
- `box { ... }` — generic box with properties

### Comments
Line comments with `//` are supported.

## Design Decisions

### Leftover Space
When children's sizes sum to **less** than the parent along the layout axis, the remaining space is given to the **last child**.

### Overflow Handling
When children's sizes exceed the parent, each child is **clipped to the remaining space** along the layout axis. This guarantees the invariant: **no child sticks out of its parent**.

## Building

```bash
cd project/ui-layout-lang
stack build
```

## Running

```bash
# Render a layout to SVG (stdout)
stack run -- examples/simple.uilayout

# Render to a file
stack run -- examples/simple.uilayout output.svg
```

## Testing

```bash
stack test
```

The test suite includes:

- **Parser unit tests**: window declarations, containers, properties, comments, error handling, nested layouts
- **Engine unit tests**: size resolution, row/col layout, overflow clamping, mixed sizes
- **End-to-end tests**: full parse → resolve pipeline with hand-computed positions
- **Property-based tests** (QuickCheck):
  - Containment invariant: every child's bounding box lies inside its parent's
  - Axis-sum invariant: sum of children's sizes along the layout axis ≤ parent's size
  - Determinism: same input always produces the same output
  - Non-negative dimensions: all resolved boxes have width ≥ 0 and height ≥ 0

## Dependencies

- [megaparsec](https://hackage.haskell.org/package/megaparsec) — parser combinators
- [hspec](https://hackage.haskell.org/package/hspec) — BDD-style test framework
- [QuickCheck](https://hackage.haskell.org/package/QuickCheck) — property-based testing
