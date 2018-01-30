module Samples.Welcome
    exposing (source)

source : String
source = """# Welcome to Datadown

## what

Make interactive elements using Markdown. Prototype HTML and SVG, and export as React components.

## why

Edit live. No installation or config. Mobile friendly. Upcoming support for multiple platforms.

## what.svg
```svg
<svg width="100%">
<text x="10" y="50" font-size="1.8rem" fill="#5bf" stroke="#14f" stroke-dasharray="6 2">
{{ what }}
</text>
</svg>
```
"""
