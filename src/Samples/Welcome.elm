module Samples.Welcome exposing (source)


source : String
source =
    """# Welcome to Datadown

## what

Prototype interactive HTML & SVG using Markdown.

## colors
### blue
`#5bf`
### blueDarker
`#14f`
### pink
`#d6b`
### pinkDarker
`#a29`

## why

Edit live. No installation or config. Mobile friendly.

## what.svg
```svg
<svg width="100%">
<text x="10" y="50" font-size="1.8rem" fill="{{ colors.blue }}" stroke="{{ colors.blueDarker }}" stroke-dasharray="6 2">
{{ what }}
</text>
</svg>
```

## why.svg
```svg
<svg width="100%">
<text x="10" y="50" font-size="1.8rem" fill="{{ colors.pink }}" stroke="{{ colors.pinkDarker }}" stroke-dasharray="6 2">
{{ why }}
</text>
</svg>
```
"""
