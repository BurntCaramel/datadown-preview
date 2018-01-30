module Samples.Button
    exposing (source)

source : String
source = """# Button

A button similar to Bootstrap 4’s.

## props
### style

## content
Button

## color
purple

## classes
- inline-block px-3 py-2
- text-white
- bg-{{ color }} hover:bg-{{ color }}-dark border-{{ color }}
- rounded

## render
```html
<button class="{{ classes }}">
  {{ content }}
</button>
```
"""
