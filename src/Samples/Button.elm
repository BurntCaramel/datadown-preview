module Samples.Button
    exposing (source)

source : String
source = """# Button

A button similar to Bootstrap 4’s.

## props
### style

## content
Button

## classes
- inline-block px-3 py-2
- text-white
- bg-blue hover:bg-blue-dark border-blue `props.style == "primary"`
- bg-red hover:bg-red-dark border-red `props.style == "danger"`
- bg-green hover:bg-green-dark border-green `props.style == "success"`
- rounded

## render
```html
<button class="{{ classes }}">
  {{ content }}
</button>
```
"""
