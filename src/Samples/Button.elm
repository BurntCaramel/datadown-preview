module Samples.Button exposing (source)


source : String
source =
    """# Button

A button similar to Bootstrap 4’s.

## content: `String`
Click me!

## color: `String`
- purple
- orange
- red
- green
- blue

## classes
- inline-block px-3 py-2
- text-white
- bg-{{ color }} hover:bg-{{ color }}-dark border-{{ color }}
- rounded shadow

## view
```html
<button class="{{ classes }}">
  {{ content }}
</button>
```

---

> # Red button
> ## content
> Button title
> ## color
> red

## view
```css
button.bg-red.border-red { content: 'Button title'; }
```
"""
