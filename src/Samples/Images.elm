module Samples.Images exposing (source)


source : String
source =
    """# Images

## placeholder_url
<https://via.placeholder.com/100x100>

## embedded
data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAUAAAAFCAYAAACNbyblAAAAHElEQVQI12P4//8/w38GIAXDIBKE0DHxgljNBAAO9TXL0Y4OHwAAAABJRU5ErkJggg==

## view
```html
<img src="{{ placeholder_url }}">
<img src="{{ embedded }}">
```
"""
