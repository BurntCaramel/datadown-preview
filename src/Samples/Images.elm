module Samples.Images exposing (source)


source : String
source =
    """# Images

## placeholder_url
<https://via.placeholder.com/100x100>

## size
400

## subject
beach

## unsplash_url
https://source.unsplash.com/{{size * 2}}x{{size * 2}}?{{subject}}

## embedded
data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAUAAAAFCAYAAACNbyblAAAAHElEQVQI12P4//8/w38GIAXDIBKE0DHxgljNBAAO9TXL0Y4OHwAAAABJRU5ErkJggg==

## view
```html
<img src="{{ unsplash_url }}" width="{{size}}">
<br>
<img src="{{ placeholder_url }}">
<br>
<img src="{{ embedded }}">
```
"""
