module Samples.API exposing (source)


source : String
source =
    """# Loading from APIs

## postIndex
3

## post
```
https://jsonplaceholder.typicode.com/posts postIndex
```

## view
```html
{{#post.error}}
<p>Error: {{ .message }}</p>
{{/post.error}}
{{#post.result}}
<p>{{ .title }}</p>
{{/post.result}}
```
"""
