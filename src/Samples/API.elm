module Samples.API exposing (source)


source : String
source =
    """# Loading from APIs

## post_index
3

## post
```
https://jsonplaceholder.typicode.com/posts post_index
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
