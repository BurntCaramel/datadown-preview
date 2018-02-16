module Samples.API exposing (source)


source : String
source =
    """# Loading from APIs

## post_url
<https://jsonplaceholder.typicode.com/posts/3>

## post
```
HTTP.get_json post_url
```

## view
```html
{{#post.error}}
<p>Error: {{ post.error.message }}</p>
{{/post.error}}
{{#post.result}}
<p>{{ post.result.title }}</p>
{{/post.result}}
```
"""
