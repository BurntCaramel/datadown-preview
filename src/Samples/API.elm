module Samples.API exposing (source)


source : String
source =
    """# Load from API

## data
<https://jsonplaceholder.typicode.com/posts/3>

## out
```
data
```

## render
```html
<h2>{{ data }}</h2>
```



## data
### post1: json
https://jsonplaceholder.typicode.com/posts/1

### post2: json
https://jsonplaceholder.typicode.com/posts/2

## render
```html
<h2>{{ post1.title }}</h2>
```
"""
