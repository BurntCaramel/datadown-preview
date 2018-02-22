module Samples.UserProfile exposing (source)


source : String
source =
    """# User Profile

## profile_data
```
https://randomuser.me/api
```

## view
```html
<Row>
{{#profile_data.result.results.0}}
<img class="rounded-full mr-2" src="{{ .picture.thumbnail }}">
<strong>{{ .name.first }} {{ .name.last }}</strong>
{{/profile_data.result.results.0}}
</Row>
```
"""
