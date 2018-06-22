module Samples.UserProfile exposing (source)


source : String
source =
    """# User Profile

## profileData
```
https://randomuser.me/api
```

## view
```html
<Row>
{{#profileData.result.results.0}}
<img class="rounded-full mr-2" src="{{ .picture.thumbnail }}">
<strong>{{ .name.first }} {{ .name.last }}</strong>
{{/profileData.result.results.0}}
</Row>
```
"""
