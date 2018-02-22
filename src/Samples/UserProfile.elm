module Samples.UserProfile exposing (source)


source : String
source =
    """# User Profile

## profile_data_url
https://randomuser.me/api

## profile_data
```
HTTP.get_json profile_data_url
```

## blah
```html
<Row>
{{#profile_data.result.results.0}}
<img class="rounded-full mr-2" src="{{ .picture.thumbnail }}">
<strong>{{ .name.first }} {{ .name.last }}</strong>
{{/profile_data.result.results.0}}
</Row>
```
"""
