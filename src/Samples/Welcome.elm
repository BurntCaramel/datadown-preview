module Samples.Welcome
    exposing (source)

source : String
source = """# Welcome screen

## firstName
Jane!

## lastName
Doe

## fullName
{{ firstName }} {{ lastName }}

## data
```json
{ "firstName": "{{ firstName }}", "name": "Doe", "items": ["first", { "nested": true }] }
```

## Header
```html
<h1>Welcome, {{ fullName }}!</h1>
"""
