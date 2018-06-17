module Samples.FileBrowserModel exposing (source)


source : String
source =
    """# File browser

## ViewMode: `String`
- Icons
- List
- Columns

## Query

### viewMode: `ViewMode`

## Mutation

### viewAsIcons

### viewAsList

### viewAsColumns


## Initial

### viewMode
`Icons`


## Update

### viewAsIcons

#### viewMode
`Icons`

### viewAsList

#### viewMode
`List`

### viewAsColumns

#### viewMode
`Columns`


## view

```html
<p>{{ viewMode }}</p>
```

"""
