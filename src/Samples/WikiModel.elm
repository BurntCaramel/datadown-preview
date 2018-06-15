module Samples.WikiModel exposing (source)


source : String
source =
    """# Wiki editor

## Query

### body: `String`

### editing: `Bool`

### editedBody: `String`


## Mutation

### beginEditing

### commitEditing

### discardEditing


## Initial

### body
Hello world

### editing
`.false`


## Update

### beginEditing

#### editing
`.true`

#### editedBody
`$body`

### commitEditing

#### body
`$editedBody`

#### editing
`.false`

#### editedBody
`.empty`

### discardEditing

#### editing
`.false`

#### editedBody
`.empty`

"""
