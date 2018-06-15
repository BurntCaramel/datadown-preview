module Samples.WikiModel exposing (source)


source : String
source =
    """# Wiki editor

## Query

### body: `String`

### editing: `Bool`

### editedBody: `String`

### editsCount: `Int`


## Mutation

### beginEditing

### commitEditing

### discardEditing


## Initial

### body
Hello world

### editing
`.false`

### editsCount

0


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

#### editsCount
`$editsCount + 1`

### discardEditing

#### editing
`.false`

#### editedBody
`.empty`

"""
