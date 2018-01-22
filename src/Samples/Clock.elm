module Samples.Clock
    exposing (source)

source : String
source = """# Clock

```svg
{{ clock }}
```

## seconds
```
now.seconds / 60
```

## pi_1_5
```
1.5 * Math.pi
```

## angle
```
Math.turns seconds
+ pi_1_5
```

## handX
```
Math.cos angle
* 38
+ 50
```

## handY
```
Math.sin angle
* 38
+ 50
```

## background
```svg
<circle cx="50" cy="50" r="45" class="text-yellow fill-current" stroke="black" stroke-width="7" />
<circle cx="50" cy="50" r="45" class="text-yellow-darkest stroke-current" fill="none" stroke-width="7" />
<circle cx="50" cy="50" r="2" class="text-yellow-darker fill-current" />
```

## numbers
```svg
<g class="text-yellow-darkest fill-current">
<text x="41" y="28">12</text>
<text x="75" y="58">3</text>
<text x="45" y="87">6</text>
<text x="14" y="58">9</text>
</g>
```

## line
```svg
<line x1="50" y1="50" x2="{{ handX }}" y2="{{ handY }}" stroke="black" stroke-width="2" />
```

## clock
```svg
{{ background }}
{{ numbers }}
{{ line }}
```
"""
