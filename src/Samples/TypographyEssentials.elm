module Samples.TypographyEssentials exposing (source)


source : String
source =
    """# Typography Essentials

## FontFamily: `String`
- System
- Georgia
- Columns

## Query

### content: `[String]`

### fontSize: `Int`

### measure: `Int`

### fontFamily: `FontFamily`


## Mutation

### decreaseFontSize

### increaseFontSize

### decreaseMeasure

### increaseMeasure

### changeFontFamily
#### fontFamily: `FontFamily`

### changeContent
#### content: `[String]`


## Initial

### content

Zebras are mammals of the family Equidae. Zebras are African horses. They are in the same genus as the common horse, Equus caballus, and donkeys. Zebras are known for having many black and white stripes. There are three main species of zebra, Grevy's Zebra, the Plains Zebra, and the Mountain Zebra. In 2004, new research led to reclassifying zebra subspecies.

All zebras have very short fur because they live in hot areas. Their fur has black and white stripes. Scientists don't know exactly why they have stripes, but they predict its for some form of camouflage. The main part of the body has mostly vertical stripes, and the legs have horizontal stripes. They also have a dark line directly down their spine. Each of the different zebra species has different type of stripes. Each zebra has a unique pattern.

Zebras are social animals that spend time in herds, they graze together and sometimes even groom each other[2]. They can have babies (foals) when they are about five years old and can have one every year. Zebras mainly eat grass, but they also eat fruit, leaves and some vegetables. They always live near water and are an endangered species.

Zebras live in Africa, south of the Sahara desert.

Source: https://simple.wikipedia.org/wiki/Zebra

### fontSize
`16`

### measure
`40`


## Update

### decreaseFontSize

#### fontSize

`$fontSize - 1`

### increaseFontSize

#### fontSize

`$fontSize + 1`

### decreaseMeasure

#### measure

`$measure - 1`

### increaseMeasure

#### measure

`$measure + 1`


## view

```html
<p>{{ content }}</p>
```

"""
