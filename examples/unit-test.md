# Welcome screen

## firstName
Jane

## lastName
Doe

## fullName
{{ firstName }} {{ lastName }}

## render
```html
<div>Welcome, {{ fullName }}!</div>
```

---

> ##firstName
> Jane

> ## lastName
> Doe

> ## fullName
> Jane Doe

---

> ## fullName
> Bob Marley

> ## render
```html
<div>Welcome, Bob Marley!</div>
```