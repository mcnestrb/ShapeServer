our# ShapeServer DSL

## Running the Code
From the stack app root use:

```
stack ghci
main
```

If any changes are made to the file then within `stack ghci` just use `:r` to reload the files before running `main` again.

## Drawing Description Format
A Drawing is a list of the triple `(Transform, Shape, Style)`

### Transform
* `Identity`: Does nothing to transform the shape
* `Translate (Vector Double Double)`: Translates the object according to the vector in the x and y direction
* `Scale (Vector Double Double`: Scales the object according to the vector in the x and y direction
* `Rotate Double`: Rotates the object about the origin
* `Compose Transform Transform`: Allows the user to use multiple `Transforms`

### Shape
The input box takes in shapes that are either a `Circle` or a `Square`.

### Style
The input box takes in shapes that are either a `Circle` or a `Square`.

### Style
Style is in the form of `Style Colour Colour Double`. `Colours` can be either `Red, Blue, Green, Black or White`
The first `Colour` is the strokeColour
The second `Colour` is the fillColour
The `Double` is the strokeWidth

## Sample Input
