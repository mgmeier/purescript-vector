## Module Data.Vector3

Binding to mjs library

#### `Vec3`

``` purescript
type Vec3 = Vec Three
```

#### `vec3`

``` purescript
vec3 :: forall a. a -> a -> a -> Vec3 a
```

#### `vec3'`

``` purescript
vec3' :: forall a. Array a -> Vec3 a
```

#### `i3`

``` purescript
i3 :: Vec3 Number
```

#### `j3`

``` purescript
j3 :: Vec3 Number
```

#### `k3`

``` purescript
k3 :: Vec3 Number
```

#### `get3X`

``` purescript
get3X :: forall a. Vec3 a -> a
```

#### `get3Y`

``` purescript
get3Y :: forall a. Vec3 a -> a
```

#### `get3Z`

``` purescript
get3Z :: forall a. Vec3 a -> a
```

#### `set3X`

``` purescript
set3X :: forall a. a -> Vec3 a -> Vec3 a
```

#### `set3Y`

``` purescript
set3Y :: forall a. a -> Vec3 a -> Vec3 a
```

#### `set3Z`

``` purescript
set3Z :: forall a. a -> Vec3 a -> Vec3 a
```

#### `cross`

``` purescript
cross :: forall a. (Num a) => Vec3 a -> Vec3 a -> Vec3 a
```

The cross product of a and b


