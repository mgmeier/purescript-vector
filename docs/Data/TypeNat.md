## Module Data.TypeNat

Simple type level naturals for vector and matrix sizes

#### `Zero`

``` purescript
data Zero
```

##### Instances
``` purescript
Sized Zero
```

#### `One`

``` purescript
data One
```

##### Instances
``` purescript
Sized One
```

#### `Two`

``` purescript
data Two
```

##### Instances
``` purescript
Sized Two
```

#### `Three`

``` purescript
data Three
```

##### Instances
``` purescript
Sized Three
```

#### `Four`

``` purescript
data Four
```

##### Instances
``` purescript
Sized Four
```

#### `Sized`

``` purescript
class Sized a where
  sized :: Proxy a -> Int
```

##### Instances
``` purescript
Sized Zero
Sized One
Sized Two
Sized Three
Sized Four
```


