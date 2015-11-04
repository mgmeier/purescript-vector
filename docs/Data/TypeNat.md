## Module Data.TypeNat

#### `Zero`

``` purescript
data Zero
```

##### Instances
``` purescript
instance sz :: Sized Zero
```

#### `One`

``` purescript
data One
```

##### Instances
``` purescript
instance s1 :: Sized One
```

#### `Two`

``` purescript
data Two
```

##### Instances
``` purescript
instance s2 :: Sized Two
```

#### `Three`

``` purescript
data Three
```

##### Instances
``` purescript
instance s3 :: Sized Three
```

#### `Four`

``` purescript
data Four
```

##### Instances
``` purescript
instance s4 :: Sized Four
```

#### `Sized`

``` purescript
class Sized a where
  sized :: Proxy a -> Int
```

##### Instances
``` purescript
instance sz :: Sized Zero
instance s1 :: Sized One
instance s2 :: Sized Two
instance s3 :: Sized Three
instance s4 :: Sized Four
```


