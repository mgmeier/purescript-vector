## Module Data.TypeNat

#### `Zero`

``` purescript
data Zero
```

##### Instances
``` purescript
instance sz :: Sized Zero
```

#### `Suc`

``` purescript
data Suc a
```

##### Instances
``` purescript
instance ss :: (Sized a) => Sized (Suc a)
```

#### `One`

``` purescript
type One = Suc Zero
```

#### `Two`

``` purescript
type Two = Suc (Suc Zero)
```

#### `Three`

``` purescript
type Three = Suc (Suc (Suc Zero))
```

#### `Four`

``` purescript
type Four = Suc (Suc (Suc (Suc Zero)))
```

#### `Sized`

``` purescript
class Sized a where
  sized :: Proxy a -> Int
```

##### Instances
``` purescript
instance sz :: Sized Zero
instance ss :: (Sized a) => Sized (Suc a)
```


