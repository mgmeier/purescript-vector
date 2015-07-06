# Module Documentation

## Module Data.TypeNat


#### `Zero`

``` purescript
data Zero
```

#### `Suc`

``` purescript
data Suc a
```


#### `One`

``` purescript
newtype One
  = One (Suc Zero)
```


#### `Two`

``` purescript
newtype Two
  = Two (Suc (Suc Zero))
```


#### `Three`

``` purescript
newtype Three
  = Three (Suc (Suc (Suc Zero)))
```


#### `Four`

``` purescript
newtype Four
  = Four (Suc (Suc (Suc (Suc Zero))))
```


#### `Sized`

``` purescript
class Sized a where
  sized :: a -> Number
```



## Module Data.Vector


#### `Vec`

``` purescript
newtype Vec s a
  = Vec [a]
```


#### `Vector`

``` purescript
class (Sized v) <= Vector v where
```


#### `sv1`

``` purescript
instance sv1 :: Sized (Vec One a)
```


#### `sv2`

``` purescript
instance sv2 :: Sized (Vec Two a)
```


#### `sv3`

``` purescript
instance sv3 :: Sized (Vec Three a)
```


#### `sv4`

``` purescript
instance sv4 :: Sized (Vec Four a)
```


#### `fromArray`

``` purescript
fromArray :: forall s a. (Vector (Vec s a)) => [a] -> Vec s a
```


#### `toArray`

``` purescript
toArray :: forall s a. Vec s a -> [a]
```


#### `eqVec`

``` purescript
instance eqVec :: (Eq a) => Eq (Vec s a)
```


#### `showVec`

``` purescript
instance showVec :: (Show a) => Show (Vec s a)
```


#### `functorVec`

``` purescript
instance functorVec :: Functor (Vec s)
```


#### `applyVec`

``` purescript
instance applyVec :: Apply (Vec s)
```


#### `foldableVector`

``` purescript
instance foldableVector :: Foldable (Vec s)
```


#### `add`

``` purescript
add :: forall a s. (Num a) => Vec s a -> Vec s a -> Vec s a
```


#### `sub`

``` purescript
sub :: forall a s. (Num a) => Vec s a -> Vec s a -> Vec s a
```


#### `mult`

``` purescript
mult :: forall a s. (Num a) => Vec s a -> Vec s a -> Vec s a
```


#### `vnegate`

``` purescript
vnegate :: forall a s. (Num a) => Vec s a -> Vec s a
```


#### `direction`

``` purescript
direction :: forall s. Vec s Number -> Vec s Number -> Vec s Number
```

The normalized direction from a to b: (a - b) / |a - b|

#### `vlengthSquared`

``` purescript
vlengthSquared :: forall s. Vec s Number -> Number
```

The length of the given vector: |a|

#### `vlength`

``` purescript
vlength :: forall s. Vec s Number -> Number
```

The length of the given vector: |a|

#### `normalize`

``` purescript
normalize :: forall s. Vec s Number -> Vec s Number
```

#### `distanceSquared`

``` purescript
distanceSquared :: forall s. Vec s Number -> Vec s Number -> Number
```

The distance between two vectors.

#### `distance`

``` purescript
distance :: forall s. Vec s Number -> Vec s Number -> Number
```

The distance between two vectors.

#### `scale`

``` purescript
scale :: forall a s. (Num a) => a -> Vec s a -> Vec s a
```

Multiply the vector by a scalar: s * v

#### `dot`

``` purescript
dot :: forall s. Vec s Number -> Vec s Number -> Number
```

The dot product of a and b


## Module Data.Vector2


#### `Vec2`

``` purescript
type Vec2 = Vec Two
```


#### `vec2`

``` purescript
vec2 :: forall a. a -> a -> Vec2 a
```


#### `vec2'`

``` purescript
vec2' :: forall a. [a] -> Vec2 a
```


#### `i`

``` purescript
i :: Vec2 Number
```


#### `j`

``` purescript
j :: Vec2 Number
```


#### `getX`

``` purescript
getX :: forall a. Vec2 a -> a
```


#### `getY`

``` purescript
getY :: forall a. Vec2 a -> a
```


#### `setX`

``` purescript
setX :: forall a. a -> Vec2 a -> Vec2 a
```


#### `setY`

``` purescript
setY :: forall a. a -> Vec2 a -> Vec2 a
```



## Module Data.Vector3


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
vec3' :: forall a. [a] -> Vec3 a
```


#### `i`

``` purescript
i :: Vec3 Number
```


#### `j`

``` purescript
j :: Vec3 Number
```


#### `k`

``` purescript
k :: Vec3 Number
```


#### `getX`

``` purescript
getX :: forall a. Vec3 a -> a
```


#### `getY`

``` purescript
getY :: forall a. Vec3 a -> a
```


#### `getZ`

``` purescript
getZ :: forall a. Vec3 a -> a
```


#### `setX`

``` purescript
setX :: forall a. a -> Vec3 a -> Vec3 a
```


#### `setY`

``` purescript
setY :: forall a. a -> Vec3 a -> Vec3 a
```


#### `setZ`

``` purescript
setZ :: forall a. a -> Vec3 a -> Vec3 a
```


#### `cross`

``` purescript
cross :: forall a. (Num a) => Vec3 a -> Vec3 a -> Vec3 a
```

The cross product of a and b


## Module Data.Vector4


#### `Vec4`

``` purescript
type Vec4 = Vec Four
```


#### `vec4`

``` purescript
vec4 :: forall a. a -> a -> a -> a -> Vec4 a
```


#### `vec4'`

``` purescript
vec4' :: forall a. [a] -> Vec4 a
```


#### `i`

``` purescript
i :: Vec4 Number
```


#### `j`

``` purescript
j :: Vec4 Number
```


#### `k`

``` purescript
k :: Vec4 Number
```


#### `l`

``` purescript
l :: Vec4 Number
```


#### `getX`

``` purescript
getX :: forall a. Vec4 a -> a
```


#### `getY`

``` purescript
getY :: forall a. Vec4 a -> a
```


#### `getZ`

``` purescript
getZ :: forall a. Vec4 a -> a
```


#### `getU`

``` purescript
getU :: forall a. Vec4 a -> a
```


#### `setX`

``` purescript
setX :: forall a. a -> Vec4 a -> Vec4 a
```


#### `setY`

``` purescript
setY :: forall a. a -> Vec4 a -> Vec4 a
```


#### `setZ`

``` purescript
setZ :: forall a. a -> Vec4 a -> Vec4 a
```


#### `setU`

``` purescript
setU :: forall a. a -> Vec4 a -> Vec4 a
```