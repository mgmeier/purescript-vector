# Module Documentation

## Module Data.TypeNat

### Types

    newtype Four where
      Four :: Suc (Suc (Suc (Suc Zero))) -> Four

    newtype One where
      One :: Suc Zero -> One

    data Suc a

    newtype Three where
      Three :: Suc (Suc (Suc Zero)) -> Three

    newtype Two where
      Two :: Suc (Suc Zero) -> Two

    data Zero


### Type Classes

    class Sized a where
      sized :: a -> Number


## Module Data.Vector

### Types

    newtype Vec s a where
      Vec :: [a] -> Vec s a


### Type Classes

    class (Sized v) <= Vector v where


### Type Class Instances

    instance applyVec :: Apply (Vec s)

    instance eqVec :: (Eq a) => Eq (Vec s a)

    instance foldableVector :: Foldable (Vec s)

    instance functorVec :: Functor (Vec s)

    instance showVec :: (Show a) => Show (Vec s a)

    instance sv1 :: Sized (Vec One a)

    instance sv2 :: Sized (Vec Two a)

    instance sv3 :: Sized (Vec Three a)

    instance sv4 :: Sized (Vec Four a)


### Values

    add :: forall a s. (Num a) => Vec s a -> Vec s a -> Vec s a

    direction :: forall s. Vec s Number -> Vec s Number -> Vec s Number

    distance :: forall s. Vec s Number -> Vec s Number -> Number

    distanceSquared :: forall s. Vec s Number -> Vec s Number -> Number

    dot :: forall s. Vec s Number -> Vec s Number -> Number

    fromArray :: forall s a. (Vector (Vec s a)) => [a] -> Vec s a

    mult :: forall a s. (Num a) => Vec s a -> Vec s a -> Vec s a

    normalize :: forall s. Vec s Number -> Vec s Number

    scale :: forall a s. (Num a) => a -> Vec s a -> Vec s a

    sub :: forall a s. (Num a) => Vec s a -> Vec s a -> Vec s a

    toArray :: forall s a. Vec s a -> [a]

    vlength :: forall s. Vec s Number -> Number

    vlengthSquared :: forall s. Vec s Number -> Number

    vnegate :: forall a s. (Num a) => Vec s a -> Vec s a


## Module Data.Vector2

### Types

    type Vec2 = Vec Two


### Values

    getX :: forall a. Vec2 a -> a

    getY :: forall a. Vec2 a -> a

    i :: Vec2 Number

    j :: Vec2 Number

    setX :: forall a. a -> Vec2 a -> Vec2 a

    setY :: forall a. a -> Vec2 a -> Vec2 a

    vec2 :: forall a. a -> a -> Vec2 a

    vec2' :: forall a. [a] -> Vec2 a


## Module Data.Vector3

### Types

    type Vec3 = Vec Three


### Values

    cross :: forall a. (Num a) => Vec3 a -> Vec3 a -> Vec3 a

    getX :: forall a. Vec3 a -> a

    getY :: forall a. Vec3 a -> a

    getZ :: forall a. Vec3 a -> a

    i :: Vec3 Number

    j :: Vec3 Number

    k :: Vec3 Number

    setX :: forall a. a -> Vec3 a -> Vec3 a

    setY :: forall a. a -> Vec3 a -> Vec3 a

    setZ :: forall a. a -> Vec3 a -> Vec3 a

    vec3 :: forall a. a -> a -> a -> Vec3 a

    vec3' :: forall a. [a] -> Vec3 a


## Module Data.Vector4

### Types

    type Vec4 = Vec Four


### Values

    getU :: forall a. Vec4 a -> a

    getX :: forall a. Vec4 a -> a

    getY :: forall a. Vec4 a -> a

    getZ :: forall a. Vec4 a -> a

    i :: Vec4 Number

    j :: Vec4 Number

    k :: Vec4 Number

    l :: Vec4 Number

    setU :: forall a. a -> Vec4 a -> Vec4 a

    setX :: forall a. a -> Vec4 a -> Vec4 a

    setY :: forall a. a -> Vec4 a -> Vec4 a

    setZ :: forall a. a -> Vec4 a -> Vec4 a

    vec4 :: forall a. a -> a -> a -> a -> Vec4 a

    vec4' :: forall a. [a] -> Vec4 a