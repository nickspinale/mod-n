# mod-n

The integers under a modulus, using `GHC.TypeLits`.
Values are not constructed directly, but are calculated by `fromInteger`.
The fact that the `Mod` type's constructor is not exposed ensures the following invariant:

```haskell
0 <= fromIntegral (x :: Mod n) < natVal' (proxy# :: Proxy# n)
```

This package is mostly just an auxilary for [bigword](https://github.com/nickspinale/bigword).
It just seemed like a general (and useful) enough type to exist on its own.
