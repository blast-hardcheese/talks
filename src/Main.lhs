Goals
=====

- [ ] Parse a language into HOAS
  - [x] Understand how to use HOAS
  - [ ] Write a small program in HOAS in Haskell
  - [ ] Compile HOAS to source in some language
  - [ ] POC parser that outputs HOAS

This is a literate haskell file. For interactive-style editing, type `make watch`, which will use [entr](http://entrproject.org/) to watch for file changes in the source.

```haskell
> {-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
> module Main where
```

Starting off with the definition of HOAS from Phil Freeman's [hoas](https://github.com/paf31/haskell-slides/blob/master/hoas/HOAS.hs) talk:
```haskell
> class HOAS f where
>   infixr 0 $$
>   ($$) :: f (a -> b) -> f a -> f b
>   lam :: (f a -> f b) -> f (a -> b)
```

As Phil put it, this is the minimal useful implementation of the lambda calculus. We've got a fair amount of primitive expression:

<h6 id="pure_identity">Identity</h6>
```haskell
> hid :: HOAS f => f (a -> a)
> hid = lam $ \x -> x
```

<h6 id="pure_first-order-functions">First-order functions</h6>
```haskell
> hid' :: HOAS f => f (a -> a)
> hid' = hid $$ hid
```

<h6 id="pure_application">Function application (Freeman)</h6>
```haskell
> app :: HOAS f => f (a -> (a -> b) -> b)
> app = lam $ \a -> lam $ \f -> f $$ a
```

<h6 id="pure_value-manipulation">Value manipulation (Freeman)</h6>
```haskell
> konst :: HOAS f => f (a -> b -> a)
> konst = lam $ \a -> lam $ \_ -> a
```

<h6 id="pure_composition">Function composition</h6>
```haskell
> infixr 9 $.
> ($.) :: HOAS f => f (b -> c) -> f (a -> b) -> f (a -> c)
> g $. f = lam $ \x -> g $$ (f $$ x)
```

<h6 id="pure_parameter_flip">Parameter flipping</h6>
```haskell
> hflip :: HOAS f => f ((a -> b -> c) -> (b -> a -> c))
> hflip = lam (\f -> lam (\x -> lam (\y -> (f $$ y) $$ x)))
```

<h6 id="pure_church_numerals">Church Numerals</h6>
```haskell
> hsucc :: HOAS f => f (((a -> a) -> a -> a) -> (a -> a) -> a -> a)
> hsucc = lam (\next -> lam (\f -> lam (\x -> f $$ ((next $$ f) $$ x))))

> hzero :: HOAS f => f ((a -> a) -> a -> a)
> hzero = konst $$ hid

> hone :: HOAS f => f ((a -> a) -> a -> a)
> hone = hsucc $$ hzero

> htwo :: HOAS f => f ((a -> a) -> a -> a)
> htwo = hsucc $$ hsucc $$ hzero
```

... all without providing an instance of `HOAS` or concrete types.

Starting with a modified version of Freeman's PPrint instance of HOAS
```haskell
> data PPrint a = PPrint { prettyPrint :: Int -> String }
>
> instance HOAS PPrint where
>   PPrint f $$ PPrint g = PPrint (\i -> "(" ++ (f i) ++ " $ " ++ (g i) ++ ")")
>   lam f = PPrint (\i -> "(\\" ++ name i ++ " -> " ++ prettyPrint (f (PPrint (\_ -> name i))) (i + 1) ++ ")")
>     where
>     name :: Int -> String
>     name i = "a" ++ show i

> data HEval a = HEval { haskellEval :: a }
>   deriving (Show)
>
> instance HOAS HEval where
>   HEval f $$ HEval g = HEval (f $ g)
    lam :: (f a -> f b) -> f (a -> b)
>   lam f = HEval (\x -> haskellEval (f $ HEval x))
```

```haskell
> class HOAS f => HOASType f t where
>   hpure :: t -> f t

> class HOAS f => HOASNumOps f where
>   add :: Num n => f n -> f n -> f n
>   int :: Int -> f Int

-- Using church numerals
> addOne :: HOASNumOps f => f (Int -> Int)
> addOne = (lam $ add (int 1))

> runChurchTwo :: HOASNumOps f => f Int
> runChurchTwo = (htwo $$ addOne) $$ (int 0)

> class HOAS f => HOASListOps f where
>   hcons :: f a -> f [a] -> f [a]
>   hnil :: f [a]
>   hmap :: f (a -> b) -> f [a] -> f [b]

> class HOAS f => HOASBoolOps f where
>   ifThenElse :: f Bool -> f a -> f a -> f a
>   hnot :: f Bool -> f Bool

> class HOAS f => HOASStringOps f where
>   hlength :: f [a] -> f Int

> class HOAS f => HOASEqOps f where
>   equals :: Eq a => f a -> f a -> f Bool

> instance HOASListOps PPrint where
>   hcons (PPrint lhs) (PPrint arr) = PPrint (\i -> "(" ++ (lhs i) ++ " : " ++ (arr i) ++ ")")
>   hnil = PPrint (\_ -> "[]")
>   hmap (PPrint f) (PPrint arr) = PPrint (\i -> "map " ++ f i ++ " " ++ arr i)

> instance HOASNumOps PPrint where
>   add (PPrint lhs) (PPrint rhs) = PPrint (\i -> lhs i ++ " + " ++ rhs i)
>   int = hpure

> instance HOASType PPrint Bool where
>   hpure x = PPrint (\_ -> show x)

> instance HOASBoolOps PPrint where
>   ifThenElse (PPrint pred) (PPrint ts) (PPrint fs) = PPrint (\i -> "if (" ++ (pred i) ++ ") then " ++ ts i ++ " else " ++ fs i)
>   hnot (PPrint value) = PPrint (\i -> "(" ++ "not " ++ value i ++ ")")

> instance HOASType PPrint Int where
>   hpure x = PPrint (\_ -> show x)

> instance HOASType PPrint String where
>   hpure x = PPrint (\_ -> show x)

> instance HOASStringOps PPrint where
>   hlength (PPrint xs) = PPrint (\i -> "length " ++ xs i)

> instance HOASEqOps PPrint where
>   equals (PPrint lhs) (PPrint rhs) = PPrint (\i -> lhs i ++ " == " ++ rhs i)


> instance HOASType HEval Int where
>   hpure x = HEval x

> instance HOASListOps HEval where
>   hcons (HEval lhs) (HEval arr) = HEval (lhs : arr)
>   hnil = HEval ([])
>   hmap (HEval f) (HEval arr) = HEval (map f arr)

> instance HOASNumOps HEval where
>   add (HEval lhs) (HEval rhs) = HEval (lhs + rhs)
>   int = hpure

> pprintMain :: IO ()
> pprintMain = do
>   putStrLn $ prettyPrint (ifThenElse (hnot (hpure True)) (hpure False) (hpure True)) 0
>   putStrLn $ prettyPrint (hmap (lam hnot) (hcons (hpure True) (hcons (hpure False) hnil))) 0
>   putStrLn $ prettyPrint (lam hnot) 0
>   putStrLn $ prettyPrint (int 123) 0
>   putStrLn $ prettyPrint (hpure "string?") 0
>   putStrLn $ prettyPrint (hcons (hpure True) (ifThenElse (hnot (hpure True)) (hcons (hpure False) (hcons (hpure True) hnil)) hnil)) 0
>   putStrLn $ prettyPrint (hcons
>                                 (int 0)
>                                 (ifThenElse (equals (add (int 23) (int 27)) (int 50))
>                                   (hcons (int 1) (hcons (int 2) (hcons (int 3) hnil)))
>                                   hnil
>                                 )
>                          ) 0
>   putStrLn $ prettyPrint (ifThenElse (equals (hlength (hcons (hpure True) hnil)) (hpure 0)) (hpure True) (hpure False)) 0
>   putStrLn $ prettyPrint (equals (hlength (hcons (hpure True) hnil)) (hpure 0)) 0
>   putStrLn $ prettyPrint (hmap (lam hlength) (hcons (hpure "foo") (hcons (hpure "bar") (hcons (hpure "baz") hnil)))) 0
```

```haskell
> main :: IO ()
> main = do
>   pprintMain
```
