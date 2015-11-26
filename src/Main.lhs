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
>   ($$) :: f (a -> b) -> f a -> f b
>   lam :: (f a -> f b) -> f (a -> b)
```

> class HOAS f => HOASType f t where
>   hpure :: t -> f t

> class HOAS f => HOASNumOps f where
>   add :: Num n => f n -> f n -> f n
>   int :: Int -> f Int

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

Pretty Printing instance of HOAS
```haskell
> data PPrint a = PPrint { prettyPrint :: Int -> String }
>
> instance HOAS PPrint where
>   PPrint f $$ PPrint g = PPrint (\i -> parens (f i) ++ " $$ " ++ parens (g i))
>     where
>     parens x = "(" ++ x ++ ")"
>   lam f = PPrint (\i -> "(\\" ++ name i ++ " -> " ++ prettyPrint (f (PPrint (\_ -> name i))) (i + 1) ++ ")")
>     where
>     name :: Int -> String
>     name i = "a" ++ show i

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
