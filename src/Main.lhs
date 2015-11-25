Goals
=====

- [ ] Parse a language into either GADTs or HOAS
  - [ ] Understand how to use HOAS
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
>   equals :: f a -> f a -> f Bool
>
>   true :: f Bool
>   false :: f Bool
>   hnot :: f Bool -> f Bool
>   int :: Int -> f Int
>
>   hcons :: f a -> f [a] -> f [a]
>   hnil :: f [a]
>   hmap :: f (a -> b) -> f [a] -> f [b]
>
>   add :: f Int -> f Int -> f Int
>
>   ifThenElse :: f Bool -> f a -> f a -> f a
>
```

> class HOAS f => HOASType f t where
>   hpure :: t -> f t

> class HOAS f => HOASStringOps f where
>   hlength :: f [a] -> f Int

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
>   equals (PPrint lhs) (PPrint rhs) = PPrint (\i -> lhs i ++ " == " ++ rhs i)
>
>   true = PPrint (\_ -> "True")
>   false = PPrint (\_ -> "False")
>   hnot (PPrint value) = PPrint (\i -> "(" ++ "not " ++ value i ++ ")")
>   int x = PPrint (\_ -> show x)
>
>   hcons (PPrint lhs) (PPrint arr) = PPrint (\i -> "(" ++ (lhs i) ++ " : " ++ (arr i) ++ ")")
>   hnil = PPrint (\_ -> "[]")
>   hmap (PPrint f) (PPrint arr) = PPrint (\i -> "map " ++ f i ++ " " ++ arr i)
>
>   add (PPrint lhs) (PPrint rhs) = PPrint (\i -> lhs i ++ " + " ++ rhs i)
>
>   ifThenElse (PPrint pred) (PPrint ts) (PPrint fs) = PPrint (\i -> "if (" ++ (pred i) ++ ") then " ++ ts i ++ " else " ++ fs i)

> instance HOASType PPrint Int where
>   hpure x = PPrint (\_ -> show x)

> instance HOASType PPrint String where
>   hpure x = PPrint (\_ -> show x)

> instance HOASStringOps PPrint where
>   hlength (PPrint xs) = PPrint (\i -> "length " ++ xs i)

>
> pprintMain :: IO ()
> pprintMain = do
>   putStrLn $ prettyPrint (ifThenElse (hnot true) false true) 0
>   putStrLn $ prettyPrint (hmap (lam hnot) (hcons true (hcons false hnil))) 0
>   putStrLn $ prettyPrint (lam hnot) 0
>   putStrLn $ prettyPrint (hpure (123 :: Int)) 0
>   putStrLn $ prettyPrint (hpure "string?") 0
>   putStrLn $ prettyPrint (hcons true (ifThenElse (hnot true) (hcons false (hcons true hnil)) hnil)) 0
>   putStrLn $ prettyPrint (hcons
>                                 (hpure (0 :: Int)) -- Unpleasant wart
>                                 (ifThenElse (equals (add (hpure 23) (hpure 27)) (hpure 50))
>                                   (hcons (hpure 1) (hcons (hpure 2) (hcons (hpure 3) hnil)))
>                                   hnil
>                                 )
>                          ) 0
>   putStrLn $ prettyPrint (ifThenElse (equals (hlength (hcons true hnil)) (hpure 0)) true false) 0
>   putStrLn $ prettyPrint (equals (hlength (hcons true hnil)) (hpure 0)) 0
>   putStrLn $ prettyPrint (hmap (lam hlength) (hcons (hpure "foo") (hcons (hpure "bar") (hcons (hpure "baz") hnil)))) 0
```

```haskell
> main :: IO ()
> main = do
>   pprintMain
```
