Goals
=====

- [ ] Parse a language into either GADTs or HOAS
  - [ ] Understand how to use HOAS
  - [ ] Write a small program in HOAS in Haskell
  - [ ] Compile HOAS to source in some language
  - [ ] POC parser that outputs HOAS

This is a literate haskell file. For interactive-style editing, type `make watch`, which will use [entr](http://entrproject.org/) to watch for file changes in the source.

```haskell
> module Main where
```

Starting off with the definition of HOAS from Phil Freeman's [hoas](https://github.com/paf31/haskell-slides/blob/master/hoas/HOAS.hs) talk:
```haskell
> class HOAS f where
>   ($$) :: f (a -> b) -> f a -> f b
>   lam :: (f a -> f b) -> f (a -> b)
>
>   true :: f Bool
>   false :: f Bool
>
>   ifThenElse :: f Bool -> f a -> f a -> f a
>
> data PPrint a = PPrint { prettyPrint :: Int -> String, atomic :: Bool }
>
> instance HOAS PPrint where
>   PPrint f at1 $$ PPrint g at2 = PPrint (\i -> parens at1 (f i) ++ " $$ " ++ parens at2 (g i)) False
>     where
>     parens True  x = x
>     parens False x = "(" ++ x ++ ")"
>   lam f = PPrint (\i -> "(\\" ++ name i ++ " -> " ++ prettyPrint (f (PPrint (\_ -> name i) True)) (i + 1) ++ ")") False
>     where
>     name :: Int -> String
>     name i = "a" ++ show i
>
>   true = PPrint (\_ -> "True") True
>   false = PPrint (\_ -> "False") True
>
>   ifThenElse (PPrint pred _) (PPrint ts _) (PPrint fs _) = PPrint (\i -> "if (" ++ (pred i) ++ ") then " ++ ts i ++ " else " ++ fs i) False
```

```haskell
> main :: IO ()
> main = do
>   putStrLn $ prettyPrint (ifThenElse true false true) 0
```
