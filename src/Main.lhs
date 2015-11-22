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
```

```haskell
> main :: IO ()
> main = do
>   putStrLn "hello world"
```
