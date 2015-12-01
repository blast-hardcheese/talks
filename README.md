Goals
=====

1. Explore the Simply Typed Lambda Calculus
2. Write some simple programs in a language embedded in Haskell
3. Compile programs written in this language into runnable code
4. Extend the language with types

Extra credit
------------
* Provide more implementations

<b>This is a literate haskell file</b>. For interactive-style editing, type `make watch`, which will use [entr](http://entrproject.org/) to watch for file changes in the source.

```haskell
> {-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}
> module Main where
```

Lambda Calculus as a Basis for our Language
===========================================

Starting off with the definition of HOAS from Phil Freeman's [hoas](https://github.com/paf31/haskell-slides/blob/master/hoas/HOAS.hs) talk, (based on the [Simply Typed Lambda Calculus](https://en.wikipedia.org/wiki/Simply_typed_lambda_calculus)):
```haskell
> class HOAS f where
>   infixr 0 $$
>   ($$) :: f (a -> b) -> f a -> f b
>   infixl 9 $< -- Provide a parameter to allow left-associated applies
>   ($<) :: f (a -> b) -> f a -> f b
>   lam :: (f a -> f b) -> f (a -> b)
```

Building abstractions in the Simply Typed Lambda Calculus
=========================================================

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

To borrow (and slightly modify) Freeman's `PPrint` instance of `HOAS`:
```haskell
> data PPrint a = PPrint { prettyPrint :: Int -> String }

> instance HOAS PPrint where
>   PPrint f $$ PPrint g = PPrint (\i -> "(" ++ (f i) ++ " $ " ++ (g i) ++ ")")
>   PPrint f $< PPrint x = PPrint (\i -> "(" ++ (f i) ++ " " ++ (x i) ++ ")")
>   lam f = PPrint (\i -> "(\\" ++ name i ++ " -> " ++ prettyPrint (f (PPrint (\_ -> name i))) (i + 1) ++ ")")
>     where
>     name :: Int -> String
>     name i = "a" ++ show i
```

... with a small convenience function to make evaluation easier:

```haskell
> runPretty expr = prettyPrint expr 0
```

we can now print the representations of the pure functons we defined above:

```haskell
λ> putStrLn $ prettyPrint konst 0
(\a0 -> (\a1 -> a0))
λ> putStrLn $ prettyPrint app 0
(\a0 -> (\a1 -> (a1 $ a0)))
λ> putStrLn $ prettyPrint hid' 0
((\a0 -> a0) $ (\a0 -> a0))
```

While this is useful, without types, this is still pretty abstract. In order to keep the core language small, we can use `MultiParamTypeClasses` to allow specific Haskell types to be lifted into HOAS:

```haskell
> class HOAS f => HOASType f t where
>   hpure :: t -> f t

> instance HOASType PPrint Bool where
>   hpure x = PPrint (\_ -> show x)

```

Again, in an attempt to keep the core language small, we can define language extensions to use these types as typeclasses as well.

```haskell
> class HOAS f => HOASBoolOps f where
>   ifThenElse :: f Bool -> f a -> f a -> f a
>   hand :: f Bool -> f Bool -> f Bool

> instance HOASBoolOps PPrint where
>   ifThenElse (PPrint pred) (PPrint ts) (PPrint fs) = PPrint (\i -> "if (" ++ (pred i) ++ ") then " ++ ts i ++ " else " ++ fs i)
>   hand (PPrint p1) (PPrint p2) = PPrint (\i -> "(" ++ (p1 i) ++ " && " ++ (p2 i) ++ ")")
```

Now we can represent some simple conditional logic:

```haskell
> hnot :: (HOASType f Bool, HOASBoolOps f) => f (Bool -> Bool)
> hnot = lam $ \x -> ifThenElse x (hpure False) (hpure True)
> -- (\a0 -> if (a0) then False else True)
```

... and expose equality for types we know how to compare (piggybacking on Haskell's `Eq`):

```haskell
> class HOAS f => HOASEqOps f where
>   equals :: Eq a => f (a -> a -> Bool)

> instance HOASEqOps PPrint where
>   equals = lam (\lhs -> lam (\rhs -> PPrint (\i -> (prettyPrint lhs i) ++ " == " ++ (prettyPrint rhs i))))
```

... and as a final exercise, we can implement a simple list in our language:

```haskell
> class HOAS f => HOASListOps f where
>   hcons :: f a -> f [a] -> f [a]
>   hnil :: f [a]
>   hmap :: f (a -> b) -> f [a] -> f [b]
>   (+++) :: f [a] -> f [a] -> f [a]

> instance HOASListOps PPrint where
>   hcons (PPrint lhs) (PPrint arr) = PPrint (\i -> "(" ++ lhs i ++ " : " ++ arr i ++ ")")
>   hnil = PPrint (\_ -> "[]")
>   -- Explicitly using `map` in the generated Haskell, since we're not exposing the Functor typeclass
>   hmap (PPrint f) (PPrint arr) = PPrint (\i -> "(map " ++ f i ++ " " ++ arr i ++ ")")
>   PPrint lhs +++ PPrint rhs = PPrint (\i -> "(" ++ lhs i ++ " ++ " ++ rhs i ++ ")")
```

... which allows us to...

```haskell
> -- Generate
> ex1 :: (HOASType f Bool, HOASListOps f, HOASBoolOps f) => f [Bool]
> ex1 = (hmap hnot (hcons (hpure True) (hcons (hpure False) hnil)))
> -- map (\a0 -> if (a0) then False else True) (True : (False : []))

> ex1' :: [Bool] -- Output from above
> ex1' = map (\a0 -> if (a0) then False else True) (True : (False : []))
> -- [False,True]
```

I guess, with all this cool functionality, we should look at...

Writing a Simple Program
========================

```haskell
> ex2 :: (HOAS f, HOASBoolOps f, HOASEqOps f, HOASListOps f, HOASType f Bool) => f ([Bool] -> Bool -> [Bool])
> ex2 = lam (\arr ->
>         lam (\toInvert ->
>           hmap (lam (\x ->
>                  ifThenElse (equals $< toInvert $< x)
>                    (hnot $< x)
>                    x
>                )) arr))
```

A little dense, but based on the type signature, we've got something that goes from `[Bool] -> Bool -> [Bool]`. Looking at the innermost lambda (in the `map`), if the current element (`x`) is equal to `toInvert`, we should flip `x`, otherwise, leave `x` as it is.

Looking at the `prettyPrint`-ed output (not starting to get not so pretty), we can at least see our `cons` at work, and a couple `if` expressions:

```haskell
λ> putStrLn $ prettyPrint (ex2 $< ((hcons (hpure True) (hcons (hpure False) (hcons (hpure True) (hcons (hpure False) hnil))))) $< (hpure True)) 0
(((\a0 -> (\a1 -> (map (\a2 -> if ((((\a3 -> (\a4 -> a3 == a4)) a1) a2)) then ((\a3 -> if (a3) then False else True) a2) else a2) a0))) (True : (False : (True : (False : []))))) True)
```

Again, while this is pretty neat, we've got to run it in order to see what it does:

```haskell
λ> (((\a0 -> (\a1 -> (map (\a2 -> if ((((\a3 -> (\a4 -> a3 == a4)) a1) a2)) then ((\a3 -> if (a3) then False else True) a2) else a2) a0))) (True : (False : (True : (False : []))))) True)
[False,False,False,False]
```

Flexibility of implementation
=============================

Something worth mentioning is that all of our expressions are of the type `HOAS f => f a`, meaning we don't need to choose an implementation of `HOAS` up front. Let's define some other implementations to see how this works:

Javascript
----------
```haskell
> data PrettyJS a = PrettyJS { prettyJS :: Int -> String }

> instance HOAS PrettyJS where
>   PrettyJS f $$ PrettyJS g = PrettyJS (\i -> "(" ++ (f i) ++ ")(" ++ (g i) ++ ")")
>   PrettyJS f $< PrettyJS x = PrettyJS (\i -> "(" ++ (f i) ++ ")(" ++ (x i) ++ ")")
>   lam f = PrettyJS (\i -> "(function(" ++ name i ++ ") { return " ++ prettyJS (f (PrettyJS (\_ -> name i))) (i + 1) ++ "; })")
>     where
>     name :: Int -> String
>     name i = "a" ++ show i

> instance HOASListOps PrettyJS where
>   hcons (PrettyJS lhs) (PrettyJS arr) = PrettyJS (\i -> "([" ++ lhs i ++ "]).concat(" ++ arr i ++ ")")
>   hnil = PrettyJS (\_ -> "[]")
>   hmap (PrettyJS f) (PrettyJS arr) = PrettyJS (\i -> arr i ++ ".map(" ++ f i ++ ")")
>   PrettyJS lhs +++ PrettyJS rhs = PrettyJS (\i -> "(" ++ lhs i ++ ").concat(" ++ rhs i ++ ")")

> instance HOASBoolOps PrettyJS where
>   ifThenElse (PrettyJS pred) (PrettyJS ts) (PrettyJS fs) = PrettyJS (\i -> "(" ++ (pred i) ++ ") ? " ++ ts i ++ " : " ++ fs i)
>   hand (PrettyJS p1) (PrettyJS p2) = PrettyJS (\i -> "(" ++ (p1 i) ++ " && " ++ (p2 i) ++ ")")

> instance HOASType PrettyJS String where
>   hpure x = PrettyJS (\_ -> show x)

> instance HOASStringOps PrettyJS where
>   hlength (PrettyJS xs) = PrettyJS (\i -> xs i ++ ".length")

> instance HOASType PrettyJS Bool where
>   hpure True = PrettyJS $ \_ -> "true"
>   hpure False = PrettyJS $ \_ -> "false"

> runPrettyJS expr = prettyJS expr 0
```

One important note here, because HOAS is a collection of typeclasses, each expression knows which language it is safe to compile against. If a language doesn't implement all typeclasses, (we've left `HOASEqOps` commented out here)...

```haskell
  instance HOASEqOps PrettyJS where
    equals = lam (\lhs -> lam (\rhs -> PrettyJS (\i -> (prettyJS lhs i) ++ " == " ++ (prettyJS rhs i))))
```

... we can see very clearly what we're missing:

```
λ> putStrLn $ prettyJS ex2 0

<interactive>:4:21:
    No instance for (HOASEqOps PrettyJS) arising from a use of ‘ex2’
    In the first argument of ‘prettyJS’, namely ‘ex2’
    In the second argument of ‘($)’, namely ‘prettyJS ex2 0’
    In the expression: putStrLn $ prettyJS ex2 0
```

Implementing a HOAS -> Haskell evaluator
----------------------------------------

Since we've got a fully abstract call tree, we can actually flatten this down to plain Haskell as well. Type constraints (`Eq`, for instance) will be preserved, so this is sort of like a Haskell sandbox:

```haskell
> data HEval a = HEval { haskellEval :: a }
>   deriving (Show)

> instance HOAS HEval where
>   HEval f $$ HEval g = HEval (f g)
>   lam f = HEval (\x -> haskellEval (f $ HEval x))
>   HEval f $< HEval x = HEval (f x)

> instance HOASListOps HEval where
>   hcons (HEval lhs) (HEval arr) = HEval (lhs : arr)
>   hnil = HEval ([])
>   hmap (HEval f) (HEval arr) = HEval (map f arr)
>   HEval lhs +++ HEval rhs = HEval (lhs ++ rhs)

> instance HOASType HEval Bool where
>   hpure = HEval

> instance HOASBoolOps HEval where
>   ifThenElse (HEval pred) (HEval ts) (HEval fs) = HEval (if (pred) then ts else fs)
>   hand (HEval p1) (HEval p2) = HEval (p1 && p2)

> instance HOASEqOps HEval where
>   equals = lam (\lhs -> lam (\rhs -> HEval $ (haskellEval lhs) == (haskellEval rhs)))

> runHEval expr = haskellEval expr
```

We can even generate well-typed Python:

Python
------
```haskell
> data PrettyPython a = PrettyPython { prettyPython :: Int -> String }

> instance HOAS PrettyPython where
>   PrettyPython f $$ PrettyPython g = PrettyPython (\i -> "(" ++ (f i) ++ ")(" ++ (g i) ++ ")")
>   PrettyPython f $< PrettyPython x = PrettyPython (\i -> "(" ++ (f i) ++ ")(" ++ (x i) ++ ")")
>   lam f = PrettyPython (\i -> "(lambda " ++ name i ++ ": " ++ prettyPython (f (PrettyPython (\_ -> name i))) (i + 1) ++ ")")
>     where
>     name :: Int -> String
>     name i = "a" ++ show i

> instance HOASListOps PrettyPython where
>   hcons (PrettyPython lhs) (PrettyPython arr) = PrettyPython (\i -> "([" ++ lhs i ++ "] + " ++ arr i ++ ")")
>   hnil = PrettyPython (\_ -> "[]")
>   hmap (PrettyPython f) (PrettyPython arr) = PrettyPython (\i -> "map(" ++ f i ++ ", " ++ arr i ++ ")")
>   PrettyPython lhs +++ PrettyPython rhs = PrettyPython (\i -> "((" ++ lhs i ++ ") + (" ++ rhs i ++ "))")

> instance HOASBoolOps PrettyPython where
>   ifThenElse (PrettyPython pred) (PrettyPython ts) (PrettyPython fs) = PrettyPython (\i -> "(" ++ ts i ++ ") if (" ++ (pred i) ++ ") else (" ++ fs i ++ ")")
>   hand (PrettyPython p1) (PrettyPython p2) = PrettyPython (\i -> "(" ++ (p1 i) ++ " and " ++ (p2 i) ++ ")")

> instance HOASType PrettyPython String where
>   hpure x = PrettyPython (\_ -> show x)

> instance HOASStringOps PrettyPython where
>   hlength (PrettyPython xs) = PrettyPython (\i -> "len(" ++ xs i ++ ")")

> instance HOASType PrettyPython Bool where
>   hpure True = PrettyPython $ \_ -> "True"
>   hpure False = PrettyPython $ \_ -> "False"

> instance HOASEqOps PrettyPython where
>   equals = lam (\lhs -> lam (\rhs -> PrettyPython (\i -> (prettyPython lhs i) ++ " == " ++ (prettyPython rhs i))))

> runPrettyPython expr = prettyPython expr 0
```

More types!
===========

Adding `Int`/`Num a`
--------------------

```haskell
> class HOAS f => HOASNumOps f where
>   add :: Num n => f n -> f n -> f n
>   int :: Int -> f Int
```

<h6 id="pprint-int">`PPrint`</h6>
```haskell
> instance HOASType PPrint Int where
>   hpure x = PPrint (\_ -> show x)

> instance HOASNumOps PPrint where
>   add (PPrint lhs) (PPrint rhs) = PPrint (\i -> lhs i ++ " + " ++ rhs i)
>   int = hpure
```

<h6 id="heval-int">`HEval`</h6>
```haskell
> instance HOASType HEval Int where
>   hpure x = HEval x

> instance HOASNumOps HEval where
>   add (HEval lhs) (HEval rhs) = HEval (lhs + rhs)
>   int = hpure
```

Adding `String`
---------------

```haskell
> class HOAS f => HOASStringOps f where
>   hlength :: HOASType f Int => f [a] -> f Int
```

<h6 id="pprint-string">`PPrint`</h6>
```haskell
> instance HOASType PPrint String where
>   hpure x = PPrint (\_ -> show x)

> instance HOASStringOps PPrint where
>   hlength (PPrint xs) = PPrint (\i -> "length " ++ xs i)
```

<h6 id="heval-string">`HEval`</h6>
```haskell
> instance HOASType HEval String where
>   hpure = HEval

> instance HOASStringOps HEval where
>   hlength (HEval xs) = HEval $ length xs
```

More example expressions
========================

```haskell
> -- Simple ITE/Boolean test
> ex'1 :: (HOAS f, HOASBoolOps f, HOASType f Bool) => f Bool
> ex'1 = (ifThenElse (hnot $< (hpure True)) (hpure False) (hpure True))

> -- Map `not` over a list
> ex'2 :: (HOAS f, HOASType f Bool, HOASBoolOps f, HOASListOps f) => f [Bool]
> ex'2 = hmap hnot (hcons (hpure True) (hcons (hpure False) hnil))

> -- Testing literals
> ex'3a :: HOASNumOps f => f Int
> ex'3a = (int 123)
> ex'3b :: HOASType f String => f String
> ex'3b = (hpure "string?")

> -- Prepend to a conditionally generated list
> ex'4 :: (HOASType f Bool, HOASBoolOps f, HOASListOps f) => f [Bool]
> ex'4 = (hcons (hpure True) (ifThenElse (hnot $$ (hpure True)) (hcons (hpure False) (hcons (hpure True) hnil)) hnil))

> ex'5 :: (HOASListOps f, HOASType f Bool, HOASBoolOps f, HOASType f Int, HOASNumOps f, HOASEqOps f) => f [Int]
> ex'5 = (
>          hcons
>            (int 0)
>            (ifThenElse (equals $< (add (int 23) (int 27)) $< (int 50))
>              (hcons (int 1) (hcons (int 2) (hcons (int 3) hnil)))
>              hnil
>            )
>        )

> -- Compare length of a list
> ex'6 :: (HOASListOps f, HOASEqOps f, HOASStringOps f, HOASType f Bool, HOASType f Int) => f Bool
> ex'6 = (equals $< (hlength (hcons (hpure True) hnil)) $< (hpure 0))

> -- Map length over a list of strings
> ex'7 :: (HOASType f Int, HOASType f String, HOASListOps f, HOASStringOps f) => f [Int]
> ex'7 = (hmap (lam hlength) (hcons (hpure "a") (hcons (hpure "be") (hcons (hpure "cee") hnil))))
```

```haskell
> main :: IO ()
> main = putStrLn ""
```
