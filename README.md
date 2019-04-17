<h1 align="center">zzzz</h1>

A lazily-evaluated lisp.

```lisp
(head (range 1 inf))
 => 1
```

 > This particular example doesn't _actually_ work at the moment because pretty much no functions are implemented.

This isn't a _real_ programming language, in that it would be a terrible terrible idea to use it for any practical purpose. It's just for me to learn more how a lazily-evaluated language works.

## Usage

```bash
cabal new-run
```

 > You can use `cabal run` instead, but it will start screaming at you to get you to use `cabal new-run`.

This will open up a repl, and you can enter things like this:

```
zzzz> (double 5)
10.0

zzzz> (+ 1 2)
3.0
```
