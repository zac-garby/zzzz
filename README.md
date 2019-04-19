<h1 align="center">zzzz</h1>

A non-strictly-evaluated Lisp.

```lisp
(head (range 1 inf))
 => 1
```

 > This particular example doesn't _actually_ work at the moment because pretty much no functions are implemented.

This isn't a _real_ programming language, in that it would be a terrible terrible idea to use it for any practical purpose. It's just for me to learn more how non-strict semantics work.

## Usage

```bash
cabal new-run
```

 > You can use `cabal run` instead, but it will start screaming at you to get you to use `cabal new-run`.

This will open up a repl, and you can enter things like this:

```
zzzz> (+ 1 2)
3.0

zzzz> (double 5)
10.0
```

 > Some basic expressions

```
zzzz> ((lambda (a) (if a 'false 'true)) 'true)
'false
```

 > This shows how a lambda function can be defined and applied to an argument. This particular lambda function is a simplified version of `not` from the standard library.

```
zzzz> (= not (lambda (a) (if (= a 'true) 'false (if (= a 'false) 'true a))))
'true
```

 > Functions are equal to each other if they consist of identical expressions. `not` is defined in the standard library.

```
zzzz> (if 'true 5 ⊥)
5.0
```

 > Non-strict semantics. `⊥` refers to the "bottom" value, i.e. undefined. Despite an undefined value appearing, the program exits successfuly and returns the correct answer, and this is because of the non-strict nature of the language.
 
```
zzzz> (let (x 1 y 2) (+ x y))
3.0
```

 > Let constructs. This is actually just syntax sugar and will be internally transformed into `((lambda (x y) (+ x y)) 1 2)`.

```
zzzz> (def fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))
(lambda (n) (if (= n 0.0) 1.0 (* n (fact (- n 1.0)))))

zzzz> (fact 5)
120
```

 > This shows how a function can be defined. Variables can be defined the exact same way.

### TODO List

 - Make sure not to substitute values into parameter lists in lambdas/lets/etc...
 - Trim function bodies in `Show` instance if they get too long.
 - Give an error if the entire input is not parsed in the REPL.