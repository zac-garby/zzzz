; repeats a value infinitely
; e.g. (repeat 1) -> [1 1 1 1 1 1 ...]
(defun repeat (x) (cons x (repeat x)))

; replicates some value x, n times
; e.g. (repl 5 'e') -> "eeeee"
(defun repl (n) (. (take n) repeat))

; range generates a range of values, starting from an initial
; value until the final value is reached or exceeded.
; e.g. (range 1 0.5 2) -> [1, 1.5, 2]
(defun range (s d f)
  (if (eq s f) [s]
    (let (cmp (if (lt s f) gt lt))
      (if (cmp (+ s d) f) []
        (cons s (range (+ s d) d f))))))

; range. is the same as range, but automatically infers a
; step to use. if the start value is smaller than the final
; value, a step of 1 is used. otherwise, -1.
(defun range. (s f)
  (range s (if (lt s f) 1 (- 0 1)) f))

; takes n values from the start of xs
; e.g. (take 2 [1 2 3 4]) -> [1 2]
(defun take (n xs)
  (if (eq n 0)
    []
    (cons (head xs)
          (take (- n 1) (tail xs)))))

; applies f to every value in xs
; e.g. (map (+ 1) [1 2 3]) -> [2 3 4]
(defun map (f xs)
  (if (null xs)
    []
    (cons (f (head xs))
          (map f (tail xs)))))

; folds xs from the right, starting with the initial value i,
; using the function f.
; e.g. (foldr + 0 [1 2 3]) -> (+ 1 (+ 2 (+ 3 0))) -> 6
(defun foldr (f i xs)
  (if (null xs)
    i
    (f (head xs) (foldr f i (tail xs)))))

; filters xs with the predicate f
; e.g. (filter (eq 2) [1 2 3 4 5 4 3 2 1]) -> [2 2]
(defun filter (f xs)
  (if (null xs)
    []
    (let (hd (head xs))
      (if (f hd)
        (cons hd (filter f (tail xs)))
        (filter f (tail xs))))))

; computes the length of a list.
(defun len (xs)
  (if (null xs)
    0
    (+ 1 (len (tail xs)))))

; composes two unary functions together, such that the output
; from the second one is fed into the first one.
; e.g. ((. (+ 2) (* 2)) 5) -> 12
(defun . (f g x)
  (f (g x)))
 
; applies a value to the second parameter of a function instead
; of the first. 'lhs' stands for left-hand-side, leading to code
; like (map (lhs - 2) [1 2 3]), which gives the impression of 'lhs'
; being a placeholder value.
; e.g. ((lhs / 2) 5) -> 2.5
(defun lhs (f y)
  (lambda (x) (f x y)))

; returns 'true if and only if either of the arguments is 'true
(defun or (a b)
  (if a a b))

; returns 'true if and only if both arguments are 'true
(defun and (a b)
  (if a b 'false))

; returns the logical inverse of the argument
(defun not (x)
  (if x 'false 'true))