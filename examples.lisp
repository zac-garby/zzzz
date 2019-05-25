# repeats a value infinitely
# e.g. (repeat 1) -> [1 1 1 1 1 1 ...]
(defun repeat (x) (cons x (repeat x)))

# replicates some value x, n times
# e.g. (repl 2 5) -> [2 2 2 2 2]
(defun repl (x n) (if (eq n 0) [] (cons x (repl x (- n 1)))))

# takes n values from the start of xs
# e.g. (take 2 [1 2 3 4]) -> [1 2]
(defun take (n xs)
  (if (eq n 0)
    []
    (cons (head xs)
          (take (- n 1) (tail xs)))))

# applies f to every value in xs
# e.g. (map (+ 1) [1 2 3]) -> [2 3 4]
(defun map (f xs)
  (if (null xs)
    []
    (cons (f (head xs))
          (map f (tail xs)))))

# folds xs from the right, starting with the initial value i,
# using the function f.
# e.g. (foldr + 0 [1 2 3]) -> (+ 1 (+ 2 (+ 3 0))) -> 6
(defun foldr (f i xs)
  (if (null xs)
    i
    (f (head xs) (foldr f i (tail xs)))))

# filters xs with the predicate f
# e.g. (filter (eq 2) [1 2 3 4 5 4 3 2 1]) -> [2 2]
(defun filter (f xs)
  (if (null xs)
    []
    (let (hd (head xs))
      (if (f hd)
        (cons hd (filter f (tail xs)))
        (filter f (tail xs))))))

# computes the length of a list.
(defun len (xs)
  (if (null xs)
    0
    (+ 1 (len (tail xs)))))

# composes two unary functions together, such that the output
# from the second one is fed into the first one.
(defun . (f g x)
  (f (g x)))