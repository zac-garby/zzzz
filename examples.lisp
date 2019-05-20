# repeats a value infinitely
# e.g. (repeat 1) -> [1 1 1 1 1 1 ...]
(defun repeat (x) (cons x (repeat x)))

# replicates some value x, n times
# e.g. (repl 2 5) -> [2 2 2 2 2]
(defun repl (x n) (if (eq n 0) [] (cons x (repl x (- n 1)))))

# takes n values from the start of xs
# e.g. (take 2 [1 2 3 4]) -> [1 2]
(defun take (n xs) (if (eq n 0) [] (cons (head xs) (take (- n 1) (tail xs)))))
