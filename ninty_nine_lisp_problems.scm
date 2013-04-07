; Ninty-Nine Lisp Problems

; Testings vars
(define x '(a b c a))

; P01: Find the last box of a list.
(define (find_last a)
  (car(reverse a))
  )
(find_last '(a b c d))

; P02: Find the last but one box of a list. -- check
(define (find_but_last a)
  (list-tail a 2)
  )
(find_but_last '(a b c d))

; P03: Find the K'th element of a list.
(define (find_kth_ele a k)
  (list-ref a k)
  )
(find_kth_ele '(a b c d) 3)

; P04: Find the number of elements of a list.
(define (find_num_ele a)
  (length a)
  )
(find_num_ele '(a b c d e))

; P05: Reverse a list.
(define (reverse_list a)
  (reverse a)
  )
(reverse_list '(a b c d))

; P06: Find if palindrome
(define palindromehelper
  (lambda (a si li) 
    (if (<= si li)
        (if (not(equal? (list-ref a si) (list-ref a li))) 
            #f
            (palindromehelper a (+ si 1) (- li 1))
            )
        #t
        )
    )
  )
(define (palindrome x)
    (palindromehelper x 0 (- (length x) 1))
  )
(palindrome '(a b c d))
(palindrome '(a b b a))

;P08 -- Pull out dublicates of the list
(define compress_helper
  (lambda (n res ind)
    (if (>= ind (length n))
        (reverse res)
        (if (member (list-ref n ind) res)
            (compress_helper n res (+ ind 1))
            (compress_helper n (cons (list-ref n ind) res) (+ ind 1))
            )
        )
    )
  )
(define (compress x)
    (compress_helper x '() 0)
  )
(compress '(a a a a b c c a a d e e e e))

; P14 -- Duplicate the elements of a list.
(define dupli_helper
  (lambda (n res ind)
    (if (>= ind (length n))
        res
        (dupli_helper n (append res (list (list-ref n ind) (list-ref n ind))) (+ ind 1))
        )
    )
  )
(define (dupli x)
    (dupli_helper x '() 0)
  )
(dupli '(a b c c d))

; P15 -- Replicate the elements of a list a given number of times.
(define its 
  (lambda (x i res)
    (if (> i 0)
        (its x (- i 1) (cons x res))
        res
        )
    )
  )
(define repli_helper
  (lambda (n l res ind)
    (if (>= ind (length n))
        res
        (repli_helper n l (append res (its (list-ref n ind) l '())) (+ ind 1))
        )
    )
  )
(define (repli x r)
    (repli_helper x r '() 0)
  )
(repli '(a b c) 4)

; P16 -- Drop every N'th element from a list.
(define drop_helper
  (lambda (n r res ind)
    (if (>= ind (length n))
        (reverse res)
        (if ( = (modulo (+ ind 1) r) 0)
            (drop_helper n r res (+ ind 1))
            (drop_helper n r (cons (list-ref n ind) res) (+ ind 1))
            )
        )
    )
  )
(define (drop x r)
    (drop_helper x r '() 0)
  )
(drop '(a b c d e f g h i k) 3)

; P17 -- Split a list into two parts; the length of the first part is given.
(define split_helper
  (lambda (n r x1 x2 res ind)
    (if (>= ind (length n))
        (list (reverse x1) (reverse x2))
        (if (< ind r)
            (split_helper n r (cons (list-ref n ind) x1) x2 res (+ ind 1))
            (split_helper n r x1 (cons (list-ref n ind) x2) res (+ ind 1))
            )
        )
    )
  )
(define (split x r)
    (split_helper x r '() '() '() 0)
  )
(split '(a b c d e f g h i k) 3)

; P18 -- Extract a slice from a list.
(define slice-helper
  (lambda (n r1 r2 res ind)
    (if (>= ind (length n))
        (reverse res)
        (if (>= ind (- r1 1))
            (if (< ind r2)
             (slice-helper n r1 r2 (cons (list-ref n ind) res) (+ ind 1))
             (slice-helper n r1 r2 res (+ ind 1))
             )
            (slice-helper n r1 r2 res (+ ind 1))
        )
        )
    )
  )
(define (slice x r1 r2)
    (slice-helper x r1 r2 '() 0)
  )
(slice '(a b c d e f g h i k) 3 7)

; P19 -- Rotate a list N places to the left.
(define rotate-helper
  (lambda (n r res ind)
    (if (>= r 0)
      (append (list-ref (split n (- (length n) r)) 1) (list-ref (split n (- (length n) r)) 0))
      (append (list-ref (split n (+ (length n) r)) 1) (list-ref (split n (+ (length n) r)) 0)) 
      )
    )
  )
(define (rotate x r)
    (rotate-helper x r '() 0)
  )
(rotate '(a b c d e f g h) 5)
(rotate '(a b c d e f g h) -2)

; P20 -- Remove the K'th element from a list.
(define remove-at-helper
  (lambda (n r res ind)
  (if (>= ind (length n))
      (reverse res)
      (if (= (+ ind 1) r)
          (remove-at-helper n r res (+ ind 1))
          (remove-at-helper n r (cons (list-ref n ind) res) (+ ind 1))
          )
      )
    )
  )
(define (remove-at x r)
    (remove-at-helper x r '() 0)
  )
(remove-at '(a b c d) 2)

;P21 -- Insert an element at a given position into a list. -- MOD
(define insert-at-helper
  (lambda (w n r res ind)
    (if (>= ind (length n))
      (reverse res)
      (append (reverse (cons w (list-ref (split n (- r 1)) 0))) (list-ref (split n (- r 1)) 1))
      )
    )
  )
(define (insert-at w n r)
  (insert-at-helper w n r '() 0)
  )
(insert-at 'alfa '(a b c d) 2)

;P22 -- Create a list containing all integers within a given range.\
(define range-helper 
  (lambda (x1 x2 res)
    (if (= x1 x2)
        (reverse (cons x1 res))
        (if (<= x1 x2)
            (range-helper (+ x1 1) x2 (cons x1 res))
            (range-helper (- x1 1) x2 (cons x1 res))
            )
        )
    )
  )
(define (range x1 x2)
  (range-helper x1 x2 '())
  )
(range 9 3)
(range 3 9)

(#%require (only racket/base random))

; P23 -- Extract a given number of randomly selected elements from a list.
(define rnd-select-helper
  (lambda (n r res)
    (if (<= r 0)
        res
        (rnd-select-helper n (- r 1) (cons (list-ref n (random (length n))) res))
        )
    )
  )
(define (rnd-select n r)
  (rnd-select-helper n r '())
  )
(rnd-select '(a b c d e f g h) 4)

;P24 -- Lotto: Draw N different random numbers from the set 1..M.
(define lotto-select-helper
  (lambda (x1 x2 res)
    (if (<= x1 0)
        res
        (lotto-select-helper (- x1 1) x2 (cons (random x2) res))
        )
    )
  )
(define (lotto-select x1 x2)
  (lotto-select-helper x1 x2 '())
  )
(lotto-select 6 49)

;P25 -- Generate a random permutation of the elements of a list.
(define rnd-permu-helper
  (lambda (n res)
    (rnd-select n (length n))
    )
  )
(define (rnd-permu n)
  (rnd-permu-helper n '())
  )
(rnd-permu '(a b c d e f))

;P31 -- Determine whether a given integer number is prime.
(define is-prime-helper
  (lambda (x t)
    (if (> t (expt x x))
        #t
        (if (= x t)
            (is-prime-helper x (+ t 1))
            (if (= (modulo x t) 0)
                #f
                (is-prime-helper x (+ t 1))
                )
        )
    )
    )
  )
(define (is-prime x)
  (is-prime-helper x 2)
  )
(is-prime 7)
(is-prime 8)

;P32 -- Determine the greatest common divisor of two positive integer numbers.
(define gcd-helper
  (lambda (x1 x2 res)
    (if (= x1 x2)
        x2
        (if (< x1 x2)
            (gcd-helper x1 (- x2 x1) (+(- res res)x1))
            (gcd-helper x2 (- x1 x2) (+(- res res)x2))
            )
        )
    )
  )
(define (gcd-func x1 x2)
  (gcd-helper x1 x2 0)
  )
(gcd-func 93 9)

;P54A -- Check whether a given term represents a binary tree
(define istree 
  (lambda (n ind res)
    (if (= (length n) 3)
        (if (equal? (car n) '(nil))
            #f
            (if (equal? (cdr n) '(nil nil))
                #t
                (istree (list-ref n (+ ind 1)) 0 #t)
                )
            )
        #f
        )
    )
  )
(istree '(a (b nil nil) nil) 0 #t)
(istree '(a (b nil nil)) 0 #t)
