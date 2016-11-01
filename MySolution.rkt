#lang racket
;; 1)Find the last box of a list.
(define (last li)
  (if (null? (cdr li))
      (car li)
      (last (cdr li)))
  )

;; 2) Find the last but one box of a list and return a couple of the elements as a list
(define (last-but-one li)
    (if(= (length li) 2)
       li
       (last-but-one (cdr li))))

;; 3) Find the k-th element of a list
(define (k-list li num)
  (define (tail-k-list li i num)
    (if(= i num)
       (car li)
       (tail-k-list (cdr li) (+ i 1) num))
    )
  (tail-k-list li 1 num))

;; 4) Find the number of elements of a list
(define (my-length li)
  (if (null? (cdr li))
      1
      (+ 1 (my-length (cdr li)))))

;; 5) Reverse a list
(define (my-reverse li)
  (if (null? li)
      '()
      (cons (my-reverse (cdr li)) (car li) ))
  )
;; 5) Tail recursive versione of Reverse a List
(define (tail-my-reverse li)
  (define (tail-reverse li res)
    (if (null? li)
        res ;; Remember! With tail-recursive things you return the ''accumulator'', with normal-recursive things you return the base case
        (tail-reverse (cdr li) (append (list (car li)) res))))
  (tail-reverse li '()))
