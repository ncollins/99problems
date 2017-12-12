#lang racket

(require racklog)

;; p01 - find the last element of a list

(define %lastElement
  (%rel (last xs)
        [(last (cons last '()))]
        [(last (cons (_) xs)) (%lastElement last xs)]))

(define %member
  (%rel (x y xs)
    [(x (cons x xs))]
    [(x (cons y xs))
      (%member x xs)]))

;; p02 - find the last but one element of a list

(define %lastButOne
  (%rel (penultimate xs)
        [(penultimate '()) %fail]
        [(penultimate (cons (_) '())) %fail]
        [(penultimate (cons penultimate (cons (_) '())))]
        [(penultimate (cons (_) xs)) (%lastButOne penultimate xs)]))


;; p03 - find the k-th element of a list

(define %kth-element
  (%rel (xs e k kminus1)
        [(0 e (cons e (_)))]
        [(k e (cons (_) xs)) (%is kminus1 (- k 1))
                             (%kth-element kminus1 e xs)]))

;; p04 - find the number of elements of a list

;; this can check, but not calculate the length
(define %length-check
  (%rel (len len0 tail)
        [(0 '())]
        [(len (cons (_) tail)) (%is len0 (- len 1)) (%length-check len0 tail)]))

;; use an accumulator and a second function to wrap this
(define %length-inner
  (%rel (len prev prev1 tail)
        [(prev prev '())]
        [(len prev (cons (_) tail)) (%is prev1 (+ prev 1)) (%length-inner len prev1 tail)]))

(define %length
  (%rel (len xs)
        [(len xs) (%length-inner len 0 xs)]))

;; p05 - reverse a list

(define %reverse-inner
  (%rel (head tail acc newacc rev)
        [('() acc acc)]
        [((cons head tail) acc rev) (%= newacc (cons head acc)) (%reverse-inner tail newacc rev)]))

(define %reverse
  (%rel (xs rxs)
        [(xs rxs) (%reverse-inner xs '() rxs)]))


;; p06 - find out if a list is a palindrome
(define %palindrome
  (%rel (xs)
        [(xs) (%reverse xs xs)]))

;; this version works when looking at the middle element
(define %palindrome2
  (%rel (xs a b)
        [(xs) (%append a b xs) (%reverse a b)]
        [(xs) (%append a (cons (_) b) xs) (%reverse a b)]))

;; p07 - flatten a list (can use `append` and `atom?`)

(define %is-list
  (%rel (hd tl)
        [('())]
        [((cons hd tl)) (%is-list tl)]))

(define %flatten
  (%rel (hd tl hd0 tl0 flat flathead flattail)
        [('() '())]
        [((cons hd tl) flat) (%is-list hd)
                             (%flatten hd flathead)
                             (%flatten tl flattail)
                             (%append flathead flattail flat)]
        [((cons hd tl) flat) (%flatten tl flattail)
                             (%= flathead (list hd))
                             (%append flathead flattail flat)]))


;; p08 - Eliminate consecutive duplicates of list elements

(define %compress-reverse
  (%rel (hd tl hd-acc tl-acc comprev)
        [((cons hd-acc tl) (cons hd-acc tl-acc) comprev)
         (%compress-reverse tl (cons hd-acc tl-acc) comprev)]
        [((cons hd tl) (cons hd-acc tl-acc) comprev)
         (%compress-reverse tl (cons hd (cons hd-acc tl-acc)) comprev)]
        [((cons hd tl) '() comprev)
         (%compress-reverse tl (cons hd '()) comprev)]
        [('() comprev comprev)]))

(define %compress
  (%rel (xs comp comprev)
        [(xs comp) (%compress-reverse xs '() comprev) (%reverse comp comprev)]))
