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


;; TODO - this doesn't work in reverse
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

;; why reverse? because pulling from the head of the "input"
;; and cons-ing to the accumulator reverses the list

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

;; p09 - Pack consecutive duplicates of list elements into sublists.

;; this runs out of memory when trying to turn the sublists into the original
;; how do I write a reversible 

(define %group-sublists-reverse
  (%rel (hd-acc tl-acc1 tl-acc acc hd tl grouprev)
        [((cons hd-acc tl) (cons (cons hd-acc tl-acc1 ) tl-acc) grouprev)
         (%group-sublists-reverse tl (cons (cons hd-acc (cons hd-acc tl-acc1)) tl-acc) grouprev)]
        [((cons hd tl) acc grouprev)
         (%group-sublists-reverse tl (cons (cons hd '()) acc) grouprev)]
        [('() grouprev grouprev)]))

;; helper function

(define %all-match
  (%rel (tl e)
        ;;[('() _)]
        [((cons e '()) e)]
        [((cons e tl) e) (%all-match tl e)]))

(define %concat
  (%rel (xs ys x0 x1 ytail)
        [('() '())]
        [(xs ys) (%append x0 x1 xs) (%= (cons x0 ytail) ys) (%concat x1 ys)]))

(define %group-sublists-better
  (%rel (xs ys tail0 tail matching e rest-ys p q)
        [('() '())]
        [(xs ys) (%all-match xs e) (%= xs ys)]
        [(xs ys)
         (%= ys (cons matching rest-ys))
         (%append matching (cons tail0 tail) xs)
         (%all-match matching e)
         (%= rest-ys (cons (cons tail0 p) q))
         (%/= e tail0) ; fails here, %/= succeeds only if they CANNOT be unified
         (%group-sublists-better (cons tail0 tail) rest-ys)]))

        

(define %group-sublists
  (%rel (xs group grouprev)
        [(xs group) (%group-sublists-reverse xs '() grouprev) (%reverse group grouprev)]))


;; p10 - Run-length encoding of a list (use p09)

(define %repeatr
  (%rel (e tl n n0)
        [('() _ 0)]
        [((cons e tl) e n) (%is n0 (- n 1)) (%repeatr tl e n0)]
        ))

(define %repeata
  (%rel (e n n0 acc rep)
        [(e acc 0 acc)] ;; for some reason, including "e" is neccessary, I'm not sure why
        [(e acc n rep) (%is n0 (- n 1)) (%repeata e (cons e acc) n0 rep)]))

;; This works to some extend, but cannot find results for
;; (%which (n) (%list-repeat 2 n '(2 2 2)))
;; which is the exact thing I need :(
(define %list-repeat
  (%rel (e n rep)
        [(e n rep) (%repeata e '() n rep)]))

;; ---------------

(define %rep-list
  (%rel (e n n0 tl)
        [(e 0 '())] ;; again, the "e" seems important ??
        ;[(e n (cons e tl)) (%is n0 (- n 1)) (%rep-list e n0 tl)]))
        [(e n (cons e tl)) (%is n (+ n0 1)) (%rep-list e n0 tl)]))

;; ---------------

;(define %run-length-encode
;  (%rel ()
;        [(xs run-length-xs)
;         (%group-sublists xs grouped)



(define %repeat-element
  (%rel (length element lst)
        [(length element lst) (%length length lst) (%all-match lst element)]))


(define %run-length-grouped
  (%rel (hd0 tl0 n1 e1 tl1)
        [('() '())]
        [((cons hd0 tl0) (cons (cons n1 e1) tl1)) (%repeat-element n1 e1 hd0) (%run-length-grouped tl0 tl1)]))

(define %run-length-encode
  (%rel (xs grouped run-length-xs)
        [(xs run-length-xs)
         (%group-sublists xs grouped) (%run-length-grouped grouped run-length-xs)]))


;(define %maplist
;  (%rel (hd0 tl0;hd1 tl1 rel)
;        [('() '() rel)]
;        [((cons hd0 tl0) (cons hd1 tl1) rel) (rel hd0 hd1) (%maplist tl0 tl1 rel)]))
         