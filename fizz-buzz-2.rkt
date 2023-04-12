;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname fizz-buzz-2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; question a)

;; (make-div-pred m) consumes a number and makes a predicate function where
;; it produces true if the remainder of the two numbers is zero and false
;; otherwise

;; make-div-pred: Int -> Bool
(define (make-div-pred m)
  (local [;; (f n) consumes a number a produces true
          ;; if the remainder of the number divided by another number
          ;; m equals to zeroa dn false otherwise
          ;; f: Num -> Bool
          (define (f n)
            (cond [(= (remainder n m) 0) true]
                  [else false]))]
    f))

;; question b)

;; (fizz-buzz-2 start end lop) consumes a start and end number for a list
;; with a list of Any with a predicate function to produce the list of
;; numbers but it is replaced with the assigned value if it satisfies
;; a predicate in the list
;; Examples:

(check-expect (fizz-buzz-2 1 5 (list (list 'honk (make-div-pred 4))
                                     (list 'fizz (make-div-pred 2))
                                     (list 'buzz (make-div-pred 3))))
              '(1 fizz buzz honk 5))

(check-expect (fizz-buzz-2 0 4 (list (list "bee" zero?)
                                     (list 'neg negative?)
                                     (list 'even even?)
                                     (list 'odd odd?)))
              '("bee" odd even odd even))

(check-expect (fizz-buzz-2 -1 4 (list (list "bee" zero?)
                                     (list 'neg negative?)
                                     (list 'even even?)
                                     (list 'odd odd?)))
              '(neg "bee" odd even odd even))

;; fizz-buzz-2: Int Int (list Any (Int -> Bool) -> (listof Char)
(define (fizz-buzz-2 start end lop)
  (cond [(> start end) empty]
        [else (local [(define main-lop lop)
                      (define (number-list start loi)
                        (cond [(<= (first loi) start) loi]
                              [(> (first loi) start)
                               (number-list start (cons (sub1 (first loi)) loi))]))
                      (define (fizz-buzz-list lop loc)
                        (cond [(empty? lop) (cons (first loc)
                                                  (fizz-buzz-list main-lop (rest loc)))]
                              [(empty? loc) loc]
                              [((second (first lop)) (first loc))
                               (cons (first (first lop))
                                     (fizz-buzz-list main-lop (rest loc)))]
                              [else (fizz-buzz-list (rest lop) loc)]))]
                (fizz-buzz-list lop (number-list start (cons end empty))))]))

;; Tests:
(check-expect (fizz-buzz-2 6 0 (list (list 'honk (make-div-pred 4))
                                     (list 'fizz (make-div-pred 2))
                                     (list 'buzz (make-div-pred 3))))
              empty)