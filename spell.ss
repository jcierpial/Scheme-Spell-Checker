; *********************************************
; *  314 Principles of Programming Languages  *
; *  Spring 2013                              *
; *  Student Version                          *
; *********************************************

;; contains "ctv", "A", and "reduce" definitions
(load "include.ss")

;; contains simple dictionary definition
(load "dictionary.ss")

;; -----------------------------------------------------
;; HELPER FUNCTIONS

(define key2
	(lambda (w)
		(cond ((null? w) 5381)
			(else
				(+ (ctv(car w)) (* (key2(cdr w)) 33)) 
			)
		)
	)	
)

(define hash-listword
	(lambda (hashfunctionlist word hashwordlist)
		(cond ((null? hashfunctionlist) hashwordlist)
			(else
				(hash-listword (cdr hashfunctionlist) word (cons ((car hashfunctionlist) word) hashwordlist))
			)
		)
	)
)

(define hash-listdict
	(lambda (hashfunctionlist dict hashdictlist)
		(cond ((null? dict) hashdictlist)
			(else
			(hash-listdict hashfunctionlist (cdr dict) (hash-listword hashfunctionlist (car dict) hashdictlist))
			)
		)
	)
)

(define intersection 
	(lambda (wordhashes dicthashes)
		(cond ((null? wordhashes) '())
			(
				(member (car wordhashes) dicthashes) 
				(cons (car wordhashes) (intersection (cdr wordhashes) dicthashes))
			)
			(else
				(intersection (cdr wordhashes) dicthashes)
			)
		)
	)
)

;; -----------------------------------------------------
;; KEY FUNCTION

(define key
	(lambda (w)
		(key2 (reverse w))
	)	
)

;; -----------------------------------------------------
;; HASH FUNCTION GENERATORS
;; value of parameter "size" should be a prime number

(define gen-hash-division-method
	(lambda (size)
		(lambda (k)
			(modulo (key k) size)
		)
    )
)

(define gen-hash-multiplication-method
	(lambda (size)
		(lambda (k)
			(floor (* size (- (* (key k) A) (floor (* (key k) A)))))
		)	
	)
)

;; -----------------------------------------------------
;; EXAMPLE HASH FUNCTIONS AND HASH FUNCTION LISTS

(define hash-1 (gen-hash-division-method 250000000))
(define hash-2 (gen-hash-division-method 100000000))
(define hash-3 (gen-hash-multiplication-method 240000000))
(define hash-4 (gen-hash-multiplication-method 110000000))

(define hashfl-1 (list hash-1 hash-2 hash-3 hash-4))
(define hashfl-2 (list hash-1 hash-3))

;; -----------------------------------------------------
;; EXAMPLE HASH VALUES
;;   to test your hash function implementation
;;
;;  (hash-1 '(h e l l o)) ==> 657
;;  (hash-1 '(d a y))     ==> 46
;;  (hash-1 '(c l a s s)) ==> 183
;;
;;  (hash-2 '(h e l l o)) ==> 273
;;  (hash-2 '(d a y))     ==> 218
;;  (hash-2 '(c l a s s)) ==> 254
;;
;;  (hash-3 '(h e l l o)) ==> 595.0
;;  (hash-3 '(d a y))     ==> 546.0
;;  (hash-3 '(c l a s s)) ==> 169.0
;;
;;  (hash-4 '(h e l l o)) ==> 765.0
;;  (hash-4 '(d a y))     ==> 702.0
;;  (hash-4 '(c l a s s)) ==> 217.0

;; -----------------------------------------------------
;; SPELL CHECKER GENERATOR

(define gen-checker
	(lambda (hashfunctionlist dict)
		(lambda (word)
			(let* ((hashwordlist '()) (hashdictlist '()))
				(let * ((wordhashes (hash-listword (reverse hashfunctionlist) word hashwordlist)) (dicthashes (hash-listdict (reverse hashfunctionlist) dict hashdictlist))) 
					(let * ((intersecthashes (intersection wordhashes dicthashes)))
						(= (length wordhashes) (length intersecthashes))					
					)
				)
			)
		)
	)
)

;; -----------------------------------------------------
;; EXAMPLE SPELL CHECKERS

(define checker-1 (gen-checker hashfl-1 dictionary))
(define checker-2 (gen-checker hashfl-2 dictionary))

;; EXAMPLE APPLICATIONS OF A SPELL CHECKER
;;
;;  (checker-1 '(a r g g g)) ==> #f
;;  (checker-2 '(h e l l o)) ==> #t