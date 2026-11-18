;; Problem 1
;; Define a variable `year`
(def year 2025)


;; Problem 5
;; Define a function `add-up`
(defn add-up [x y] (+ x y))


;; Problem 6
;; Define a function `is-it-four?` 
;;   (Note: Don't forget the question mark in the name!)
(defn is-it-four? [x] (= 4 x))

;; Problem 7
; Add your code in the indicated space below
; (so that the symbol `problem-7` evaluates to `true`)

(def problem-7 ;<-- do not edit this
		(= (quote    ;<-- do not edit this
		
		; ~~ your code here! ~~
		platypus

		) 'platypus) ;<-- do not edit this
)              ;<-- do not edit this


;; Problem 8
;; Define a function `func` and an expression `expr` 
(def func +)
(def expr '(1 2))

;; Problem 9
;; Define a function `both-same-type?`
;;   (Note: Don't forget the question mark in the name!)
(defn both-same-type? [x y] (= (type x) (type y)))

;; Problem 10
;; Define a function `list-longer-than?`
;;   (Note: Don't forget the question mark in the name!)
(defn list-longer-than? [n lst] (> (count lst) n))

;; Problem 11
;; Define a function `dot-product`
(defn dot-product [x y] (apply + (map * x y)))

;; Problem 12
;; Define a function `swap-arg-order`
(defn swap-arg-order [func] (fn [x y] (func y x)))


;; Problem 13
;; Define a higher order function `g`
(defn g [func] (func 10))