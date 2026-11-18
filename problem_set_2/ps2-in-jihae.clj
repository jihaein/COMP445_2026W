;; Problem 1: define a function `absval`
(defn absval [x] (Math/sqrt (* x x)))

;; Problem 2: fix the functions `take-square` and `sum-of-squares`

(defn take-square [x] (* x x)) 
;; it is wrong because it forgot to put the [x]

(defn sum-of-squares [x y] (+ (take-square x) (take-square y)))


;; Problem 3: define expressions `exp-13-1`, `exp-13-2`, `exp-13-3`, and `exp-13-4`
(def exp-13-1 '(+ (* 2 5) 3)) 
(def exp-13-2 '(Math/sqrt (* 13 13)))
(def exp-13-3 '(/ (* 13 2) 2)) 
(def exp-13-4 '(+ 10 3))   


;; Problem 4: define a function `third`
(defn third [x] (first (rest (rest x))))


;; Problem 5:
;; [let's import and define `sqrt` and `abs` from Java.lang.Math for the example]
(defn sqrt [x] (Math/sqrt x))
(defn abs [x] (Math/abs x))


;; define a function `compose`
(defn compose [f g] (fn [x] (f (g x))))


;; Problem 6: define a function `first-two`
(defn first-two [x] (list (first x) (first (rest x))))

;; Problem 7: define a function `remove-second`
(defn remove-second [x] (cons (first x) (rest (rest x))))

;; Problem 8: define a function `add-to-end`
(defn add-to-end [lst x] (concat lst (list x)))


;; Problem 9: define a function `reverse-list`
;;            (Note that this function will overwrite a built in function.
;;             This is okay.)

(defn reverse-list [x] (if (empty? x) '() (concat (reverse-list (rest x)) (list (first x)))))


;; Problem 10: define a function `count-to-1`
(defn count-to-1 [n] (if (= n 1) (list 1) (concat (list n) (count-to-1 (- n 1)))))

;; Problem 11: define a function `count-to-n`
(defn count-to-n [n] (reverse-list (count-to-1 n)))

;; Problem 12: define a function `get-max`
(defn getmax [x]   
(if (empty? (rest x)) (first x) 
(if (< (first x) (first (rest x))) (getmax (rest x))
(getmax (cons (first x) (rest (rest x)))))))


;; Problem 13: define a function `greater-than-five?`
(defn greater-than-five? [x] (map (fn [x] (> x 5)) x))


;; Problem 14: define a function `concat-three`
(defn concat-two [x y]
(if (empty? x) y
(cons (first x) (my-concat (rest x) y))))

(defn concat-three [x y z]
(concat-two x (concat-two y z)))

;; Problem 15: define a function `sequence-to-power`
(defn sequence-to-power [x n]
(if (<= n 0) '()
(concat x (sequence-to-power x (- n 1)))))

;; Problem 16: define a function `in-L-star?`
(defn in-L-star? [xs]
(if (empty? xs) true
(if (= (first xs) 'a) (in-L-star? (rest xs))
false)))