;; for this HMM, the hidden states are just the categories Start, N, and V.
(def hidden-states '(Start N V))
;; the observations are the words Call, me, and Ishmael.
(def vocabulary '(Call me Ishmael))

;; we are given the transition and observation distributions
(def theta-transition-Start '(0 0.9 0.1))
(def theta-transition-N '(0 0.3 0.7))
(def theta-transition-V '(0 0.8 0.2))

(def theta-transition-dists-1
  (list
    theta-transition-Start
    theta-transition-N
    theta-transition-V))


; there is no theta-observation-Start

(def theta-observation-N '(0.1 0.5 0.4))
(def theta-observation-V '(0.8 0.1 0.1))

(def theta-observation-dists-1
  (list
    theta-observation-N
    theta-observation-V))


(defn dist-lookup
  "gets the distribution over states for a given state
   from the list of dists"
  [state states dists]
  (if (= state (first states))
    (first dists)
    (dist-lookup state (rest states) (rest dists))))

(defn log2 [n] (/ (Math/log n) (Math/log 2)))

(defn logsumexp
  "given a list of log-vals: exponentiates, sums, and re-logs them"
  [log-vals]
  (let [mx (apply max log-vals)]
    (+ mx
      (log2
        (apply +
          (map (fn [z] (Math/pow 2 z))
            (map (fn [x] (- x mx)) log-vals)))))))

(defn logscore-categorical
  "log probability of word 'outcome' given the vocabulary 'outcomes'
   and the list of categorical outcome probabilities 'params'"
  [outcome outcomes params]
  (if (= outcome (first outcomes))
    (log2 (first params))
    (logscore-categorical outcome (rest outcomes) (rest params))))

;; Problem 1  - score-next-state-word

; (defn score-next-state-word
;   "compute log probability of going from 'curr-state' to
;    'next-state' and emmitting 'next-word'"
;   [curr-state next-state next-word t-dists o-dists]
;   ... )

(defn score-next-state-word
  [curr-state next-state next-word t-dists o-dists]
  (let [transition-dist (dist-lookup curr-state hidden-states t-dists)
        transition-score (logscore-categorical next-state hidden-states transition-dist)
        observation-dist (dist-lookup next-state '(N V) o-dists)
        emission-score (logscore-categorical next-word vocabulary observation-dist)] (+ transition-score emission-score)))

;; Problem 2  - compute-next-observation-marginal

; (defn compute-next-observation-marginal
;   "get log marginal probability of next word being 
;    'next-observation' given 'curr-state'"
;   [curr-state next-observation t-dists o-dists]
;   ... )

(defn compute-next-observation-marginal
  [curr-state next-observation t-dists o-dists]
  (logsumexp
    (map (fn [next-state] (score-next-state-word curr-state next-state next-observation t-dists o-dists))
        '(N V))))

;; Problem 3  - score-next-states-words

; (defn score-next-states-words
;   "compute log prob of going from 'curr-state'
;     to sequence of states 'next-states'
;     and emmitting sequence of words 'next-words'"
;   [curr-state next-states next-words t-dists o-dists]
;   ... )

(defn score-next-states-words
  [curr-state next-states next-words t-dists o-dists]
  (if (empty? next-states)
    0
    (+ (score-next-state-word curr-state (first next-states) (first next-words) t-dists o-dists)
       (score-next-states-words (first next-states) (rest next-states) (rest next-words) t-dists o-dists))))

;; Problem 4  - compute-next-words-marginal

; (defn compute-next-words-marginal
;   "get the log marginal prob of sequence 'next-words' given 'curr-state'"
;   [curr-state next-words t-dists o-dists]
;   ... )

(defn compute-next-words-marginal
  [curr-state next-words t-dists o-dists]
  (if (empty? next-words)
    0
    (logsumexp (map (fn [next-state]
             (+ (score-next-state-word curr-state next-state (first next-words) t-dists o-dists)
                (compute-next-words-marginal next-state (rest next-words) t-dists o-dists)))
              '(N V)))))


;; Problem 6  - compute-hidden-prior

; (defn compute-hidden-prior
;   "get log prior probability 'list-of-states' given transition 
;    distributions 't-dists'"
;   [list-of-states t-dists]
;   ... )

(defn compute-hidden-prior [list-of-states t-dists]
  (letfn [(helper [prev-state states]
          (if (empty? states)
            0
            (let [t-dist (dist-lookup prev-state hidden-states t-dists)
                  log-t (logscore-categorical (first states) hidden-states t-dist)]
              (+ log-t (helper (first states) (rest states))))))]
              (helper 'Start list-of-states)))

;; Problem 7  - compute-likelihood-of-words

; (defn compute-likelihood-of-words
;   "Likelihood (of hidden states) = conditional probability
;   of the words in the list-of-words given the list-of-states"
;   [list-of-states list-of-words o-dists]
;   ... )

(defn compute-likelihood-of-words
  [list-of-states list-of-words o-dists]
  (if (empty? list-of-states)
    0
    (let [state (first list-of-states)
          word (first list-of-words)
          dist (dist-lookup state '(N V) o-dists)
          logp (logscore-categorical word vocabulary dist)]
      (+ logp (compute-likelihood-of-words (rest list-of-states)(rest list-of-words) o-dists)))))

;; Problem 8  - compute-hidden-posterior

; (defn compute-hidden-posterior
;   "computes log posterior probability of the 'list-of-states' 
;    given the observed 'list-of-words'"
;   [list-of-states list-of-words t-dists o-dists]
;   ... )

(defn compute-hidden-posterior
  [list-of-states list-of-words t-dists o-dists]
  (let [prior (compute-hidden-prior list-of-states t-dists)
        likelihood (compute-likelihood-of-words list-of-states list-of-words o-dists)
        evidence (compute-next-words-marginal 'Start list-of-words t-dists o-dists)]
    (- (+ prior likelihood) evidence)))

;; Problem 10 - compute-next-words-marginal-mem

; (def compute-next-words-marginal-mem
;   "(like compute-next-words-marginal, but with memoization)
;   gets the log marginal prob of sequence 'next-words' given 'curr-state'"
;   ... )

(def compute-next-words-marginal-mem
(memoize compute-next-words-marginal))
