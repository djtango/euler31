(ns euler31
  (:require [clojure.set :as set]))

;; In England the currency is made up of pound, £, and pence, p, and there are eight coins in general circulation:
;;
;; 1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).
;; It is possible to make £2 in the following way:
;;
;; 1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p
;; How many different ways can £2 be made using any number of coins?

(def denom->value
  "coins represented in terms of pence (the smallest possible denom)"
  {:1p 1
   :2p 2
   :5p 5
   :10p 10
   :20p 20
   :50p 50
   :1pound 100
   :2pound 200})

(def value->denom
  (set/map-invert denom->value))

(def denoms [200 100 50 20 10 5 2 1]) ;; sorted
(def denoms-2 [5 2 1]) ;; sorted

(def except-1 butlast)

;; (defn try-deduct []
;;   (if (>= x current-denom)
;;         (recur (- x current-denom)
;;                (update result d inc)
;;                denoms
;;                d)
;;         (if (> x 0)
;;           (recur x
;;                  (inc d)
;;                  denoms
;;                  result)
;;           result)))
;;

(defn solution [x result denoms]
  (->> denoms
       ;; (map (fn [denom] (remove #{denom} denoms))) ;; finding 'suboptimal'
       ;; except-1 ;; remove the one which lost 1
       ;; (concat [denoms])
       (map (fn [d]
              (if (>= x d)
                (solution (- x d)
                          (update result d inc)
                          denoms)
                (if (zero? x)
                  result
                  (if (neg? x)
                    (throw (ex-info "oops"))
                    (solution x
                              result
                              (remove #(>= % d) denoms))))))))
  )

(println
  (let [denoms [200 100 50 20 10 5 2 1]]
    (count (distinct (flatten (solution 200
                                  (zipmap denoms (repeat 0))
                                  denoms)))))
  )


(comment
  (let [denoms [5 2 1]] ;; smaller problem
    (count (distinct (flatten (solution 7
                                        (zipmap denoms (repeat 0))
                                        denoms)))))
  [{5 1, 2 1, 1 0}
   {5 1, 2 0, 1 2}
   {5 0, 2 3, 1 1}
   {5 0, 2 2, 1 3}
   {5 0, 2 1, 1 5}
   {5 0, 2 0, 1 7}]
  )
