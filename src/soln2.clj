(ns soln2)

;; Overall strategy: find all permutations that satisfy this equation
;; a1 b2 c5 d10 e20 f50 g100 h200 = 200
;; credits to Miguel Roca for inspiration

;; Can reduce initial permutations by realizing that independently each coefficient can be bounded:
;; a <- (into-2-pounds 200)
;; b <- (into-2-pounds 100)
;; c <- (into-2-pounds 50)
;; d <- (into-2-pounds 20)
;; e <- (into-2-pounds 10)
;; f <- (into-2-pounds 5)
;; g <- (into-2-pounds 2)
;; h <- (into-2-pounds 1)

;; Can further optimize by 'sieving' out permutations which are impossible after on current branch of permutations e.g. once selecting h = 1, a...g must be 0 and h = 0, g = 1, f >= 2

(defn bounds [denom target]
  (->> (/ target denom)
       Math/floor
       int
       inc
       range
       (into [])))

(def into-2-pounds #(bounds % 200))

;; (comment
;; (println
;;   (let [denoms [5 2 1]
;;         target 7
;;         denom->bounds (zipmap denoms (mapv #(bounds % target) denoms))
;;         branch {}
;;         path []
;;         result {:branch branch
;;                 :path path}] ;; smaller problem
;;   (let [current-denom (apply max denoms)
;;         coeffs (denom->bounds current-denom)] ;; for each value of biggest denom
;;     (assoc-in result
;;               current-denom
;;               (reduce (fn [branch c]
;;                         (let [denoms (vec (rest denoms))
;;                               target (- target (* c current-denom))
;;                               denom->bounds (->> denoms
;;                                                  (mapv (fn [d]
;;                                                          [d (bounds d target)]))
;;                                                  (into {}))]
;;                           (let [current-denom (apply max denoms)
;;                                 coeffs (denom->bounds current-denom)]
;;                             (reduce (fn [branch c]
;;                                       (let [denoms (vec (rest denoms))
;;                                             target (- target (* c current-denom))
;;                                             denom->bounds (->> denoms
;;                                                                (mapv (fn [d]
;;                                                                        [d (bounds d target)]))
;;                                                                (into {}))]
;;                                         (let [current-denom (apply max denoms)
;;                                               coeffs (denom->bounds current-denom)]
;;                                           (reduce (fn [branch c]
;;                                                     (let [denoms (vec (rest denoms))
;;                                                           target (- target (* c current-denom))
;;                                                           denom->bounds (->> denoms
;;                                                                              (mapv (fn [d]
;;                                                                                      [d (bounds d target)]))
;;                                                                              (into {}))]
;;                                                       (conj branch c)))
;;                                                   (conj branch c)
;;                                                   coeffs))))
;;                                     (conj branch c)
;;                                     coeffs))))
;;                       path
;;                       coeffs))))
;;   )
;; ;; )
;;
;; (let [denoms [5 2 1]
;;       target 7
;;       denom->bounds (zipmap denoms (mapv #(bounds % target) denoms))
;;       branch []] ;; smaller problem
;;   (let [current-denom (apply max denoms)
;;         coeffs (denom->bounds current-denom)] ;; for each value of biggest denom
;;     (reduce (fn [branch c]
;;               (let [denoms2 (vec (rest denoms))
;;                     target (- target (* c current-denom))
;;                     denom->bounds (->> denoms2
;;                                        (mapv (fn [d]
;;                                                [d (bounds d target)]))
;;                                        (into {}))]
;;                 (let [current-denom (apply max denoms)
;;                       coeffs (denom->bounds current-denom)])))
;;             branch
;;             coeffs)))

(defn x [denoms target denom->bounds result]
  (let [current-denom (apply max denoms)
        coeffs (denom->bounds current-denom)] ;; for each value of biggest denom
    (reduce (fn [result c]
              (if (seq (rest denoms))
                (let [denoms (vec (rest denoms))
                      target (- target (* c current-denom))
                      denom->bounds (->> denoms
                                         (mapv (fn [d]
                                                 [d (bounds d target)]))
                                         (into {}))]
                  (let [current-denom (apply max denoms)
                        coeffs (denom->bounds current-denom)]
                    (conj result c (x denoms target denom->bounds []))))
                [c]))
            result
            coeffs)))
;;
;; (defn flatten-result [denoms result]
;;   (loop [[d & denoms] denoms]
;;     (if d
;;       ())
;;     (recur)))
;;




;; (defn tree-reduce [leaf-fn leaf? t]
;;   (loop [path []
;;          t t
;;          result []]
;;     (if (leaf? t)
;;       (conj result (leaf-fn t))
;;       (let [[value left right] t]
;;         (recur (conj path value)
;;                right
;;                (recur (conj path value)
;;                       result))))))
;;
(defn tree-reduce
  "depth-first"
  [left right value leaf-fn rf t]
  (letfn [(go-deeper [path t result]
            (let [[l r v] ((juxt left right value) t)
                  leaf? (and (nil? l) (nil? r))]
              (if leaf?
                (rf result (leaf-fn path v))
                (go-deeper path
                           r
                           (go-deeper (conj path v)
                                      l
                                      result)))))]
    (go-deeper [] t [])))


(defn solution [denoms target]
  (let [denom->bounds (zipmap denoms (mapv #(bounds % target) denoms))
        result []
        left second
        right (comp next next)
        value first]
    ;; example tree:
    ;; [0 [0 [7]
    ;;     1 [5]
    ;;     2 [3]
    ;;     3 [1]]
    ;;  1 [0 [2]
    ;;     1 [0]]]

    (->> (x denoms target denom->bounds result)
         (tree-reduce
           left
           right
           value
           (fn [path node]
             (let [final-path (conj path node)]
               (if (= (count final-path)
                      (count denoms))
                 (zipmap denoms final-path)
                 nil)))
           conj)
         (remove nil?))))

(time (println (count (solution [200 100 50 20 10 5 2 1] 200))))
