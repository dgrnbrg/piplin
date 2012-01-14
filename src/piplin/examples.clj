(ns piplin.examples)

(comment
  integral types are of the form (XintT N) where
    X is "u" or "s", meaning unsigned or signed
    T is "m", "e", or "s", meaning modular arithmatic, exceptional arithmatic (bind to exception node), and saturating arithmetic
    N is the number of bits
  )

;default values for structural?
;how to extract results from structural?
; - always valid output, or gated output
;what are the guards for the structural?

(defn johnson []
  "johnson counter"
  (:q (structural [q (make-type (uintm 4) 0)]
              (let [new-q (cat (slice q 1 4) (not (slice q 0 1)))]
                (connect q new-q)))))

(defn flip [n]
  {:pre [(bits? n)]}
  (loop [idx (dec (bit-count n))
         result []]
    (if (>= idx 0)
      (conj (bit n idx) result)
      (reduce cat nil result))))

(defn johnson-with-dir [dir]
  "johnson counter for either direction"
  (flip (johnson)))

(defn cordic
  "computes [sin(z) cos(z)]"
  {:pre [(int? z)]}

  [z]
  (let [w (bit-count z)
        m (pow 2 (- w 2))
        n (dec w)
        an (reduce 1.0
                   (fn [an i]
                         (* an (sqrt (+ 1 (pow 2 (* -2 i))))))
                   (range n))
        x0 (int (round (* m (/ 1 an))))
        angles (for [x (range n)] (int
                                    (round
                                      (* m (atan
                                             (pow 2 (- i)))))))
        logic (structural [x (make-type (typeof z) 0)
                           y (make-type (typeof z) 0)
                           z (make-type (typeof z) 0)
                           i (make-type (uintm i) 0)
                           state (make-type
                                   (enum :waiting :calculating)
                                   :waiting)]
                          (let [inc-fn (fn [a da comp]
                                         (if (apply comp z 0)
                                           (- a da)
                                           (+ a da)))
                                init-fn (fn [a a0 da comp]
                                          (if (= :waiting state)
                                            a0
                                            (inc-fn a da comp)))]
                            (connect x (init-fn x x0 dx >=))
                            (connect y (init-fn y y0 dy <))
                            (connect z (init-fn z z0 dz >=)))
                          (connect i (if (= :waiting state)
                                       0
                                       (inc i))))]
    (if (= (dec n) (:i logic))
      [(:x logic) (:y logic)]
      nil))
