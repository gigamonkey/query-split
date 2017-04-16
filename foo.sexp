;; original
(or
 (or
  (or
   (or (and v1 (and v3 v4)) (or (and v2 (and v1 (and v3 v4))) (and (and v1 v2) (and v3 v4))))
   (or
    (or (and v3 (and v1 v4)) (and (and v1 v3) v4))
    (or (and (and v2 v3) (and v1 v4)) (and (and (and v1 v2) v3) v4))))
  (or
   (or
    (or (and v4 (and v1 v3)) (and (and v1 v4) v3))
    (or (and (and v2 v4) (and v1 v3)) (and (and (and v1 v2) v4) v3)))
   (or
    (or (and (and v3 v4) v1) (and v1 (and v3 v4)))
    (or (and (and v2 (and v3 v4)) v1) (and (and v1 v2) (and v3 v4))))))
 (or
  (or
   (or (and v2 (and v1 (and v3 v4))) (and (and v1 v2) (and v3 v4)))
   (or
    (or (and (and v3 v2) (and v1 v4)) (and (and (and v1 v3) v2) v4))
    (or (and (and v2 v3) (and v1 v4)) (and (and (and v1 v2) v3) v4))))
  (or
   (or
    (or (and (and v4 v2) (and v1 v3)) (and (and (and v1 v4) v2) v3))
    (or (and (and v2 v4) (and v1 v3)) (and (and (and v1 v2) v4) v3)))
   (or
    (or (and (and (and v3 v4) v2) v1) (and (and v1 (and v3 v4)) v2))
    (or (and (and v2 (and v3 v4)) v1) (and (and v1 v2) (and v3 v4)))))))

(or
 (or
  (or
   (or (and v1  v3 v4) (and v1 v2 v3 v4))
   (or
    (or (and v3 (and v1 v4)) (and (and v1 v3) v4))
    (or (and (and v2 v3) (and v1 v4)) (and (and (and v1 v2) v3) v4))))
  (or
   (or
    (or (and v4 (and v1 v3)) (and (and v1 v4) v3))
    (or (and (and v2 v4) (and v1 v3)) (and (and (and v1 v2) v4) v3)))
   (or
    (or (and (and v3 v4) v1) (and v1 (and v3 v4)))
    (or (and (and v2 (and v3 v4)) v1) (and (and v1 v2) (and v3 v4))))))
 (or
  (or
   (or (and v2 (and v1 (and v3 v4))) (and (and v1 v2) (and v3 v4)))
   (or
    (or (and (and v3 v2) (and v1 v4)) (and (and (and v1 v3) v2) v4))
    (or (and (and v2 v3) (and v1 v4)) (and (and (and v1 v2) v3) v4))))
  (or
   (or
    (or (and (and v4 v2) (and v1 v3)) (and (and (and v1 v4) v2) v3))
    (or (and (and v2 v4) (and v1 v3)) (and (and (and v1 v2) v4) v3)))
   (or
    (or (and (and (and v3 v4) v2) v1) (and (and v1 (and v3 v4)) v2))
    (or (and (and v2 (and v3 v4)) v1) (and (and v1 v2) (and v3 v4)))))))


(or (and v1 v3 v4) (and v1 v2 v3 v4)) => (or (and v1 (or (and v3 v4) (and v2 v3 v4))))


(v1 * v3 * v4) + (v1 * v2 * v3 * v4)

v1 * ((v3 * v4) + (v2 * v3 * v4))

(and v1 (or (and v3 v4) (and v2 v3 v4)))
(and (and v1 v3) (or (and v4) (and v2 v4)))
(and (and v4 (and v1 v3)) (or (and) (and v2)))
(and (and v4 (and v1 v3)) (or t v2))
(and (and v4 (and v1 v3)) t)
(and v4 (and v1 v3))
(and v1 (and v3 v4))




(or (and v1 (and v3 v4)) (or (and v2 (and v1 (and v3 v4))) (and (and v1 v2) (and v3 v4))))
(or (and v1 v3 v4) (and v1 v2 v3 v4) (and v1 v2 v3 v4)) ;; flatten both ands and ors
(or (and v1 v3 v4) (and v1 v2 v3 v4)) ;; delete duplicates (from or)
(and v1 v3 v4 (or (and) (and v2))) ;; ors of ands -- pull out common factors
(and v1 v3 v4 (or t v2))
(and v1 v3 v4 t)
(and v1 v3 v4)
(and v1 (and v3 v4))


(and (and a b) (and c d))
(and a (and b (and c d)))

;; to canonicalize

;; flatten ands and ors recursively.
;; undistribute (will delete dups in or of ands.
;; delete dups
;; (and x) => x
;; (or x) => x
;; (and) => t
;; (or) => nil
;; sort
;; unflatten
