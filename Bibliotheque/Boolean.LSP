;; A B Out
;; 0 0 0
;; 0 1 1
;; 1 0 1
;; 0 0 0
(defun xor (a b)
  (or (and a (not b)) (and (not a) b))
)

;; A B Out
;; 0 0 1
;; 0 1 0
;; 1 0 0
;; 1 1 0
(defun nor (a b)
  (not (or a b))
)

;; A B Out
;; 0 0 1
;; 0 1 0
;; 1 0 0
;; 1 1 1
(defun xnor (a b)
  (or (and a b) (and (not a) (not b)))
)

;; A B Out
;; 0 0 1
;; 0 1 1
;; 1 0 1
;; 1 1 0
(defun nand (a b)
  (not (and a b))
)

