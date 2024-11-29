(ql:quickload "alexandria")


(defmacro -> (v &rest es)
  (reduce
    (lambda (a b) (cons (first b) (cons a (rest b))))
    (map 'list (lambda (e) (if (listp e) e (list e))) es)
    :initial-value v
  )
)

(defun pairs (kv)
  (reverse (reduce
    (lambda (ac v)
      (-> ac first rest null
        (if
          (-> ac first first (list v) (cons (rest ac)))
          (cons (list v) ac)
        )
      )
    )
    (rest kv)
    :initial-value (-> kv first list list)
  ))
)

(defun cptable (m)
  (let ((n (make-hash-table)))
    (maphash (lambda (k v) (setf (gethash k n) v)) m)
    n
  )
)

(defun tassoc (m &rest kv)
  (reduce
    (lambda (n p) (setf (gethash (first p) n) (second p)) n)
    (pairs kv)
    :initial-value (if (null m) (make-hash-table) (alexandria:copy-hash-table m))
  )
)

(defun table (&rest kv) (apply #'tassoc (cons () kv)))

(defun partial (f &rest pre)
  (lambda (&rest post) (apply f (append pre post)))
)

(defun tstr (m)
  (alexandria:hash-table-alist m)
)
