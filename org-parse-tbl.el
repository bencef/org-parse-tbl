(defun get-headers (tbl)
  "Get the name of headers from a table."
  (car tbl))

(defun get-data (tbl)
  "Get unprocessed rows of data"
  (let (acc)
    (dolist (row (cdr tbl) (nreverse acc))
      (when (listp row)
        (push row acc)))))

(defun parse-row (headers readers row)
  "Collect the elements of row into a hash-map
using headers as keys and applying readers."
  (let ((h (make-hash-table :test #'equal)))
    (loop for key in headers
          for reader in readers
          for elt in row
          do (setf (gethash key h) (funcall reader elt)))
    h))

(defun org-parse-tbl (tbl readers)
  "Parse data from a table into a list of hash-maps.
Using headers as keys and applying readers."
  (let ((headers (get-headers tbl))
        (data (get-data tbl)))
    (when (not (= (length headers) (length readers)))
      (error "Length of headers doesn't match the lenght of readers."))
    (loop for row in data
          collect (parse-row headers readers row))))

(provide 'org-parse-tbl)
