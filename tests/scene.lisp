(in-package #:luv-in-space.tests)

(defun vertex-norms (data)
  (loop for i below (length data) by 3
        collect (sqrt (+ (expt (aref data i) 2)
                         (expt (aref data (+ i 1)) 2)
                         (expt (aref data (+ i 2)) 2)))))

(define-test uv-sphere-counts
  ;; A triangle list: two triangles per stack and slice, three floats each.
  (loop for (stacks slices) in '((1 3) (4 8) (32 64))
        for data = (lis::uv-sphere :stacks stacks :slices slices)
        do (is = (* stacks slices 6 3) (length data))
           (is eq 'single-float (array-element-type data))))

(define-test uv-sphere-unit-radius
  ;; Each position is also its normal, so this checks both.
  (let ((norms (vertex-norms (lis::uv-sphere :stacks 8 :slices 16))))
    (true (every (lambda (r) (< (abs (- r 1)) 1e-5)) norms))))

(define-test uv-sphere-poles
  (let* ((data (lis::uv-sphere :stacks 8 :slices 16))
         (ys (loop for i from 1 below (length data) by 3 collect (aref data i))))
    (is = 1.0 (reduce #'max ys))
    (is = -1.0 (reduce #'min ys))))
