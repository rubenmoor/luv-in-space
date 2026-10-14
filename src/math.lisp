(in-package #:luv-in-space)

;;; Double vectors, quaternions and 3x3 matrices on the CPU, and the one
;;; single-float boundary (decision 65: Vectors and quaternions in math.lisp).
;;; A quaternion is (w x y z), unit, body -> inertial; a mat3 is row-major.
;;; Plain functions return fresh arrays; each `!' twin writes its first
;;; argument, which may alias an input.

(deftype vec3 () '(simple-array double-float (3)))
(deftype quat () '(simple-array double-float (4)))
(deftype mat3 () '(simple-array double-float (9)))

(defmacro with-vec3 (bindings &body body)
  "Bind the components of vec3s: ((ax ay az) a) ..."
  `(let* ,(loop for (names v) in bindings
                for g = (gensym)
                collect `(,g ,v)
                append (loop for n in names for i from 0
                             collect `(,n (aref ,g ,i))))
     (declare (ignorable ,@(loop for (names) in bindings append names)))
     ,@body))

(defmacro with-quat (bindings &body body)
  "Bind the components of quaternions: ((aw ax ay az) a) ..."
  `(with-vec3 ,bindings ,@body))

(declaim (inline make-vec3 make-quat make-mat3 vec3 quat
                 vec3-set! vec3-copy quat-set! quat-copy))

(defun make-vec3 () (make-array 3 :element-type 'double-float :initial-element 0d0))
(defun make-quat () (make-array 4 :element-type 'double-float :initial-element 0d0))
(defun make-mat3 () (make-array 9 :element-type 'double-float :initial-element 0d0))

(defun vec3-set! (out x y z)
  (declare (type vec3 out) (type double-float x y z))
  (setf (aref out 0) x (aref out 1) y (aref out 2) z)
  out)

(defun quat-set! (out w x y z)
  (declare (type quat out) (type double-float w x y z))
  (setf (aref out 0) w (aref out 1) x (aref out 2) y (aref out 3) z)
  out)

(defun vec3 (x y z)
  (declare (type real x y z))
  (vec3-set! (make-vec3) (float x 1d0) (float y 1d0) (float z 1d0)))

(defun quat (w x y z)
  (declare (type real w x y z))
  (quat-set! (make-quat) (float w 1d0) (float x 1d0) (float y 1d0) (float z 1d0)))

(defun vec3-copy (a) (declare (type vec3 a)) (copy-seq a))
(defun quat-copy (a) (declare (type quat a)) (copy-seq a))

(defun mat3 (&rest rows)
  "A mat3 from nine reals, row by row."
  (let ((m (make-mat3)))
    (loop for x in rows for i from 0 do (setf (aref m i) (float x 1d0)))
    m))

;;; vec3

(declaim (inline vec3-add! vec3-sub! vec3-scale! vec3-add-scaled! vec3-cross!
                 vec3-lerp! vec3-normalize! vec3-dot vec3-norm
                 vec3-add vec3-sub vec3-scale vec3-add-scaled vec3-cross
                 vec3-lerp vec3-normalize))

(defun vec3-add! (out a b)
  (declare (type vec3 out a b))
  (with-vec3 (((ax ay az) a) ((bx by bz) b))
    (vec3-set! out (+ ax bx) (+ ay by) (+ az bz))))

(defun vec3-sub! (out a b)
  (declare (type vec3 out a b))
  (with-vec3 (((ax ay az) a) ((bx by bz) b))
    (vec3-set! out (- ax bx) (- ay by) (- az bz))))

(defun vec3-scale! (out a s)
  (declare (type vec3 out a) (type double-float s))
  (with-vec3 (((ax ay az) a))
    (vec3-set! out (* s ax) (* s ay) (* s az))))

(defun vec3-add-scaled! (out a b s)
  "a + s b."
  (declare (type vec3 out a b) (type double-float s))
  (with-vec3 (((ax ay az) a) ((bx by bz) b))
    (vec3-set! out (+ ax (* s bx)) (+ ay (* s by)) (+ az (* s bz)))))

(defun vec3-cross! (out a b)
  (declare (type vec3 out a b))
  (with-vec3 (((ax ay az) a) ((bx by bz) b))
    (vec3-set! out (- (* ay bz) (* az by)) (- (* az bx) (* ax bz)) (- (* ax by) (* ay bx)))))

(defun vec3-lerp! (out a b s)
  "a + s (b - a)."
  (declare (type vec3 out a b) (type double-float s))
  (with-vec3 (((ax ay az) a) ((bx by bz) b))
    (vec3-set! out (+ ax (* s (- bx ax))) (+ ay (* s (- by ay))) (+ az (* s (- bz az))))))

(defun vec3-dot (a b)
  (declare (type vec3 a b))
  (with-vec3 (((ax ay az) a) ((bx by bz) b))
    (+ (* ax bx) (* ay by) (* az bz))))

(defun vec3-norm (a)
  (declare (type vec3 a))
  (sqrt (the (double-float 0d0) (vec3-dot a a))))

(defun vec3-normalize! (out a)
  (declare (type vec3 out a))
  (vec3-scale! out a (/ (vec3-norm a))))

(defun vec3-add (a b) (vec3-add! (make-vec3) a b))
(defun vec3-sub (a b) (vec3-sub! (make-vec3) a b))
(defun vec3-scale (a s) (vec3-scale! (make-vec3) a s))
(defun vec3-add-scaled (a b s) (vec3-add-scaled! (make-vec3) a b s))
(defun vec3-cross (a b) (vec3-cross! (make-vec3) a b))
(defun vec3-lerp (a b s) (vec3-lerp! (make-vec3) a b s))
(defun vec3-normalize (a) (vec3-normalize! (make-vec3) a))

;;; mat3, row-major: element (i, j) at 3i + j.

(declaim (inline mat3*vec3!))

(defun mat3*vec3! (out m v)
  (declare (type vec3 out v) (type mat3 m))
  (with-vec3 (((x y z) v))
    (vec3-set! out
               (+ (* (aref m 0) x) (* (aref m 1) y) (* (aref m 2) z))
               (+ (* (aref m 3) x) (* (aref m 4) y) (* (aref m 5) z))
               (+ (* (aref m 6) x) (* (aref m 7) y) (* (aref m 8) z)))))

(defun mat3*vec3 (m v) (mat3*vec3! (make-vec3) m v))

(defun mat3-transpose (m)
  (declare (type mat3 m))
  (let ((out (make-mat3)))
    (dotimes (i 3 out)
      (dotimes (j 3)
        (setf (aref out (+ (* 3 j) i)) (aref m (+ (* 3 i) j)))))))

(defun mat3* (a b)
  (declare (type mat3 a b))
  (let ((out (make-mat3)))
    (dotimes (i 3 out)
      (dotimes (j 3)
        (setf (aref out (+ (* 3 i) j))
              (loop for k below 3
                    sum (* (aref a (+ (* 3 i) k)) (aref b (+ (* 3 k) j)))
                      of-type double-float))))))

(defun mat3-inverse-symmetric (m)
  "Inverse of a symmetric 3x3 matrix (the inertia tensor) by its adjugate."
  (declare (type mat3 m))
  (let* ((a (aref m 0)) (b (aref m 1)) (c (aref m 2))
         (d (aref m 4)) (e (aref m 5)) (f (aref m 8))
         (c00 (- (* d f) (* e e)))
         (c01 (- (* c e) (* b f)))
         (c02 (- (* b e) (* c d)))
         (c11 (- (* a f) (* c c)))
         (c12 (- (* b c) (* a e)))
         (c22 (- (* a d) (* b b)))
         (s (/ (+ (* a c00) (* b c01) (* c c02)))))
    (mat3 (* s c00) (* s c01) (* s c02)
          (* s c01) (* s c11) (* s c12)
          (* s c02) (* s c12) (* s c22))))

;;; Quaternions

(declaim (inline quat-mul! quat-conjugate! quat-normalize! quat-rotate!
                 quat-derivative! quat-mul quat-conjugate quat-normalize
                 quat-rotate quat-derivative))

(defun quat-mul! (out a b)
  (declare (type quat out a b))
  (with-quat (((aw ax ay az) a) ((bw bx by bz) b))
    (quat-set! out
               (- (* aw bw) (* ax bx) (* ay by) (* az bz))
               (+ (* aw bx) (* ax bw) (* ay bz) (- (* az by)))
               (+ (* aw by) (- (* ax bz)) (* ay bw) (* az bx))
               (+ (* aw bz) (* ax by) (- (* ay bx)) (* az bw)))))

(defun quat-conjugate! (out q)
  (declare (type quat out q))
  (with-quat (((w x y z) q))
    (quat-set! out w (- x) (- y) (- z))))

(defun quat-normalize! (out q)
  (declare (type quat out q))
  (with-quat (((w x y z) q))
    (let ((s (/ (sqrt (the (double-float 0d0) (+ (* w w) (* x x) (* y y) (* z z)))))))
      (quat-set! out (* s w) (* s x) (* s y) (* s z)))))

(defun quat-rotate! (out q v)
  "q v q*: v in body axes to inertial for a unit q."
  (declare (type vec3 out v) (type quat q))
  (with-quat (((w x y z) q))
    (with-vec3 (((vx vy vz) v))
      ;; t = 2 u x v; v' = v + w t + u x t
      (let ((tx (* 2 (- (* y vz) (* z vy))))
            (ty (* 2 (- (* z vx) (* x vz))))
            (tz (* 2 (- (* x vy) (* y vx)))))
        (vec3-set! out
                   (+ vx (* w tx) (- (* y tz) (* z ty)))
                   (+ vy (* w ty) (- (* z tx) (* x tz)))
                   (+ vz (* w tz) (- (* x ty) (* y tx))))))))

(defun quat-derivative! (out q w)
  "q-dot = 1/2 q (x) (0, w), w the angular velocity in body axes."
  (declare (type quat out q) (type vec3 w))
  (with-quat (((qw qx qy qz) q))
    (with-vec3 (((p r s) w))
      (quat-set! out
                 (* -0.5d0 (+ (* qx p) (* qy r) (* qz s)))
                 (* 0.5d0 (+ (* qw p) (* qy s) (- (* qz r))))
                 (* 0.5d0 (+ (* qw r) (* qz p) (- (* qx s))))
                 (* 0.5d0 (+ (* qw s) (* qx r) (- (* qy p))))))))

(defun quat-mul (a b) (quat-mul! (make-quat) a b))
(defun quat-conjugate (q) (quat-conjugate! (make-quat) q))
(defun quat-normalize (q) (quat-normalize! (make-quat) q))
(defun quat-rotate (q v) (quat-rotate! (make-vec3) q v))
(defun quat-derivative (q w) (quat-derivative! (make-quat) q w))

(defun quat-from-axis-angle (axis angle)
  "Rotation by angle (radians, right-handed) about axis (need not be unit)."
  (declare (type vec3 axis) (type double-float angle))
  (let ((u (vec3-normalize axis))
        (s (sin (* 0.5d0 angle))))
    (quat (cos (* 0.5d0 angle)) (* s (aref u 0)) (* s (aref u 1)) (* s (aref u 2)))))

(defun quat-slerp (a b s)
  "Spherical interpolation from a (s = 0) to b (s = 1) along the shortest arc."
  (declare (type quat a b) (type double-float s))
  (let* ((d (loop for i below 4 sum (* (aref a i) (aref b i)) of-type double-float))
         (sign (if (minusp d) -1d0 1d0))
         (d (abs d)))
    (multiple-value-bind (ka kb)
        (if (> d 0.9995d0)
            (values (- 1 s) (* sign s))
            (let* ((theta (acos (min d 1d0)))
                   (sin-theta (sin theta)))
              (values (/ (sin (* (- 1 s) theta)) sin-theta)
                      (* sign (/ (sin (* s theta)) sin-theta)))))
      (let ((out (make-quat)))
        (dotimes (i 4)
          (setf (aref out i) (+ (* ka (aref a i)) (* kb (aref b i)))))
        (quat-normalize! out out)))))

(defun mat3-from-quat (q)
  "The rotation matrix of q: (mat3*vec3 m v) = (quat-rotate q v)."
  (declare (type quat q))
  (with-quat (((w x y z) q))
    (mat3 (- 1 (* 2 (+ (* y y) (* z z)))) (* 2 (- (* x y) (* w z))) (* 2 (+ (* x z) (* w y)))
          (* 2 (+ (* x y) (* w z))) (- 1 (* 2 (+ (* x x) (* z z)))) (* 2 (- (* y z) (* w x)))
          (* 2 (- (* x z) (* w y))) (* 2 (+ (* y z) (* w x))) (- 1 (* 2 (+ (* x x) (* y y)))))))

(defun quat-from-mat3 (m)
  "The unit quaternion (w >= 0) of a rotation matrix (Shepperd's method)."
  (declare (type mat3 m))
  (let ((m00 (aref m 0)) (m01 (aref m 1)) (m02 (aref m 2))
        (m10 (aref m 3)) (m11 (aref m 4)) (m12 (aref m 5))
        (m20 (aref m 6)) (m21 (aref m 7)) (m22 (aref m 8)))
    (let* ((trace (+ m00 m11 m22))
           (q (cond ((>= trace (max m00 m11 m22))
                     (let ((s (* 2 (sqrt (+ 1 trace)))))
                       (quat (/ s 4) (/ (- m21 m12) s) (/ (- m02 m20) s) (/ (- m10 m01) s))))
                    ((>= m00 (max m11 m22))
                     (let ((s (* 2 (sqrt (+ 1 m00 (- m11) (- m22))))))
                       (quat (/ (- m21 m12) s) (/ s 4) (/ (+ m01 m10) s) (/ (+ m02 m20) s))))
                    ((>= m11 m22)
                     (let ((s (* 2 (sqrt (+ 1 m11 (- m00) (- m22))))))
                       (quat (/ (- m02 m20) s) (/ (+ m01 m10) s) (/ s 4) (/ (+ m12 m21) s))))
                    (t
                     (let ((s (* 2 (sqrt (+ 1 m22 (- m00) (- m11))))))
                       (quat (/ (- m10 m01) s) (/ (+ m02 m20) s) (/ (+ m12 m21) s) (/ s 4)))))))
      (when (minusp (aref q 0))
        (dotimes (i 4) (setf (aref q i) (- (aref q i)))))
      (quat-normalize! q q))))

(defun quat-from-basis (x y z)
  "The attitude whose body axes x, y, z (unit, orthogonal, right-handed) are
the given inertial vectors."
  (declare (type vec3 x y z))
  (quat-from-mat3 (mat3 (aref x 0) (aref y 0) (aref z 0)
                        (aref x 1) (aref y 1) (aref z 1)
                        (aref x 2) (aref y 2) (aref z 2))))

;;; The single-float boundary (decision 5: Precision of positions)

(defun relative-single (p origin &optional
                                   (out (make-array 3 :element-type 'single-float))
                                   (index 0))
  "p - origin, subtracted in double, then coerced into out at index."
  (declare (type vec3 p origin) (type (simple-array single-float (*)) out)
           (type fixnum index))
  (dotimes (i 3 out)
    (setf (aref out (+ index i))
          (coerce (- (aref p i) (aref origin i)) 'single-float))))
