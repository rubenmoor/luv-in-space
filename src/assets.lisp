(in-package #:luv-in-space)

;;; Finding and decoding assets (docs/architecture.md "Assets"; decision 13:
;;; Asset storage and attribution). The package's launcher sets
;;; LUV_IN_SPACE_ASSETS; the dev shell uses the repo's assets/.

(defun assets-root (&optional (env (uiop:getenv "LUV_IN_SPACE_ASSETS")))
  "The assets directory: ENV when set and non-empty, else the system's assets/."
  (if (and env (plusp (length env)))
      (uiop:ensure-directory-pathname (uiop:parse-native-namestring env))
      (asdf:system-relative-pathname "luv-in-space" "assets/")))

(defun asset-path (relative)
  "The pathname of RELATIVE (a string such as \"earth/blue-marble-5400.jpg\")
under the assets root; an error naming it when the file is missing."
  (let ((path (merge-pathnames (uiop:parse-unix-namestring relative) (assets-root))))
    (unless (probe-file path)
      (error "Asset ~A missing: no file ~A (assets root from LUV_IN_SPACE_ASSETS ~
or the system's assets/)." relative (uiop:native-namestring path)))
    path))

(defun copy-surface-pixels (pixels width height pitch)
  "A rank-2 (unsigned-byte 32) array HEIGHT x WIDTH of the 32-bit words at PIXELS."
  (declare (optimize speed) (type sb-sys:system-area-pointer pixels)
           (type (integer 0 65535) width height) (type (integer 0 #.(expt 2 20)) pitch))
  (let* ((words (make-array (list height width) :element-type '(unsigned-byte 32)))
         (flat (sb-ext:array-storage-vector words)))
    (dotimes (y height words)
      (let ((row (* y pitch)) (start (* y width)))
        (dotimes (x width)
          (setf (aref flat (+ start x)) (sb-sys:sap-ref-32 pixels (+ row (* 4 x)))))))))

(defun load-image-pixels (path)
  "Decode the image at PATH through sdl3-image into a rank-2 (unsigned-byte 32)
array rows x columns of ABGR8888 words (R in the low byte), as
texture-from-pixels takes it."
  (let ((loaded (sdl3-image:img-load (uiop:native-namestring path))))
    (when (cffi:null-pointer-p loaded)
      (error "Cannot decode ~A: ~A" path (sdl3:get-error)))
    (let ((surface (unwind-protect (sdl3:convert-surface loaded :abgr8888)
                     (sdl3:destroy-surface loaded))))
      (when (cffi:null-pointer-p surface)
        (error "Cannot convert ~A to ABGR8888: ~A" path (sdl3:get-error)))
      ;; SDL_Surface: flags, format, w, h, pitch, then the pixels pointer.
      (unwind-protect
           (copy-surface-pixels (cffi:mem-ref surface :pointer 24)
                                (cffi:mem-ref surface :int 8)
                                (cffi:mem-ref surface :int 12)
                                (cffi:mem-ref surface :int 16))
        (sdl3:destroy-surface surface)))))

(defun downsample-pixels (pixels factor)
  "PIXELS (as load-image-pixels returns them) box-filtered by FACTOR: each
output pixel the mean of its FACTOR x FACTOR block per byte, the last row
and column over what is left (decision 78: Sky in numbers: stars, Sun, Moon
and Earth's look: the far Earth's copy, as Vulkan textures in luv have one
mip level)."
  (declare (optimize speed) (type (simple-array (unsigned-byte 32) (* *)) pixels)
           (type (integer 1 64) factor))
  (destructuring-bind (height width) (array-dimensions pixels)
    (declare (type (integer 1 65535) height width))
    (let* ((rows (ceiling height factor)) (columns (ceiling width factor))
           (out (make-array (list rows columns) :element-type '(unsigned-byte 32)))
           (sums (make-array (* 4 columns) :element-type '(unsigned-byte 32))))
      (dotimes (row rows out)
        (fill sums 0)
        (let ((y0 (* row factor)) (y1 (min height (* (1+ row) factor))))
          (loop for y from y0 below y1
                do (dotimes (x width)
                     (let ((word (aref pixels y x)) (base (* 4 (floor x factor))))
                       (dotimes (k 4)
                         (incf (aref sums (+ base k)) (ldb (byte 8 (* 8 k)) word))))))
          (dotimes (column columns)
            (let ((n (* (- y1 y0) (- (min width (* (1+ column) factor)) (* column factor))))
                  (word 0))
              (declare (type (unsigned-byte 32) word))
              (dotimes (k 4)
                (setf word (dpb (round (aref sums (+ (* 4 column) k)) n) (byte 8 (* 8 k)) word)))
              (setf (aref out row column) word))))))))

;;; The star table (assets/stars/SOURCE.md; decision 78: Sky in numbers:
;;; stars, Sun, Moon and Earth's look).

(defstruct (star-table (:constructor %make-star-table))
  "The naked-eye stars: per star i, its ICRF unit direction at 3i..3i+2 of
DIRECTIONS, its visual MAGNITUDES, its B-V COLOUR-INDICES (0 where
COLOURED is 0: no colour index, drawn white) and its HIP number (0: none)."
  (count 0 :type fixnum)
  (directions #() :type (simple-array single-float (*)))
  (magnitudes #() :type (simple-array single-float (*)))
  (colour-indices #() :type (simple-array single-float (*)))
  (coloured #* :type simple-bit-vector)
  (hip #() :type (simple-array (unsigned-byte 32) (*))))

(defun read-star-rows (path)
  "The forms of the star table at PATH, read without evaluation in the
standard readtable, numbers as doubles; an error unless each is
(hip ra-deg dec-deg mag ci)."
  (with-open-file (stream path :external-format :utf-8)
    (with-standard-io-syntax
      (let ((*read-eval* nil) (*read-default-float-format* 'double-float))
        (loop for row = (read stream nil stream)
              until (eq row stream)
              do (unless (and (listp row) (= (length row) 5)
                              (typep (first row) '(or null (integer 1 #.(1- (expt 2 32)))))
                              (every #'realp (subseq row 1 4))
                              (typep (fifth row) '(or null real)))
                   (error "Star table ~A: malformed row ~S" path row))
              collect row)))))

(defun load-star-table (&optional (path (asset-path "stars/hyg-6.5.sexp")))
  "The star table at PATH as a star-table of specialised arrays."
  (let* ((rows (read-star-rows path))
         (n (length rows))
         (table (%make-star-table
                 :count n
                 :directions (make-array (* 3 n) :element-type 'single-float)
                 :magnitudes (make-array n :element-type 'single-float)
                 :colour-indices (make-array n :element-type 'single-float
                                               :initial-element 0.0)
                 :coloured (make-array n :element-type 'bit :initial-element 0)
                 :hip (make-array n :element-type '(unsigned-byte 32)
                                    :initial-element 0))))
    (loop for (hip ra dec mag ci) in rows
          for i from 0
          do (let ((alpha (* (float ra 1d0) (/ pi 180)))
                   (delta (* (float dec 1d0) (/ pi 180)))
                   (directions (star-table-directions table)))
               (setf (aref directions (* 3 i)) (float (* (cos delta) (cos alpha)) 1.0)
                     (aref directions (+ (* 3 i) 1)) (float (* (cos delta) (sin alpha)) 1.0)
                     (aref directions (+ (* 3 i) 2)) (float (sin delta) 1.0)
                     (aref (star-table-magnitudes table) i) (float mag 1.0))
               (when hip (setf (aref (star-table-hip table) i) hip))
               (when ci
                 (setf (aref (star-table-colour-indices table) i) (float ci 1.0)
                       (sbit (star-table-coloured table) i) 1))))
    table))
