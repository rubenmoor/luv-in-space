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
