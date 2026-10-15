(in-package #:luv-in-space)

;;; The screen-space overlay: dot-matrix labels (decision 7: Placeholder
;;; labels: dot-matrix overlay quads) and their pipeline.

(defparameter *label* "PLACEHOLDER(76 77 80)"
  "Top left until the parts that replace the placeholders land.")

(defparameter *glyphs*
  '((#\P "####." "#...#" "#...#" "####." "#...." "#...." "#....")
    (#\L "#...." "#...." "#...." "#...." "#...." "#...." "#####")
    (#\A ".###." "#...#" "#...#" "#####" "#...#" "#...#" "#...#")
    (#\C ".###." "#...#" "#...." "#...." "#...." "#...#" ".###.")
    (#\E "#####" "#...." "#...." "####." "#...." "#...." "#####")
    (#\H "#...#" "#...#" "#...#" "#####" "#...#" "#...#" "#...#")
    (#\O ".###." "#...#" "#...#" "#...#" "#...#" "#...#" ".###.")
    (#\D "####." "#...#" "#...#" "#...#" "#...#" "#...#" "####.")
    (#\R "####." "#...#" "#...#" "####." "#.#.." "#..#." "#...#")
    (#\( "..#.." ".#..." "#...." "#...." "#...." ".#..." "..#..")
    (#\) "..#.." "...#." "....#" "....#" "....#" "...#." "..#..")
    (#\0 ".###." "#...#" "#..##" "#.#.#" "##..#" "#...#" ".###.")
    (#\1 "..#.." ".##.." "..#.." "..#.." "..#.." "..#.." ".###.")
    (#\2 ".###." "#...#" "....#" "...#." "..#.." ".#..." "#####")
    (#\3 "####." "....#" "....#" ".###." "....#" "....#" "####.")
    (#\4 "...#." "..##." ".#.#." "#..#." "#####" "...#." "...#.")
    (#\5 "#####" "#...." "####." "....#" "....#" "#...#" ".###.")
    (#\6 ".###." "#...." "#...." "####." "#...#" "#...#" ".###.")
    (#\7 "#####" "....#" "...#." "..#.." ".#..." ".#..." ".#...")
    (#\8 ".###." "#...#" "#...#" ".###." "#...#" "#...#" ".###.")
    (#\9 ".###." "#...#" "#...#" ".####" "....#" "....#" ".###."))
  "5x7 dot-matrix glyphs for placeholder labels; other characters are blank.")

(defun quad (anchor-x anchor-y left top size color)
  "Two triangles of overlay vertices: a SIZE-pixel square at pixel offset
LEFT, TOP from the clip-space anchor."
  (loop for (dx dy) in '((0 0) (1 0) (1 1) (0 0) (1 1) (0 1))
        append (list* anchor-x anchor-y (+ left (* dx size)) (+ top (* dy size)) color)))

(defun label-vertices (string &key (dot 3) (margin 16) (color '(1.0 0.75 0.2)))
  "Overlay vertices drawing STRING in dot-matrix glyphs at the top left."
  (loop for char across string
        for column from 0
        append (loop for row in (rest (assoc char *glyphs*))
                     for y from 0
                     append (loop for cell across row
                                  for x from 0
                                  when (char= cell #\#)
                                    append (quad -1.0 -1.0
                                                 (+ margin (* dot (+ x (* 6 column))))
                                                 (+ margin (* dot y))
                                                 dot color)))))

(defconstant +overlay-vertex-floats+ 7 "Anchor xy, pixel offset xy, colour rgb.")

(defun overlay-pipeline (renderer layout)
  "The :overlay live pipeline: screen-space quads, no depth test or write."
  (live-pipeline renderer :overlay layout
                 `((:array-stride ,(* 4 +overlay-vertex-floats+)
                    :attributes ((:shader-location 0 :offset 0 :format :float32x2)
                                 (:shader-location 1 :offset 8 :format :float32x2)
                                 (:shader-location 2 :offset 16 :format :float32x3))))
                 :depth-stencil '(:format :depth32-float :depth-write-enabled nil
                                  :depth-compare :always)))
