;; SPDX-License-Identifier: LGPL-3.0-or-later
;; Copyright (C) 2023 Massimo Zaniboni <mzan@dokmelody.org>

(in-package :scoreboard)

(defparameter *data-dir* #P"./")

(defparameter *data-in*
  (uiop:merge-pathnames* "data.in" *data-dir*))

(defparameter *graph-out*
  (uiop:merge-pathnames* "chart.svg" *data-dir*))

(defparameter *web-browser-cmd* "firefox")

(defparameter *data-file* #P"../data.in")

(defclass player ()
  ((name
    :accessor name
    :initarg :name
    :type string )
   (pos
    :accessor pos
    :initarg :pos
    :type integer
    :initform 0)
   (tot-points
    :accessor tot-points
    :type integer
    :initform 0 )
   (data-points
    :accessor data-points
    :initform (make-array 128 :fill-pointer 0 :adjustable t))))

(defmethod turns# ((p Player))
  (length (data-points p)))

(defmethod add-data-point ((p Player) d)
  (incf (tot-points p) d)
  (vector-push-extend (tot-points p) (data-points p)))

(defun compare-players (p1 p2)
  (> (tot-points p1) (tot-points p2)))

(defclass game ()
  ((target-points
    :accessor target-points
    :type integer
    :initform 10000)
   (players
    :accessor players
    :initform (make-array 25 :fill-pointer 0 :adjustable t))))

; Maintain the same proportion for a certain number of turns
(defmethod graph-turns ((g game))
  (let ((t1 (turns# g)))
    (+ 1 t1 (mod t1 5))))

(defmethod turns# ((g game))
  (apply #'min (map 'list #'turns# (players g))))

(defmethod players# ((g game))
  (length (players g)))

(defmethod add-player ((g game) (p player))
  (let ((pos (1+ (players# g))))
    (setf (pos p) pos)
    (vector-push-extend p (players g))))

(defmethod player-turn ((g game))
  (loop with g-turn = (turns# g)
        for p being the elements of (players g)
        for p-turn = (turns# p)
        thereis (when (= p-turn g-turn) p)))

(defun robust-split (str)
  "Split a string accetping rendundant spaces."
  (remove-if #'str:blankp (uiop:split-string str)))

(defun parse-data ()
  (loop with game = (make-instance 'game)
        for cmd in (uiop:read-file-lines *data-in*)
        for cmd* = (robust-split cmd)
        for skip? = (null cmd*)
        for cmd1 = (car cmd*)
        for cmdr = (cdr cmd*)
        for cmd2 = (car cmdr)
        for is-data? = (string-equal cmd1 "d")
        for is-valid-cmd? = (member cmd1 '("d" "w" "p" "#") :test #'string-equal)
        for line# = -1 then (if is-data? (1+ line#) line#)
        for file-line# from 1
        when (and (not skip?)
                  (not is-valid-cmd?)) do
          (error (format nil "Parsing error at line ~a" file-line#))
        when (string-equal cmd1 "p") do
             (let* ((p# (players# game))
                    (p (make-instance 'player :name (str:join " " cmdr))))
                 (add-player game p))
        when (string-equal cmd1 "w") do
             (setf (target-points game) (parse-integer cmd2))
        when is-data? do
          (let* ((points (apply #'+ (mapcar #'parse-integer cmdr)))
                 (i (mod line# (players# game)))
                 (p (elt (players game) i)))
            (add-data-point p points))
       finally (return game)
        ))

(defmethod to-gnuplot ((g game))
  (with-plots (s :debug nil)
    (format s "~%set terminal svg size 1280,720 dynamic")
    (format s "~%set output \"~a\"" *graph-out*)
    (format s "~%set style data linespoints")
    (format s "~%set size 0.9, 1")
    (format s "~%set yrange[0:~a]" (target-points g))
    (format s "~%set xrange[0:~a]" (graph-turns g))

    (loop for p being the elements of (players g)
          for pos = (pos p)
          do (format s "~%set style line ~a pointtype ~a" pos pos))

    (loop for p being the elements of (players g)
          for i from 1
          do (progn
               (format s "~%")
               (format s "$Player")
               (format s "~a << EOD~%0" i)
               (loop for x being the elements of (data-points p)
                     do (format s "~%~a" x))
               (format s "~%EOD~%")))

    (format s "~%plot ")
    (loop with players = (sort (coerce (players g) 'list)  #'compare-players)
          for p in players
          for pos = (pos p)
          for sep = "" then ", "
          for classified from 1
          do (format s "~a $Player~a using :1 title '  ~a, ~a points' at end ls ~a" sep pos (name p) (tot-points p) pos))
    (format s "~%")))

(defun update-graph ()
  (let ((g (parse-data)))
    (to-gnuplot g)
    (cmd:with-working-directory (*data-dir*)
      (cmd:cmd& *web-browser-cmd* *graph-out*))))

(defun d (&rest xs)
  "Add the points to the data-file, and update the graph."
  (with-open-file (s *data-in* :direction :output :if-exists :append :if-does-not-exist :create)
    (format s "d ")
    (loop for x in xs
          for sep = "" then " "
          do (format s "~a~a" sep x))
    (format s "~%"))
  (update-graph))

(defun prompt-for-data-and-update ()
  "Read lines of data, updating the graph."
  (loop for g = (parse-data)
        for p = (player-turn g)
        for turn = (turns# g)
        for line = (progn
                     (format t "~%Turn ~a for player ~a: " turn (name p))
                     (finish-output)
                     (read-line *standard-input*))
        for xs = (mapcar #'parse-integer (robust-split line))
        for line? = (> (length line) 0)
        while line?
        do (apply #'d xs)))

(defun generate-data-file-template ()
   (with-open-file (s *data-in* :direction :output :if-exists :overwrite :if-does-not-exist :create)
    (format s "
# The points to reach, for winning the game
w 10000

# The players
p Maurizio
p Renata
p Linda
p Luca
p Massimo
p Andrea
p Katrin
p Alessandro
p Torres

# The points for each player start with  `d`.
#
# > d 150
# these are 150 points to the first player at first turn
#
# > d 150 50
# there are 200 points for the second player at first turn,
# because the points are summed
#
# Points can be inserted using the read-line utility
# of scoreboard, or manually edited here.

")))

(defun main () (prompt-for-data-and-update))
