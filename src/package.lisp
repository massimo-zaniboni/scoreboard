;; SPDX-License-Identifier: LGPL-3.0-or-later
;; Copyright (C) 2023 Massimo Zaniboni <mzan@dokmelody.org>

(defpackage :scoreboard
  (:import-from :trivial-types
     :proper-list
     :tuple)
  (:import-from :alexandria
     :hash-table-keys)
  (:import-from :serapeum :let1)
  (:use :cl :defstar :eazy-gnuplot)
  (:export main))

(in-package :scoreboard)
