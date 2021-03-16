;;;; quicklisp-stats.lisp

(defpackage #:quicklisp-stats
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:d #:drakma)
                    (#:s #:split-sequence))
  (:export
   ;; Variables
   #:*cache*
   ;; Conditions
   #:no-data-yet #:no-data-yet-month #:no-data-yet-year
   ;; API
   #:month #:all #:system-downloads))

(in-package #:quicklisp-stats)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Variables

(defvar *url* "https://www.quicklisp.org/stats/~4,'0D~:*/~4,'0D-~2,'0D.csv")

(defvar *cache* (make-hash-table :test #'equal)
  "The cache storing Quicklisp stats data. Keys are conses of year and month,
values are alists of system names and download counts.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Conditions

(defun no-data-yet-report (condition stream)
  (format stream "No Quicklisp stats data available yet for ~4,'0D-~2,'0D."
          (no-data-yet-year condition) (no-data-yet-month condition)))

(define-condition no-data-yet (error)
  ((year :reader no-data-yet-year :initarg :year)
   (month :reader no-data-yet-month :initarg :month))
  (:default-initargs :year (a:required-argument :year)
                     :month (a:required-argument :month))
  (:report no-data-yet-report))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; API

(defun %month (year month verbosep)
  (multiple-value-bind (response status)
      (d:http-request (format nil *url* year month))
    (when (= status 404)
      (error 'no-data-yet :month month :year year))
    (when verbosep
      (format t "~&;; Fetched ~4,'0D-~2,'0D." year month))
    (loop with split = (s:split-sequence #\Newline response
                                         :remove-empty-subseqs t)
          for line in (cdr split) ;; Skip the CSV header.
          for data = (s:split-sequence #\| line)
          for (date string-count name) = data
          collect (cons name (parse-integer string-count)))))

(defun month (year month &optional verbosep)
  "Downloads Quicklisp stats for a given month and returns them.
\
The result is cached in *CACHE*, so if data for a given month was already
downloaded, it is not fetched again."
  (check-type year (integer 2020))
  (check-type month (integer 1 12))
  (multiple-value-bind (value foundp) (gethash (cons year month) *cache*)
    (cond (foundp value)
          (t (let ((data (%month year month verbosep)))
               (setf (gethash (cons year month) *cache*) data)
               data)))))

(defun all (&optional verbosep)
  "Downloads all Quicklisp stats and returns them.
\
The result is cached in *CACHE*, so if data for a given month was already
downloaded, it is not fetched again."
  (let ((results '()))
    (handler-case
        (loop for year from 2020 do
          (loop for month from 1 to 12
                for data = (month year month verbosep)
                for result = (cons (list year month) data)
                do (push result results)))
      (no-data-yet () (nreverse results)))))

(defun system-downloads (system year month)
  "Returns the number of times SYSTEM was downloaded from Quicklisp during the
MONTH of YEAR, or NIL if the system was not found in Quicklisp stats for that
month.
\
The result is cached in *CACHE*, so if data for a given month was already
downloaded, it is not fetched again."
  (check-type system a:string-designator)
  (let ((systems (month year month))
        (string (string system)))
    (values (a:assoc-value systems string :test #'string-equal))))
