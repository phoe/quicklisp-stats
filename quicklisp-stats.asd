;;;; quicklisp-stats.asd

(asdf:defsystem #:quicklisp-stats
  :description "Fetches and operates on Quicklisp download statistics."
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria #:drakma #:split-sequence)
  :components ((:file "quicklisp-stats")))
