;;;; package.lisp

(defpackage #:cassowary
  (:use #:cl)
  (:export #:init
	   #:shutdown
	   #:create-simplex-solver
	   #:variable-new
	   #:variable-lookup
	   #:variable-value
	   #:solver-solve
	   #:solver-resolve
	   #:constraint-parse
	   #:variable-nil-p
	   #:constraint-satisfied-p
	   #:constraint-remove
	   #:solver
	   #:solver-add-strong-stay
	   #:solver-set-edited-value
	   #:solver-set-changed-callback
	   #:solver-add-edit
	   #:solver-add-weak-edit
	   #:solver-add-strong-edit
	   #:solver-suggest-value
	   #:solver-begin-edit
	   #:solver-end-edit))

