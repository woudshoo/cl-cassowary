(in-package :cassowary)


(defun print-solution ()
  (format t "Solution: x ~A, y ~A, z ~A~%" 
	  (variable-value "x")
	  (variable-value "y")
	  (variable-value "z")))

(defun simple-test ()
  (let ((solver (cassowary:create-simplex-solver)))
    (variable-new "x" :value 10.0 :solver solver)
    (variable-new "y" :value 20.0 :solver solver)
    (variable-new "z" :value 30.0 :solver solver)
    (constraint-parse "x>=30" "weak" :solver solver)
    (constraint-parse "y>=100" "weak" :solver solver)
    (constraint-parse "z=x+y" "strong" :solver solver)
    (constraint-parse "z<=50" "required" :solver solver)
    (solver-solve solver)
    (print-solution)
    solver))


(defun modify-test ()
  (let ((solver (simple-test)))
    (solver-set-edited-value solver "y" 200.0)
    (solver-resolve solver)
    (print-solution)))


(defun call-back-test ()
  (let ((solver (simple-test)))
    (solver-set-changed-callback solver (lambda (s v)
					  (format t "~A = ~A~%"
						  (variable-name v) 
						  (variable-value v))))
    (format t "Will change value!~%")
    (solver-set-edited-value solver "z" 40)
    (format t "Now resolving!~%")
#+nil    (solver-resolve solver)
    (format t "Done resolving!~%")))