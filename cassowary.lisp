;;;; cassowary.lisp

(in-package #:cassowary)

;;; "cassowary" goes here. Hacks and glory await!
(defparameter *variable-changed-callback* nil)

(cffi:define-foreign-library cassowary
  (:darwin "libccassowary.dylib"))

(cffi:use-foreign-library cassowary)

(cffi:defctype cl-variable :pointer)
(cffi:defctype cl-solver :pointer)
(cffi:defctype cl-simplex-solver :pointer)
(cffi:defctype cl-constraint :pointer)
(cffi:defctype cl-boolean :int)
(cffi:defctype cl-var-map :pointer)

(cffi:defcfun ("CL_SetVarMap" %cl-set-var-map)
    cl-var-map
  (var-map cl-var-map))

(cffi:defcfun ("CL_GetVarMap" %cl-get-var-map)
    cl-var-map)

(cffi:defcfun ("CL_VarMapNew" %cl-var-map-new)
    cl-var-map)

(cffi:defcfun ("CL_Init" %cl-init)
    :void)

(cffi:defcfun ("CL_Shutdown" %cl-shutdown)
    :void)

(cffi:defcfun ("CL_ClvNew" %cl-clv-new)
    cl-variable
  (name :string)
  (value :double)
  (solver cl-simplex-solver))

(cffi:defcfun ("CL_VariableName" %cl-variable-name)
    :string
  (variable cl-variable))

(cffi:defcfun ("CL_SimplexSolverNew" %cl-simplex-solver-new)
    cl-simplex-solver)

;; Callbacks, lets do that later!

(cffi:defcfun ("CL_ClvLookup" %cl-clv-lookup)
    cl-variable
  (name :string))

(cffi:defcfun ("CL_ClvValue" %cl-clv-value)
    :double
  (variable cl-variable))

(cffi:defcfun ("CL_ClvIsNil" %cl-clv-is-nil)
    cl-boolean
  (variable cl-variable))

;; Not sure we want to use this?
(cffi:defcfun ("CL_ParseConstraint" %cl-parse-constraint)
    cl-constraint
  (constraint-rule :string)
  (constraint-strength :string))

(cffi:defcfun ("CL_FIsSatisfied" %cl-f-is-satisfied)
    cl-boolean
  (constraint cl-constraint))

(cffi:defcfun ("CL_AddConstraint" %cl-add-constraint)
    :int
  (solver cl-solver)
  (constraint cl-constraint))

(cffi:defcfun ("CL_RemoveConstraint" %cl-remove-constraint)
    :int
  (solver cl-solver)
  (constraint cl-constraint))

(cffi:defcfun ("CL_SimplexSolverAddStrongStay" %cl-simplex-solver-add-strong-stay)
    :void
  (solver cl-simplex-solver)
  (variable cl-variable)
  (weight :double))

(cffi:defcfun ("CL_SimplexSolverAddStay" %cl-simplex-solver-add-stay)
    :void
  (solver cl-simplex-solver)
  (variable cl-variable)
  (weight :double))

(cffi:defcfun ("CL_Solve" %cl-solve)
    :void
  (solver cl-solver))

(cffi:defcfun ("CL_Resolve" %cl-resolve)
    :void
  (solver cl-solver))

(cffi:defcfun ("CL_SimplexSolverSetEditedValue" %cl-simplex-solver-set-edited-value)
    :void
  (solver cl-simplex-solver)
  (variable cl-variable)
  (value :double))

(cffi:defcfun ("CL_SimplexSolverSuggestValue" %cl-simplex-solver-suggest-value)
    :void
  (solver cl-simplex-solver)
  (variable cl-variable)
  (value :double))

(cffi:defcfun ("CL_SimplexSolverAddEditVar" %cl-simplex-solver-add-edit-var)
    :boolean
  (solver cl-simplex-solver)
  (variable cl-variable)
  (weight :double))


(cffi:defcfun ("CL_SimplexSolverAddEditVarStrong" %cl-simplex-solver-add-edit-var-strong)
    :boolean
  (solver cl-simplex-solver)
  (variable cl-variable)
  (weight :double))

(cffi:defcfun ("CL_SimplexSolverAddEditVarWeak" %cl-simplex-solver-add-edit-var-weak)
    :boolean
  (solver cl-simplex-solver)
  (variable cl-variable)
  (weight :double))

(cffi:defcfun ("CL_SimplexSolverRemoveEditVar" %cl-simplex-solver-remove-edit-var)
    :boolean
  (solver cl-simplex-solver)
  (variable cl-variable))

(cffi:defcfun ("CL_SimplexSolverBeginEdit" %cl-simplex-solver-begin-edit)
    :void
  (solver cl-simplex-solver))

(cffi:defcfun ("CL_SimplexSolverEndEdit" %cl-simplex-solver-end-edit)
    :void
  (solver cl-simplex-solver))

(cffi:defcfun ("CL_SolverSetChangeClvCallback" %cl-solver-set-changed-clv-callback)
    :void
  (solver cl-solver)
  (callback :pointer))
  
(cffi:defcallback variable-changed :void ((variable cl-variable) (solver cl-solver))
  (handler-case 
      (when *variable-changed-callback*
	(funcall *variable-changed-callback* solver variable))
    (error ())))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(defun init ()
  "Initializes the C library and sets up the variable map."
  (%cl-init)
  (%cl-set-var-map (%cl-var-map-new)))

(defun shutdown ()
  "Cleans up the library"
  (%cl-shutdown))

(defun create-simplex-solver ()
  "Creates the simplex solver"
  (%cl-simplex-solver-new))

(defun variable-new (variable-name &optional &key (value 0.0) (solver nil))
  "Creates a new variable with name `variable-name'
If the keyword argument `solver' is supplied it will
add a stay constraint for value `value' to the solver."
  (%cl-clv-new variable-name 
	       (coerce value 'double-float)
	       (or solver (cffi:null-pointer))))

(defun variable-name (variable)
  "Returns the name of the variable"
  (%cl-variable-name variable))

(defun variable-lookup (variable-name)
  "Looks up the variable in the global variable map and returns
the corresponding variable object"
  (%cl-clv-lookup variable-name))

(defun variable-for-designator (variable)
  "Returns the variable if it is a variable or looks it up if it is a string"
  (if (stringp variable) (variable-lookup variable) variable))

(defun variable-value (variable)
  "Returns the value of the variable."
  (%cl-clv-value (variable-for-designator variable)))

(defun variable-nil-p (variable)
  "Returns t if the `variable' is nil.
  (need to lookup in cassowary doc what this means)"
  (not (= 0 (%cl-clv-is-nil (variable-for-designator variable)))))

(defun solver-solve (solver)
  "Do an initial solve of posted problem."
  (%cl-solve solver))

(defun solver-resolve (solver)
  "Do a resolve after changing valus and or adding constraints."
  (%cl-resolve solver))

(defun solver-begin-edit (solver)
  "Starts an editing sequence, first add edit variables and while
then call this function and now you are setup for suggesting values and doing resolves."
  (%cl-simplex-solver-begin-edit solver))

(defun solver-end-edit (solver)
  "Stops an editing sequence and removes all edit variables."
  (%cl-simplex-solver-end-edit solver))

(defun solver-add-stay (solver variable &optional (weight 1))
  "Adds a stay constraint on the `variable' to the `solver'."
  (%cl-simplex-solver-add-stay solver 
			       (variable-for-designator variable)
			       (coerce weight 'double-float)))

(defun solver-add-strong-stay (solver variable &optional (weight 1))
  "Adds a stay constraint on the `variable' to the `solver'."
  (%cl-simplex-solver-add-strong-stay solver 
			       (variable-for-designator variable)
			       (coerce weight 'double-float)))

(defun solver-add-edit (solver variable weight)
  "Adds a variable for editing with given weight."
  (%cl-simplex-solver-add-edit-var solver
				   (variable-for-designator variable)
				   (coerce weight 'double-float)))

(defun solver-add-weak-edit (solver variable weight)
  "Adds a variable for editing with given weight."
  (%cl-simplex-solver-add-edit-var-weak solver
				   (variable-for-designator variable)
				   (coerce weight 'double-float)))

(defun solver-add-strong-edit (solver variable weight)
  "Adds a variable for editing with given weight."
  (%cl-simplex-solver-add-edit-var-strong solver
				   (variable-for-designator variable)
				   (coerce weight 'double-float)))

(defun solver-suggest-value (solver variable value)
  "Sets the edit value for the solver of variable to value."
  (%cl-simplex-solver-suggest-value  solver
				     (variable-for-designator variable)
				     (coerce value 'double-float)))

(defun solver-set-edited-value (solver variable value)
  "Sets the edit value for the solver of variable to value."
  (%cl-simplex-solver-set-edited-value  solver
					(variable-for-designator variable)
					(coerce value 'double-float)))

(defun solver-set-changed-callback (solver callback-fn)
  "Sets the solver callback function to `callback-fn' 
This function should take two arguments, the solver and the variable."
  (setf *variable-changed-callback* callback-fn)
  (%cl-solver-set-changed-clv-callback solver (cffi:callback variable-changed)))

(defun constraint-add (constraint solver)
  "Adds `constraint' to the `solver'"
  (%cl-add-constraint solver constraint))

(defun constraint-remove (constraint solver)
  "Remove `constraint' from `solver'"
  (%cl-remove-constraint solver constraint))

(defun constraint-parse (constraint strength &optional &key solver)
  "Parses the string `constraint' to create a constraint object.
The syntax of the constraint is described in the file ClReader.y.

`strength' should be string which is one of the following:
 - \"required\"
 - \"strong\"   = (1,0,0)
 - \"medium\"   = (0,1,0)
 - \"weak\"     = (0,0,1)
 - \"(n1,n2,n3)\"  with n1, n2, and n3 the triple of weights

If the optional keyword argument `solver' is given the
constraint is added to the solver.

Note 1: that the constraint should only refer to variables which are 
        created before with variable-new.

Note 2: The < and > comparison operators are not supported and silently
        ignored."
  (let ((constraint (%cl-parse-constraint constraint strength)))
    (when solver (constraint-add constraint solver))
    constraint))

(defun constraint-satisfied-p (constraint)
  "Returns 't' if the constraint is satisfied."
  (not (= 0 (%cl-f-is-satisfied constraint))))
