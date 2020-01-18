(defpackage :worlds
  (:use :cl)
  (:export #:sprout
           #:with-world
           #:commit
           #:world-class
           #:conflict))
(in-package :worlds)

(defclass world ()
  ((reads :initform (trivial-garbage:make-weak-hash-table :test 'eq
                                                          :weakness :key)
          :reader world-reads)
   (writes :initform (trivial-garbage:make-weak-hash-table :test 'eq
                                                           :weakness :key)
           :reader world-writes)
   (lock :initform (bt:make-lock "WORLD lock")
         :reader world-lock)
   (parent :initarg :parent
           :initform nil
           :reader world-parent)))

(defclass world-class (standard-class)
  ()
  (:documentation "Instances of classes of this metaclass have slots whose values are logged and committed by the worlds protocol defined by this package."))
(defmethod closer-mop:validate-superclass
    ((class world-class) (superclass standard-class))
  t)

(defvar *current-world*
  (make-instance 'world))
(defvar *unbound* (make-symbol ".UNBOUND."))

(declaim (inline puthash2 gethash2))
(defun puthash2 (key1 key2 hash-table value)
  (multiple-value-bind (hash-table2 present?)
      (gethash key1 hash-table)
    (unless present?
      (let ((new-table (make-hash-table :test 'eq)))
        (setf hash-table2 new-table
              (gethash key1 hash-table) new-table)))
    (setf (gethash key2 hash-table2) value)))
(defun gethash2 (key1 key2 hash-table)
  (multiple-value-bind (hash-table2 present?)
      (gethash key1 hash-table)
    (if present?
        (gethash key2 hash-table2)
        (values nil nil))))

(defmethod (setf closer-mop:slot-value-using-class) (new-value (class world-class) object slotd)
  (puthash2 object slotd (world-writes *current-world*) new-value))

(defmethod closer-mop:slot-value-using-class ((class world-class) object slotd)
  (let ((value? nil)
        value toplevel-world?)
    (loop for world = *current-world* then (world-parent *current-world*)
          for toplevel? = t then nil
          until (null world)
          do (flet ((get-out (present? slot-value)
                      (when present?
                        (setf value? t
                              value slot-value
                              toplevel-world? toplevel?)
                        (loop-finish))))
               (multiple-value-bind (slot-value present?)
                   (gethash2 object slotd (world-writes *current-world*))
                 (get-out present? slot-value))
               (multiple-value-bind (slot-value present?)
                   (gethash2 object slotd (world-reads *current-world*))
                 (get-out present? slot-value))))
    (when (or (not value?)
              (eql value *unbound*))
      (slot-unbound class object (closer-mop:slot-definition-name slotd)))
    (unless toplevel-world?
      (puthash2 object slotd (world-reads *current-world*) value))
    value))

(defmethod closer-mop:slot-boundp-using-class ((class world-class) object slotd)
  (handler-case (closer-mop:slot-value-using-class class object slotd)
    (:no-error (value)
      (declare (ignore value))
      t)
    (unbound-slot ()
      nil)))

(defmethod closer-mop:slot-makunbound-using-class ((class world-class) object slotd)
  (setf (closer-mop:slot-value-using-class class object slotd)
        *unbound*))

(defun sprout ()
  (make-instance 'world :parent *current-world*))

(defmacro with-world ((world) &body body)
  "Evaluate BODY with WORLD bound as the current world."
  `(let ((*current-world* ,world))
     ,@body))

(define-condition conflict (error)
  ((object    :initarg :object    :reader conflict-object)
   (slot-name :initarg :slot-name :reader conflict-slot-name)
   (parent    :initarg :parent    :reader conflict-parent)
   (child     :initarg :child     :reader conflict-child)
   (parent-value :initarg :parent-value :reader conflict-parent-value)
   (child-value  :initarg :child-value  :reader conflict-child-value))
  (:report (lambda (c s)
             (format s "~s cannot be committed to ~s because the values of the slot ~s in ~s conflict.
The parent holds the value 
  ~s
whereas the child holds the value
  ~s"
                     (conflict-child c) (conflict-parent c)
                     (conflict-slot-name c) (conflict-object c)
                     (conflict-parent-value c) (conflict-child-value c)))))
  

(defun commit (world)
  "Update the current world with the changes made in WORLD. If there have been other changes made in the current world that would create a conflict between the state of the world when WORLD was created and the changes WORLD creates, a condition of type CONFLICT is signalled."
  (let ((parent-world (world-parent world)))
    (bt:with-lock-held ((world-lock parent-world))
      ;; Check all read slots are the same as the slots in the parent world.
      (maphash (lambda (object slot-table)
                 (maphash (lambda (slotd value)
                            (multiple-value-bind (parent-value value?)
                                (gethash2 object slotd (world-writes parent-world))
                              (unless (or (not value?)
                                          (eql value parent-value))
                                (error 'conflict
                                       :child world :parent parent-world
                                       :object object
                                       :slot-name (closer-mop:slot-definition-name slotd)
                                       :child-value value :parent-value parent-value))))
                          slot-table))
               (world-reads world))
      ;; Propagate all of the information in world's `writes` objects to
      ;; parent-world's `writes` object, overriding the values of any slots
      ;; that have already been assigned into in parent-world.
      (let ((parent-writes (world-writes parent-world)))
        (maphash (lambda (object slot-table)
                   (maphash (lambda (slotd value)
                              (puthash2 object slotd parent-writes value))
                            slot-table))
                 (world-writes world)))
      ;; Propagate all of the information in world's `reads` objects to
      ;; parent-world's `reads` objects, except for the slots that have already
      ;; been read from in parent-world.
      (let ((parent-reads (world-reads parent-world)))
        (maphash (lambda (object slot-table)
                   (maphash (lambda (slotd value)
                              (unless
                                  (nth-value 1
                                             (gethash2 object slotd parent-reads)))
                              (puthash2 object slotd parent-reads value))
                            slot-table))
                 (world-reads world))))
    (clrhash (world-reads world))
    (clrhash (world-writes world))))
