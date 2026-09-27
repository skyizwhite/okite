(defpackage #:okite/layers
  (:use #:cl)
  (:export #:define-layers
           #:find-layers
           #:layer-violations
           #:ensure-layers
           #:violation
           #:violation-kind
           #:violation-file
           #:violation-layer
           #:violation-dependency
           #:violation-dependency-layer
           #:violation-error
           #:violation-error-violations))
(in-package #:okite/layers)

;;; A file of a package-inferred system is a system of its own, and what it
;;; depends on is what its defpackage uses and imports from. ASDF reads that off
;;; the first form of the file without loading it, so the layers are checked
;;; against ASDF and nothing needs to be compiled.

(defvar *layers* (make-hash-table :test 'eq))

(defstruct (definition (:constructor %make-definition))
  name
  system
  (layers '())     ; ((layer pattern ...) ...)
  (allow '())      ; ((layer layer ...) ...)
  (libraries '())  ; (("name" layer ...) ...)
  (anywhere '())   ; ("name" ...)
  (forbid '())     ; ("name" ...)
  (ignore '()))    ; (pattern ...)

(defun definition-error (name control &rest args)
  (error "In the layers ~s: ~?" name control args))

(defun make-definition (name clauses)
  (let ((definition (%make-definition :name name
                                      :system (string-downcase (symbol-name name)))))
    (dolist (clause clauses)
      (unless (consp clause)
        (definition-error name "~s is not a clause" clause))
      (destructuring-bind (key &rest body) clause
        (case key
          (:system (setf (definition-system definition) (string-downcase (string (first body)))))
          (:layers (setf (definition-layers definition) body))
          (:allow (setf (definition-allow definition) body))
          (:libraries (setf (definition-libraries definition)
                            (mapcar (lambda (entry)
                                      (if (and (consp entry) (stringp (first entry)))
                                          (cons (string-downcase (first entry)) (rest entry))
                                          entry))
                                    body)))
          (:anywhere (setf (definition-anywhere definition)
                           (mapcar (lambda (name) (if (stringp name) (string-downcase name) name))
                                   body)))
          (:forbid (setf (definition-forbid definition) (mapcar #'string-downcase body)))
          (:ignore (setf (definition-ignore definition) body))
          (t (definition-error name "unknown clause ~s" key)))))
    (validate definition)
    definition))

(defun validate (definition)
  (let* ((name (definition-name definition))
         (layers (mapcar #'first (definition-layers definition)))
         (patterns (mapcan (lambda (layer) (copy-list (rest layer))) (definition-layers definition))))
    (flet ((known (layer where)
             (unless (member layer layers)
               (definition-error name "~s in ~s is not a layer" layer where))))
      (unless layers
        (definition-error name "no (:layers ...)"))
      (loop :for (layer . more) :on layers
            :when (member layer more)
              :do (definition-error name "the layer ~s is declared twice" layer))
      (dolist (layer (definition-layers definition))
        (unless (and (keywordp (first layer)) (rest layer) (every #'pattern-p (rest layer)))
          (definition-error name "~s is not (:layer \"pattern\" ...)" layer)))
      (dolist (pattern (definition-ignore definition))
        (unless (pattern-p pattern)
          (definition-error name "~s in :ignore is not a pattern" pattern)))
      (loop :for (pattern . more) :on patterns
            :when (member pattern more :test #'string=)
              :do (definition-error name "the pattern ~s is in more than one layer" pattern))
      (dolist (entry (definition-allow definition))
        (dolist (layer entry)
          (known layer :allow)))
      (loop :for (entry . more) :on (definition-allow definition)
            :when (assoc (first entry) more)
              :do (definition-error name "~s is given twice in :allow" (first entry)))
      (dolist (entry (definition-libraries definition))
        (unless (and (consp entry) (stringp (first entry)))
          (definition-error name "~s in :libraries does not start with a library's name" entry))
        (unless (rest entry)
          (definition-error name "~s in :libraries gives no layer; a library no layer may use is for :forbid"
                            (first entry)))
        (dolist (layer (rest entry))
          (known layer :libraries)))
      (dolist (library (definition-anywhere definition))
        (unless (stringp library)
          (definition-error name "~s in :anywhere is not a library's name" library)))
      (loop :for (library . more) :on (library-names definition)
            :when (member library more :test #'string=)
              :do (definition-error name "the library ~s is listed twice" library)))))

(defun library-names (definition)
  "The names :libraries and :anywhere list, in that order."
  (append (mapcar #'first (definition-libraries definition))
          (definition-anywhere definition)))

(defun pattern-p (pattern)
  (and (stringp pattern) (plusp (length pattern))))

(defmacro define-layers (name &body clauses)
  "Define the layers NAME of a package-inferred system. The clauses:

  (:system \"name\")        the system whose files are checked, when it is not
                          NAME downcased
  (:layers (layer pattern ...) ...)
                          where each file belongs. A pattern ending in / covers
                          every file under that directory, any other pattern
                          one file, both relative to the system's :pathname and
                          without .lisp. The longest pattern that matches wins,
                          and a file no pattern matches is a violation.
  (:allow (layer layer ...) ...)
                          what the first layer may depend on besides itself.
                          Anything not listed is a violation.
  (:libraries (\"name\" layer ...) ...)
                          a system outside this one and the layers that may use
                          it. The name covers its subsystems and extensions:
                          \"lack\" covers lack/request and lack-middleware-session.
  (:anywhere \"name\" ...)  libraries every layer may use, named as in :libraries.
                          A library none of :libraries, :anywhere and :forbid
                          names is a violation, and so is a name in :libraries
                          or :anywhere that covers nothing a file uses.
  (:forbid \"name\" ...)    systems no file may use, with their subsystems.
  (:ignore pattern ...)   files under the system's :pathname that are not part
                          of it, as patterns like those of :layers. Directories
                          whose name starts with a dot are never looked in."
  `(progn
     (setf (gethash ',name *layers*) (make-definition ',name ',clauses))
     ',name))

(defun find-layers (name)
  (or (gethash name *layers*)
      (error "No layers named ~s" name)))

(defun ensure-definition (designator)
  (if (definition-p designator) designator (find-layers designator)))

;;; What ASDF says

(defun prefix-p (prefix string)
  (and (<= (length prefix) (length string))
       (string= prefix string :end2 (length prefix))))

(defun system-files (system)
  "The files of SYSTEM as paths under its :pathname, without .lisp, sorted.
Directories whose name starts with a dot (.qlot, .git) are not walked."
  (let ((found '()))
    (labels ((walk (directory prefix)
               (dolist (file (uiop:directory-files directory "*.lisp"))
                 (push (format nil "~a~a" prefix (pathname-name file)) found))
               (dolist (sub (uiop:subdirectories directory))
                 (let ((name (car (last (pathname-directory sub)))))
                   (unless (char= (char name 0) #\.)
                     (walk sub (format nil "~a~a/" prefix name)))))))
      (walk (asdf:component-pathname (asdf:find-system system)) ""))
    (sort found #'string<)))

(defun dependency-name (dependency)
  "The system a :depends-on entry names: a string, or (:version name ...),
(:feature feature name) or (:require name)."
  (etypecase dependency
    ((or string symbol) (string-downcase (string dependency)))
    (cons (ecase (first dependency)
            (:version (dependency-name (second dependency)))
            (:feature (dependency-name (third dependency)))
            (:require (dependency-name (second dependency)))))))

(defun file-dependencies (system file)
  (remove-duplicates
   (mapcar #'dependency-name
           (asdf:system-depends-on (asdf:find-system (format nil "~a/~a" system file))))
   :test #'string=
   :from-end t))

;;; The layers applied

(defun matches-p (pattern file)
  (if (char= (char pattern (1- (length pattern))) #\/)
      (prefix-p pattern file)
      (string= pattern file)))

(defun ignored-p (definition file)
  (some (lambda (pattern) (matches-p pattern file)) (definition-ignore definition)))

(defun layer-of (definition file)
  (let ((best nil) (best-length -1))
    (loop :for (layer . patterns) :in (definition-layers definition)
          :do (dolist (pattern patterns)
                (when (and (matches-p pattern file)
                           (> (length pattern) best-length))
                  (setf best layer
                        best-length (length pattern)))))
    best))

(defun internal-file (definition dependency)
  "The file of the checked system DEPENDENCY names, if it names one. The system
itself stands for its main file, the one package-inferred systems conventionally
nickname it after."
  (let ((system (definition-system definition)))
    (cond ((string= dependency system) "main")
          ((prefix-p (format nil "~a/" system) dependency)
           (subseq dependency (1+ (length system)))))))

(defun covers-p (name dependency separators)
  (or (string= name dependency)
      (and (< (length name) (length dependency))
           (prefix-p name dependency)
           (member (char dependency (length name)) separators))))

(defun library-layers (definition dependency)
  "The layers that may use DEPENDENCY, :anywhere, or NIL when neither :libraries
nor :anywhere lists it. The longest name that covers it wins, as the longest
pattern does in :layers."
  (let ((best nil))
    (dolist (entry (append (definition-libraries definition)
                           (mapcar (lambda (name) (cons name :anywhere))
                                   (definition-anywhere definition))))
      (when (and (covers-p (first entry) dependency '(#\/ #\-))
                 (or (null best) (> (length (first entry)) (length (first best)))))
        (setf best entry)))
    (rest best)))

(defun forbidden-p (definition dependency)
  (some (lambda (name) (covers-p name dependency '(#\/)))
        (definition-forbid definition)))

(defstruct (violation (:constructor make-violation
                          (kind file layer &optional dependency dependency-layer)))
  "KIND is :unplaced (FILE is in no layer), :layer (DEPENDENCY is in a layer
LAYER may not use), :library (DEPENDENCY is a library LAYER may not use),
:forbidden (DEPENDENCY is forbidden everywhere), :unlisted (DEPENDENCY is a
library nothing lists) or :unused (DEPENDENCY is a name in :libraries or
:anywhere that covers nothing a file uses; FILE and LAYER are NIL)."
  kind file layer dependency dependency-layer)

(defun describe-violation (violation stream)
  (let ((file (violation-file violation))
        (layer (violation-layer violation))
        (dependency (violation-dependency violation)))
    (ecase (violation-kind violation)
      (:unplaced (format stream "~a is in no layer" file))
      (:layer (format stream "~a (~(~a~)) uses ~a (~(~a~))"
                      file layer dependency (violation-dependency-layer violation)))
      (:library (format stream "~a (~(~a~)) uses ~a, which is for ~{~(~a~)~^, ~}"
                        file layer dependency (violation-dependency-layer violation)))
      (:forbidden (format stream "~a uses ~a, which is forbidden" file dependency))
      (:unlisted (format stream "~a (~(~a~)) uses ~a, which is not listed" file layer dependency))
      (:unused (format stream "~s covers nothing a file uses" dependency)))))

(defmethod print-object ((violation violation) stream)
  (if *print-escape*
      (print-unreadable-object (violation stream :type t)
        (describe-violation violation stream))
      (describe-violation violation stream)))

(defun layer-violations (layers)
  "The violations of LAYERS, a name given to DEFINE-LAYERS, by the files of its
system as ASDF sees them now. Files are named as ASDF systems."
  (let* ((definition (ensure-definition layers))
         (system (definition-system definition))
         (libraries '())
         (found '()))
    (dolist (file (remove-if (lambda (file) (ignored-p definition file))
                             (system-files system)))
      (let ((name (format nil "~a/~a" system file))
            (layer (layer-of definition file)))
        (if (null layer)
            (progn
              (push (make-violation :unplaced name nil) found)
              ;; what it uses still counts, or its libraries are reported unused too;
              ;; a file that does not start with a defpackage has nothing ASDF can read
              (dolist (dependency (ignore-errors (file-dependencies system file)))
                (unless (internal-file definition dependency)
                  (push dependency libraries))))
            (dolist (dependency (file-dependencies system file))
              (let ((internal (internal-file definition dependency)))
                (if internal
                    (let ((to (layer-of definition internal)))
                      ;; a file in no layer is reported once, as itself
                      (when (and to
                                 (not (eq to layer))
                                 (not (member to (rest (assoc layer (definition-allow definition))))))
                        (push (make-violation :layer name layer dependency to) found)))
                    (let ((homes (library-layers definition dependency))
                          (forbidden (forbidden-p definition dependency)))
                      (push dependency libraries)
                      (cond (forbidden
                             (push (make-violation :forbidden name layer dependency) found))
                            ((null homes)
                             (push (make-violation :unlisted name layer dependency) found))
                            ((and (listp homes) (not (member layer homes)))
                             (push (make-violation :library name layer dependency homes) found))))))))))
    ;; :forbid is left out: a forbidden name that covers nothing is what it is for
    (dolist (library (library-names definition))
      (unless (some (lambda (dependency) (covers-p library dependency '(#\/ #\-))) libraries)
        (push (make-violation :unused nil nil library) found)))
    (nreverse found)))

(define-condition violation-error (error)
  ((violations :initarg :violations :reader violation-error-violations))
  (:report (lambda (condition stream)
             (format stream "~{~a~^~%~}" (violation-error-violations condition)))))

(defun ensure-layers (layers)
  "Signal VIOLATION-ERROR, carrying every violation, when LAYERS are broken."
  (let ((found (layer-violations layers)))
    (when found
      (error 'violation-error :violations found))))
