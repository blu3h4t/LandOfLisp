(defparameter *nodes* 
'((living-room (you are in the living-room.
a wizard is snoring loudly on the couch.))
(garden (you are in a beautiful garden.
there is a well in the front of you.))
(attic (you are in the attic.
there is a giant welding torch in the corner.))))

(assoc 'garden *nodes*)

(defun describe-location (location nodes)
(cadr (assoc location nodes)))

(describe-location 'living-room *nodes*)

(defparameter *edges* 
'((living-room (garden west door)
(attic upstairs ladder))
(garden (living-room east door))
(attic (living-room downstairs ladder))))

(defun describe-path (edge)
`(there is a ,(caddr edge) going ,(cadr edge) from here.))

(describe-path '(garden west door))

(defun describe-paths (location edges)
(apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

(describe-paths 'living-room *edges*)

(cdr (assoc 'living-room *edges*))

(mapcar #'describe-path '((GARDEN WEST DOOR) (ATTIC UPSTAIRS LADDER)))

(apply #'append '((THERE IS A DOOR GOING WEST FROM HERE.)
(THERE IS A LADDER GOING UPSTAIRS FROM HERE.)))

(defun describe-paths (location edges)
(apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

(defparameter *objects* '(whiskey bucket frog chain))

(defparameter *object-locations*
'((whiskey living-room)
(bucket living-room)
(chain garden)
(frog garden)))

(defun objects-at (loc objs obj-locs)
(labels ((at-loc-p (obj)
(eq (cadr (assoc obj obj-locs)) loc)))
(remove-if-not #'at-loc-p objs)))

(objects-at 'living-room *objects* *object-locations*)

(defun describe-objects (loc objs obj-loc)
(labels ((describe-obj (obj)
`(you see a ,obj on the floor.)))
(apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc)))))

(describe-objects 'living-room *objects* *object-locations*)

(defparameter *location* 'living-room)

(defun look()
(append (describe-location *location* *nodes*)
(describe-paths *location* *edges*)
(describe-objects *location* *objects* *object-locations*)))
(look)

(defun walk (direction)
(let ((next (find direction
(cdr (assoc *location* *edges*))
:key #'cadr)))
(if next
(progn (setf *location* (car next))
(look))
'(you cannot go that way.))))
