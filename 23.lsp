;;;; 23. Practical: A Spam Filter


;;; The Heart of a Spam Filter

; (defpackage :com.gigamonkeys.spam
	; (:use :common-lisp :com.gigamonkeys.pathnames))
	
; (in-package :com.gigamonkeys.spam)

; (load "D:\\quicklisp.lisp")
; (quicklisp-quickstart:install)

(load "C:\\Users\\adebkowski\\quicklisp\\setup.lisp")
(ql:quickload "cl-ppcre")


(defun classify (text)
	(classification (score (extract-features text))))
	
(defparameter *max-ham-score* .4)
(defparameter *min-spam-score* .6)

(defun classification (score)
	(cond
		((<= score *max-ham-score*) 'ham)
		((>= score *min-spam-score*) 'spam)
		(t 'unsure)))
		
(defclass word-feature ()
	((word
		:initarg :word
		:accessor word
		:initform (error "Must supply :word")
		:documentation "The word this feature represents.")
	(spam-count
		:initarg :spam-count
		:accessor spam-count
		:initform 0
		:documentation "Number of spams we have seen this feature in.")
	(ham-count
		:initarg :ham-count
		:accessor ham-count
		:initform 0
		:documentation "Number of hams we have seen this feature in.")))

(defvar *total-spams* 0)
(defvar *total-hams* 0)

(defun make-database ()
	(make-hash-table :test #'equal))
(defvar *feature-database* (make-database))
(defun clear-database ()
	(setf 
		*feature-database* (make-database)
		*total-spams* 0
		*total-hams 0))
			
		
(defun get-word-feature (word)
	(gethash word *feature-database*))
(defun intern-feature (word)
	(or (gethash word *feature-database*)
		(setf (gethash word *feature-database*)
			(make-instance 'word-feature :word word))))
			

(defun extract-words (text)
	(delete-duplicates
		; (list "")
		(cl-ppcre:all-matches-as-strings "[a-zA-Z]{3,}" text)
		:test #'string=))

; how to download and install clisp packages at windows?
; -> https://www.quicklisp.org/beta/
; Done with
; (load "C:\\Users\\adebkowski\\quicklisp\\setup.lisp")
; (ql:quickload "cl-ppcre")
		
(defun extract-features (text)
	(mapcar #'intern-feature (extract-words text)))
			

; (print (extract-words "foo bar baz"))
; (print (extract-words "foo bar baz foo bar"))

; (print (extract-features "foo bar baz foo bar"))

; (print-unreadable-object (object stream-variable &key type identity)
; 	body-form*)

(defmethod print-object ((object word-feature) stream)
	(print-unreadable-object (object stream :type t)
		(with-slots (word ham-count spam-count) object
			(format stream "~s :hams ~d :spams ~d" word ham-count spam-count))))
		
; (print (extract-features "foo bar baz foo bar"))


;;; Training the Filter
(defun train (text type)
	(dolist (feature (extract-features text))
		(increment-count feature type))
	(increment-total-count type))

(defun increment-count (feature type)
	(ecase type
		(ham (incf (ham-count feature)))
		(spam (incf (spam-count feature)))))
;; call example 
;; (increment-count some-feature 'ham)

(defun increment-total-count (type) 
	(ecase type
		(ham (incf *total-hams*))
		(spam (incf *total-spams))))

;; my implement
; (defun do-if-ham-or-spam (type do-ham do-spam)
; 	(ecase type)
; 		(ham (do-ham))
; 		(spam (do-spam)))
; (defun increment-count (feature type)
; 	(do-if-ham-or-spam
; 		(incf (ham-count feature))
; 		(incf (spam-count feature))))
; (defun increment-total-count
; 	(do-if-ham-or-spam
; 		(incf *total-hams*)
; 		(incf *total-spams)))


;;; Per-Word Statistics
(defun spam-probability (feature)
	(with-slots (spam-count ham-count) feature
		(/ spam-count (+ spam-count ham-count))))

(defun spam-probability (feature)
	(with-slots (spam-count ham-count) feature
		(let ((spam-frequency (/ spam-count (max 1 *total-spams*)))
			(ham-frequency (/ ham-count (max 1 *total-hams))))

			(/ spam-frequency (+ spam-frequency ham-frequency)))))
		
		
(defun bayesian-spam-probability (feature &optional (assumed-probability 1/2) (weight 1))
	(let ((basic-probability (spam-probability feature))
			(data-points (+ (spam-count feature) (ham-count feature))))
		(/ (+ 	(* weight assumed-probability)
				(* data-points basic-probability))
			(+ weight data-points))))


;;; Combining Probabilities		
(defun score (features)
	(let ((spam-probs ()) 
			(ham-probs ())
			(number-of-probs 0))
		(dolist (feature features) 
			(unless (untrained-p feature)
				(let ((spam-prob (float (bayesian-spam-probability feature) 0.0d0)))
					(push spam-prob spam-probs)
					(push (- 1.0d0 spam-prob) ham-probs)
					(incf number-of-probs))))
		(let ( 	(h (- 1 (fisher spam-probs number-of-probs)))
				(s (- 1 (fisher ham-probs number-of-probs))))
			(/ (+ (- 1 h) s) 2.0d0))))
		

(defun untrained-p (feature)
	(with-slots (spam-count ham-count) feature
		(and (zerop spam-count) (zerop ham-count))))
		
		
(defun fisher (probs number-of-probs)
	"The Fisher computation described by Robinson."
	(inverse-chi-square
		(* -2 (reduce #'+ probs :key #'log))	; is better instead of (reduce #'* probs)
		(* 2 number-of-probs)))


;;; Inverse Chi Square
(defun inverse-chi-square (value degrees-of-freedom)
	(assert (evenp degrees-of-freedom))
	(min
		(loop with m = (/ value 2)
			for i below (/ degrees-of-freedom 2)
			for prob = (exp (- m)) then (* prob (/ m i))
			summing prob)
		1.0))
		

;;; Training the Filter
; (clear-database)

(train "Make money fast" 'spam)

; (print (classify "Make money fast"))
; (print (classify "Want to go to the movies?"))
		
(defun classification (score)
	(values
		(cond 
			((<= score *max-ham-score*) 'ham)
			((>= score *min-spam-score*) 'spam)
			(t 'unsure))
		score))

(print (classify "Make money fast"))
(print (classify "Want to go to the movies?"))

(train "Do you have any money for the movies?")
(classify "Make money fast")

(classify "Want to go to the movies?")


;;; Testing the Filter
(defun add-file-to-corpus (filename type corpus)
	(vector-push-extend (list filename type) corpus))

(defparameter *corpus* 
	(make-array 1000 :adjustable t :fill-pointer 0))

;; list-directory function from Chapter 15	
(defun add-directory-to-corpus (dir type corpus)
	(dolist (filename (list-directory dir))
		(add-file-to-corpus filename type corpus)))

; (add-directory-to-corpus "mail/spam/" 'spam *corpus*)
; (add-directory-to-corpus "mail/ham/" 'ham *corpus*)

(defun test-classifier (corpus testing-fraction)
	(clear-database)
	(let* ((shuffled (shuffle-vector corpus))
			(size (length corpus))
			(train-on (floor (* size (- 1 testing-fraction)))))
		(train-from-corpus shuffled :start 0 :end train-on)
		(test-from-corpus shuffled :start train-on)))

(defparameter *max-chars* (* 10 1024))

(defun train-from-corpus (corpus &key (start 0) end)
	(loop for idx from start below (or end (length corpus)) do
		(destructing-bind (file type) (aref corpus idx)
			(train (start-of-file file *max-chars*) type))))

(defun test-from-corpus (corpus &key (start 0) end)
	(loop for idx from start below (or end (length corpus)) collect
		(destructing-bind (file type) (aref corpus idx)
			(multiple-value-bind (classification score)
				(classify (start-of-file file *max-chars*))
			(list
				:file file
				:type type
				:classification classification
				:score score)))))













