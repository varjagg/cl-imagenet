(in-package #:cl-imagenet)

(defparameter *imagenet-image-root* "/home/eugene/datasets/imagenet/images")
(defparameter *imagenet-annotations-root-set* "/home/eugene/datasets/imagenet/annotations/*")

(defparameter *queue-lock* (bt:make-lock))
(defparameter *counters-lock* (bt:make-lock))
(defparameter *missing-count* 0)
(defparameter *processed-count* 0)
(defparameter *unreadable-count* 0)

(defparameter *abort* nil)

(defparameter *display-channel* (trivial-channels:make-channel))

(defconstant +sc-nprocessors-onln+ 84)
(defconstant +default-processor-count+ 4)

(defconstant +pane-h+ 60)
(defconstant +pane-w+ 80)

#+cffi(cffi:defcfun "sysconf" :long (name :int)) ; courtesy _Common_Lisp_Recipes_ by Edi Weitz
(defun get-number-of-processors ()
  (or #+cffi(sysconf +sc-nprocessors-onln+) +default-processor-count+))

(defmethod run-display (width height)
  (bt:make-thread 
   #'(lambda ()
       (let* ((display (xlib:open-default-display))
		(window (xlib:create-window :parent (xlib:screen-root (xlib:display-default-screen display))
					    :x 0 :y 0 :width width :height height
					    :event-mask '(:key-press)))
		(gc (xlib:create-gcontext :drawable window))
		(pixmap (xlib:create-pixmap :width width :height height :depth 24 :drawable window))
		(pixmap-gc (xlib:create-gcontext :drawable pixmap))
		(buffer (make-array `(,height ,width) :element-type 'xlib:pixel))
		(image (xlib:create-image  :data buffer :depth 24
					   :height height :width width)))
	   (unwind-protect
		(progn
		  (setf (xlib:wm-name window) "Processed Images")
		  (xlib:map-window window)
		  (xlib:clear-area window width height)
		  (loop for src = (opticl:fit-image-into (trivial-channels:recvmsg *display-channel*) :y-max +pane-h+ :x-max +pane-w+)
		     with quit = nil until quit 
		     for xp = (random (/ width +pane-w+))
		     for yp = (random (/ height +pane-h+)) do
		       (opticl:with-image-bounds (h w) src
			 (loop for i from 0 repeat h do
			      (loop for j from 0 repeat w
				 for spos = (* 3 (+ j (* width i))) do
				   (setf (aref buffer (+ i (* yp +pane-h+)) (+ j (* xp +pane-w+)))
					 (logior (ash (opticl:pixel src i j) 16) (ash (opticl:pixel src i j) 8) (opticl:pixel src i j)))))
			 (xlib:put-image pixmap pixmap-gc image :width w :height h :x 0 :y 0)
			 (xlib:copy-area pixmap gc 0 0 w h window 0 0))
		       (xlib:display-force-output display)
		       (xlib:event-case (display :timeout 0)
			 (:resize-request ()
					  t)
			 (:exposure ()
				    t)
			 (:key-press (window code)
				     (case (xlib:keysym->character
					    display
					    (xlib:keycode->keysym display code 0))
				       (#\q
					(setf quit t)))
				     t))))
	     (xlib:free-pixmap pixmap)
	     (xlib:free-gcontext gc)
	     (xlib:close-display display))))))

(defclass work-instance ()
  ((anno-name :accessor anno-name :initarg :anno-name)
   (buffer :accessor buffer :initarg :buffer :initform nil)
   (jpeg-descriptor :accessor jpeg-descriptor :initform (jpeg::make-descriptor))
   (luminance :accessor luminance :type opticl:8-bit-gray-image)
   (anno-plist :accessor anno-plist)))

(defun process (image bbox-annotation)
  ;; edge-detect and process
  (trivial-channels:sendmsg *display-channel* (opticl:edge-detect-image image)))

(defun read-imagenet-annotation-file (pathname)
  "Returns plist formed from parsing an annotation XML file"
  (let* ((document (cxml:parse-file pathname (cxml-dom:make-dom-builder)))
	 (filename (slot-value (dom:first-child (aref (dom:get-elements-by-tag-name document "filename") 0)) 'rune-dom::value))
	 (folder (slot-value (dom:first-child (aref (dom:get-elements-by-tag-name document "folder") 0)) 'rune-dom::value))
	 (objects (dom:get-elements-by-tag-name document "object")))
    `(filename ,filename folder ,folder boxes
	       ,(loop for o across objects
		    for name = (slot-value (dom:first-child (aref (dom:get-elements-by-tag-name o "name") 0)) 'rune-dom::value)
		    for xmin = (slot-value (dom:first-child (aref (dom:get-elements-by-tag-name o "xmin") 0)) 'rune-dom::value)
		    for xmax = (slot-value (dom:first-child (aref (dom:get-elements-by-tag-name o "xmax") 0)) 'rune-dom::value)
		    for ymin = (slot-value (dom:first-child (aref (dom:get-elements-by-tag-name o "ymin") 0)) 'rune-dom::value)
		    for ymax = (slot-value (dom:first-child (aref (dom:get-elements-by-tag-name o "ymax") 0)) 'rune-dom::value)
		    collecting `(name ,name xmin ,(parse-integer xmin) ymin ,(parse-integer ymin)
				      xmax ,(parse-integer xmax) ymax ,(parse-integer ymax))))))

(defun height (array)
  (array-dimension array 0))

(defun width (array)
  (array-dimension array 1))

(defmethod process-image ((work work-instance))
  (let* ((image-pathname (make-pathname :directory `(:absolute ,*imagenet-image-root* ,(getf (anno-plist work) 'folder))
					:name (getf (anno-plist work) 'filename) :type "JPEG")))
    (if (probe-file image-pathname)
	(handler-case 
	    (multiple-value-bind (h w ncomp)
		(jpeg:jpeg-file-dimensions image-pathname)
	      ;; grow the buffer if necessary
	      (when (> (* h w ncomp) (length (buffer work)))
		(setf (buffer work) (jpeg:allocate-buffer h w ncomp)))
	      (unless (and (slot-boundp work 'luminance) (= h (height (luminance work)))
			   (= w (width (luminance work))))
		(setf (luminance work) (opticl:make-8-bit-gray-image h w)))
	      ;; obtain luminance component from the jpeg file
	      (multiple-value-bind (buffer height width ncomp)
		  (jpeg:decode-image image-pathname :buffer (buffer work) :colorspace-conversion nil :cached-source-p t)
		(loop for i below height do
		     (loop for j below width do
			  (let ((pixoff (* ncomp (+ (* i width) j))))
			    (setf (opticl:pixel (luminance work) i j)
				  (values (aref buffer pixoff)))))))
	      (loop for bbox in (getf (anno-plist work) 'boxes)
		 for xmin = (getf bbox 'xmin)
		 for ymin = (getf bbox 'ymin)
		 for ymax = (getf bbox 'ymax)
		 for xmax = (getf bbox 'xmax)
		 if (and (< -1 xmin xmax w) (< -1 ymin ymax h)) do
		 ;; process the boundbox region
		   (process (opticl:crop-image (luminance work) ymin xmin ymax xmax) bbox)
		   (bt:with-lock-held (*counters-lock*)
		     (incf *processed-count*))
		 else do
		   (format t "Invalid bounding box for ~A~%" image-pathname)))
	  (jpeg:jpeg-decoder-error () (progn (bt:with-lock-held (*counters-lock*) (incf *unreadable-count*))
					     (return-from process-image nil))))
	(bt:with-lock-held (*counters-lock*)
	  (incf *missing-count*)
	  #+nil(format t "Missing image file ~A~%" image-pathname)))))
    
(defmethod process-from-annotation ((work work-instance))
  (setf (anno-plist work) (read-imagenet-annotation-file (anno-name work)))
  (process-image work))

(defun train-from-annotations (&optional (spec *imagenet-annotations-root-set*))
  (setf *missing-count* 0
	*processed-count* 0
	*unreadable-count* 0)
  (let ((annodirs (directory spec)))
    (run-display 800 600)
    (loop repeat (get-number-of-processors)
       do (funcall;;(bt:make-thread
	   #'(lambda ()
	       (let ((work (make-instance 'work-instance)))
		 (loop for taskdir = (bt:with-lock-held (*queue-lock*)
				       (pop annodirs))
		    for taskfiles = '()
		    while (and taskdir (not *abort*)) do
		      (cl-fad:walk-directory (namestring taskdir)
					     #'(lambda (path)
						 (when (string-equal (pathname-type path) "xml")
						   (push path taskfiles))))
		      (loop for taskfile in taskfiles do
			   (setf (anno-name work) taskfile)
			   (process-from-annotation work)))))))))
