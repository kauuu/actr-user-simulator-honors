(setf (logical-pathname-translations "ACT-R")
  `(("**;*.*" ,(namestring (merge-pathnames "**/*.*" *load-truename*)))))

(setf (logical-pathname-translations "ACT-R-support")
  `(("**;*.*" ,(namestring (merge-pathnames "**/*.*" (translate-logical-pathname "ACT-R:support;"))))))

(defun load-patch-files ()
  (let ((d (directory (translate-logical-pathname "ACT-R:patches;*.lisp"))))
    (when d
      (format t "~%######### Loading patch files #########~%")
      (dolist (file (sort d 'string< :key (lambda (x) (string (pathname-name x)))))
        (format t "  Loading: ~s~%" file)
        (compile-and-load file))
      (format t "~%######### Patch files loaded #########~%"))))

(defun load-user-files ()
  (let ((d (directory (translate-logical-pathname "ACT-R:user-loads;*.lisp"))))
    (when d
      (format t "~%######### Loading user files #########~%")
      (dolist (file (sort d 'string< :key (lambda (x) (string (pathname-name x)))))
        (format t "  Loading: ~s~%" file)
        (compile-and-load file))
      (format t "~%######### User files loaded #########~%"))))

