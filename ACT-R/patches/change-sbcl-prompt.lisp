#+:sbcl (setf sb-int::*repl-prompt-fun* (lambda (s) (format s "~%? ")))