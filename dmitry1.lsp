;; classic recursive definition of factorial
(defun fact (n) 
 (if (<= n 1) n
  (* n (fact (- n 1)))))

(out (fact 7))

;; tower of hanoi
(defun do_hanoi (height from to temp)
  (if (<= height 0) nil
    (do_hanoi (- height 1) from temp to)
    (out (+ "move " from " --> " to))
    (do_hanoi (- height 1) temp to from)))

(defun hanoi (height) (do_hanoi height 1 3 2))
(hanoi 4)

;; any form can be used as an expression
(+ 5 (progn 1 2 3) (if 1 2 3) (let ((x 1)) (if x x x)))

;; coding closures in lisp even better than coding them in Javascript:
;; if for no other reason then because there are no 'return's:
(defvar myf
   ((lambda (done)
      (lambda ()
         (if done 'done 
           (out "first time!") 
           (setq done t))))))
(myf)
(myf)

;; End of Dmitry's tests.

;; wrapper for out -> console.log (added by Zoltan)
(defun out (text) (console.log text))
