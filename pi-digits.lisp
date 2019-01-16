(defconstant +n+ "1000")

(require 'sb-gmp)

(defun pi-digits (n)
  (declare (optimize (speed 3) (debug 0) (space 0) (safety 0)))
  (let ((k 1)
        (i 0)
        k2 u v
        (w 0)
        (n1 4)
        (n2 3)
        (d 1))
    (loop
       (setf u (sb-gmp:mpz-tdiv n1 d)
             v (sb-gmp:mpz-tdiv n2 d))
       (if (= u v)
           (progn
             (format t "~d" u)
             (incf i)
             (if (= (mod i 10) 0)
                 (format t "~T:~d~%" i))
             (if (= i n)
                 (return))
             (setf u (sb-gmp:mpz-mul u -10)
                   u (sb-gmp:mpz-mul d u)
                   n1 (sb-gmp:mpz-mul n1 10)
                   n1 (sb-gmp:mpz-add n1 u)
                   n2 (sb-gmp:mpz-mul n2 10)
                   n2 (sb-gmp:mpz-add n2 u)))
           (progn
             (setf k2 (* k 2)
                   u (sb-gmp:mpz-mul n1 (- k2 1))
                   v (sb-gmp:mpz-add n2 n2)
                   w (sb-gmp:mpz-mul n1 (- k 1))
                   n1 (sb-gmp:mpz-add u v)
                   u (sb-gmp:mpz-mul n2 (+ k 2))
                   n2 (sb-gmp:mpz-add w u)
                   d (sb-gmp:mpz-mul d (+ k2 1)))
             (incf k))))
    (if (not (= (mod i 10) 0))
        (progn (format t "~v@{~A~:*~}~T" (- 10 (mod n 10)) " ")
               (format t ":~d" n)))))

(defun main (&optional
	     (n (parse-integer
                 (or (car (last sb-ext:*posix-argv*))
                     +n+))))
  (pi-digits n))

(main 100)
