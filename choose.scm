(define *paths* '())
(define failsym '@)
(define (choose choices)
  (if (null? choices)
      (fail)
      (call-with-current-continuation
       (lambda (cc)
	 (set! *paths*
	       (cons (lambda ()
		       (cc (choose (cdr choices))))
		     *paths*))
	 (cdr choices)))))
(define fail)
(call-with-current-continuation
 (lambda (cc)
   (set! fail
	 (lambda ()
	   (if (null? *paths*)
	       (cc failsym)
	       (let ((p1 (car *paths*)))
		 (set! *paths* (cdr *paths*))
		 (p1)))))))
