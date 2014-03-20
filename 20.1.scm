(define frozen)
(append '(the call/cc returned)
	(list (call-with-current-continuation
	       (lambda (cc)
		 (set! frozen cc)
		 'a))))
(frozen 'again)
(frozen 'thrice)
(+ 1 (frozen 'safely))
