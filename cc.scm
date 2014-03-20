(define frozen)                  ;; 全局变量，为了以后随处可以引用（进入）延续
(append
 '(the call/cc returned)
 (list
  (call/cc                       ;; (call/cc (lambda (cc) 这些可以当成是延续的语法糖
           (lambda (cc)          ;; 延续
             (set! frozen cc)    ;; 在此处设置一个延续；运算到此处，append等着list，list等着设置点后的计算
             'a))))              ;; 计算本身！
;;=> 第一次调用返回(the call/cc returned a)

(frozen 'again)                  ;; 进入延续（记住当时的延续，append在等待list，list在等待延续点后的计算），并用'again取代延续点后的计算。
;;=> (the call/cc returned again)
(+ 1 (frozen 'really?))              ;; 本来在此处+等着frozen的返回，它自己好继续计算。但是延续调用后，此处的+的等待被忘记了。
;;=> (the call/cc returned really?)
(define froz1)
(define froz2)
(let ((x 0))
  (call/cc
   (lambda (cc)
     (set! froz1 cc)
     (set! froz2 cc)))
  (set! x (1+ x))
  x)

(define t1  '(a (b (d h)) (c e (f i) g)))
;; 
;;                  a
;;                /   \
;;               /     \
;;              /       \
;;             b         c
;;             |       / | \
;;             |      /  |  \
;;             |     /   |   \
;;             d    e    f    g
;;             |         |
;;             |         |
;;             |         |
;;             h         i
;; 
(define t2 '(1 (2 (3 6 7) 4 6)))

;; 
;;                  1
;;                  |                         
;;                  |
;;                  |
;;                  2
;;                / | \
;;               /  |  \
;;              /   |   \
;;             3    4    5
;;            / \
;;           /   \
;;          /     \
;;         6       7

(define (dft tree)
  (cond 
    ((null? tree) '())
    ((not (pair? tree)) (write tree))
    (else (dft (car tree))
          (dft (cdr tree)))))

(define *saved* '())
(define (dft-node tree)
  (cond
    ((null? tree) (restart))
    ((not (pair? tree)) tree)
    (else (call/cc
            (lambda (cc)
              (set! *saved*
                (cons (lambda ()
                        (cc (dft-node (cdr tree))))
                      *saved*))
              (dft-node (car tree)))))))

(define (restart)
  (if (null? *saved*)
      'done
      (let ((cont (car *saved*)))
        (set! *saved* (cdr *saved*))
        (cont))))

(define (dft2 tree)
  (set! *saved* '())
  (let ((node (dft-node tree)))
    (cond
      ((eq? node 'done) '())
      (else (write node)
            (restart)))))
