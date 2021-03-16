(define (split-at lst n)
  (cond ((null? (cdr lst)) (cons lst nil))
        ((eq? n 1) (cons (cons (car lst) nil) (cdr lst)))
        ((eq? n 0) (cons '() lst))
        (else (cons (cons (car lst) (car (split-at (cdr lst) (- n 1)))) (cdr (split-at (cdr lst) (- n 1))))))
)


(define (compose-all funcs)
  (lambda (n) (cond ((null? funcs) n)
                    (else ((compose-all (cdr funcs)) ((car funcs) n)))))
)
