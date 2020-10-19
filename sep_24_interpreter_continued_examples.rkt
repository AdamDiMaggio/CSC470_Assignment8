;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname sep_24_interpreter_continued_examples) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;constructor
(define empty-env
  (lambda () '(empty-env)))

;adds name-value to current environment
(define extend-env
  (lambda (var-name var-value env)
    (list 'extend-env var-name var-value env)))

(define extend-env*
  (lambda (lon lov env)
    (if (null? lon)
        env
        (extend-env* (cdr lon) (cdr lov) (extend-env (car lon) (car lov) env)))))

     
;is this env empty?
(define empty-env?
  (lambda (env)
    (eq? (car env) 'empty-env)))

;is this env not empty?
(define extend-env?
  (lambda (env)
    (eq? (car env) 'extend-env)))

;getters
;return variable name
(define get-var-name
  (lambda (env)
    (car (cdr env))))

;return variable value
(define get-var-value
  (lambda (env)
    (car (cdr (cdr env)))))
;return next environment
(define get-next-env
  (lambda (env)
    (car (cdr (cdr (cdr env))))))

;returns value associated  w/ var-name or #f if not found
(define apply-env
  (lambda (var-name env)
    (cond
      ((empty-env? env) #f)
      ((eq? var-name (get-var-name env)) (get-var-value env))
      (else (apply-env var-name (get-next-env env))))))

;checks if a variable is "bound"(means being used already)
(define has-binding?
  (lambda (var-name env)
    (not (eq? (apply-env var-name env) #f)))) ;if apply-env comes back not false,variable is being used

;test code
(define env (extend-env 'a 5 (extend-env 'b 7 (empty-env))))

(has-binding? 'a env)
(has-binding? 'c env)






;GRAMMAR constructors

(define var-exp
  (lambda (s)
    (list 'var-exp s)))

(define lambda-exp
  (lambda (s lc-exp)
    (list 'lambda-exp s lc-exp)))

(define app-exp
  (lamda (lambda-exp param-value)
         (list 'app-exp lambda-exp param-value)))


;Grammar extractors

(define lc-exp->type
  (lambda (lc-exp)
    (car lc-exp)))

(define var-exp->var-name
  (lambda (var-exp)
    (car (cdr var-exp))))

(define lambda-exp->var-name
  (lambda (lambda-exp)
    ((car (cdr lambda-exp)))))

(define lambda-exp->body
  (lambda (lambda-exp)
    ((car (cdr (cdr lambda-exp))))))

(define app-exp->operator
  (lambda (app-exp)
    (car (cdr app-exp))))

(define app-exp->operand
  (lambda (app-exp)
    (car (cdr (cdr app-exp)))))


;Grammar predicates
(define var-exp?
  (lambda (lc-exp)
    (eq? (car lc-exp) 'var-exp)))

(define lambda-exp?
  (lambda (lc-exp)
    (eq? (car lc-exp) 'lambda-exp)))

(define app-exp?
  (lambda (lc-exp)
    (eq? (car lc-exp) 'app-exp)))





