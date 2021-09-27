#lang racket
(require "opcodes.rkt")
(provide make-stack-machine)
(provide run-stack-machine)
(provide get-stack)
(provide get-varnames)
(provide get-consts)
(provide get-names)
(provide get-code)
(provide get-IC)
(provide empty-stack)
(provide make-stack)
(provide push)
(provide pop)
(provide top)


;; TODO 1:
;; Alegeți metoda de reprezentarea a unei stive.
;; Implementați:
(define mystack '())
(define empty-stack mystack)
(define (make-stack) empty-stack)

(define (push element stack) (cons element stack))
(define (top stack) (car stack))
(define (pop stack) (cdr stack))

;; TODO 2:
;; Alegeți metoda de reprezentare a unei mașini stivă.
;; Definiți make-stack-machine, acesta trebuie sa primeasca cele 4 segmente de date
;; Veți avea nevoie de o stivă pentru execuție și un counter ca să stiți
;; la ce instrucțiune sunteți.
(define (make-stack-machine stack co-varnames co-consts co-names co-code IC)
  (list stack co-varnames co-consts co-names co-code IC))

;; Definiți funcțiile `get-varnames`, `get-consts`, `get-names`,
;; `get-code`, `get-stack`, `get-IC` care primesc o mașina stivă și întorc
;; componenta respectivă

;; ex:
;; > (get-varnames (make-stack-machine empty-stack 'dummy-co-varnames (hash) (hash) (list) 0))
;; 'dummy-co-varnames
(define (get-varnames stack-machine) (car (cdr stack-machine)))

;; ex:
;; > (get-consts (make-stack-machine empty-stack (hash) 'dummy-co-consts (hash) (list) 0))
;; 'dummy-co-consts
(define (get-consts stack-machine) (car (cdr(cdr stack-machine))))

;; ex:
;; > (get-names (make-stack-machine empty-stack (hash) (hash) 'dummy-co-names (list) 0))
;; 'dummy-co-names
(define (get-names stack-machine) (car(cdr (cdr(cdr stack-machine)))))

;; ex:
;; > (get-code (make-stack-machine empty-stack (hash) (hash) (hash) 'dummy-co-code 0))
;; dummy-co-code
(define (get-code stack-machine) (car(cdr (cdr(cdr(cdr stack-machine))))))

;; Întoarce stiva de execuție.
;; ex:
;; > (get-code (make-stack-machine 'dummy-exec-stack (hash) (hash) (hash) (list) 0))
;; dummy-exec-stack
(define (get-stack stack-machine) (car stack-machine))

;; Întoarce instruction counterul.
;; ex:
;; > (get-code (make-stack-machine empty-stack (hash) (hash) (hash) (list) 0))
;; 0
(define (get-IC stack-machine) (car(cdr(cdr(cdr(cdr(cdr stack-machine)))))))



(define symbols (list 'STACK 'CO-VARNAMES 'CO-CONSTS 'CO-NAMES 'CO-CODE 'INSTRUCTION-COUNTER))

;; TODO 3:
;; Definiți funcția get-symbol-index care gasește index-ul simbolului in listă.
(define (get-symbol-index symbol)
 (let loop ((symbols symbols)
            (index 0))
   (cond ((equal? (first symbols) symbol) index)
         (else (loop (rest symbols) (add1 index))))))

;; Definiți funcția update-stack-machine care intoarce o noua mașina stivă
;; înlocuind componenta corespondentă simbolului cu item-ul dat în paremetri.
;; > (get-varnames (update-stack-machine "new-varnames" 'CO-VARNAMES stack-machine))
;; "new-varnames"
;; > (get-varnames (update-stack-machine "new-names" 'CO-NAMES stack-machine))
;; "new-names"
(define (update-stack-machine item symbol stack-machine)
  (define idx (get-symbol-index symbol))
  (cond
    ((= idx 1) (make-stack-machine item
                      (get-varnames stack-machine)
                      (get-consts stack-machine)
                      (get-names stack-machine)
                      (get-code stack-machine)
                      (get-IC stack-machine)))
    ((= idx 2) (make-stack-machine (get-stack stack-machine)
                      item
                      (get-consts stack-machine)
                      (get-names stack-machine)
                      (get-code stack-machine)
                      (get-IC stack-machine)))
    ((= idx 3) (make-stack-machine (get-stack stack-machine)
                      (get-varnames stack-machine)
                      item
                      (get-names stack-machine)
                      (get-code stack-machine)
                      (get-IC stack-machine)))
    ((= idx 4) (make-stack-machine (get-stack stack-machine)
                      (get-varnames stack-machine)
                      (get-consts stack-machine)
                      item
                      (get-code stack-machine)
                      (get-IC stack-machine)))
    ((= idx 5) (make-stack-machine (get-stack stack-machine)
                      (get-varnames stack-machine)
                      (get-consts stack-machine)
                      (get-names stack-machine)
                      item
                      (get-IC stack-machine)))
    ((= idx 6) (make-stack-machine (get-stack stack-machine)
                      (get-varnames stack-machine)
                      (get-consts stack-machine)
                      (get-names stack-machine)
                      (get-code stack-machine)
                      item))))
    
;; Definiți funcția push-exec-stack care primește o masină stivă și o valoare
;; și intoarce o noua mașina unde valoarea este pusă pe stiva de execuție
(define (push-exec-stack value stack-machine)
  (make-stack-machine (push value (get-stack stack-machine))
                      (get-varnames stack-machine)
                      (get-consts stack-machine)
                      (get-names stack-machine)
                      (get-code stack-machine)
                      (get-IC stack-machine)))

;;  Definiți funcția pop-exec-stack care primește o masină stivă
;;  și intoarce o noua mașina aplicând pop pe stiva de execuție.
(define (pop-exec-stack stack-machine)
  (make-stack-machine (pop (get-stack stack-machine))
                      (get-varnames stack-machine)
                      (get-consts stack-machine)
                      (get-names stack-machine)
                      (get-code stack-machine)
                      (get-IC stack-machine)))

;; TODO 4:

(define (helper f a b step)
  (let fun ( (a a) (result '()))
    (if (> a b)
        result
        (fun (+ a step) (cons (f a)  result)))))

;(define symbols (list 'STACK 'CO-VARNAMES 'CO-CONSTS 'CO-NAMES 'CO-CODE 'INSTRUCTION-COUNTER))

;; Definiți funcția run-stack-machine care execută operații pană epuizează co-code.
(define (run-stack-machine stack-machine)
  (define name_f (car(car (get-code stack-machine))))
  (define argument (cdr (car (get-code stack-machine))))
                 (if(null? (get-code stack-machine))
                     (make-stack-machine
                      (get-stack stack-machine)
                      (get-varnames stack-machine)
                      (get-consts stack-machine)
                      (get-names stack-machine)
                      (get-code stack-machine)
                      (get-IC stack-machine))
                     ;(if(equal? name_f 'LOAD_CONST)
                      (if(equal? name_f 100)
                         (run-stack-machine (make-stack-machine
                                        (push (hash-ref (get-consts stack-machine) argument) (get-stack stack-machine))
                                        (get-varnames stack-machine)
                                        (hash-set (get-consts stack-machine) argument (hash-ref (get-consts stack-machine) 0))
                                        (get-names stack-machine)
                                        (cdr (get-code stack-machine))
                                        (+ 1 (get-IC stack-machine))))
                         (if(equal? name_f 125);'STORE_FAST)
                            (run-stack-machine(make-stack-machine
                                       (pop (get-stack stack-machine))
                                       (hash-set (get-varnames stack-machine) argument (top (get-stack stack-machine)))
                                       (get-consts stack-machine)
                                       (get-names stack-machine)
                                       (cdr(get-code stack-machine))
                                       (+ 1 (get-IC stack-machine))))
                           ; (if(equal? name_f 'RETURN_VALUE)
                            (make-stack-machine
                             (get-stack stack-machine)
                             (get-varnames stack-machine)
                             (get-consts stack-machine)
                             (get-names stack-machine)
                             (get-code stack-machine)
                             (get-IC stack-machine))))))
#|      (let iter
        ((name_f (car(car (get-code stack-machine))))
        (argument (cdr (car (get-code stack-machine)))))
                 ((if(equal? (length (get-code stack-machine)) (get-IC stack-machine))
                     (make-stack-machine
                      (get-stack stack-machine)
                      (get-varnames stack-machine)
                      (get-consts stack-machine)
                      (get-names stack-machine)
                      (get-code stack-machine)
                      (get-IC stack-machine))
                     (if(equal? name_f 'LOAD_CONST)
                         (run-stack-machine (make-stack-machine
                                        (push (hash-ref (get-consts stack-machine) argument) (get-stack stack-machine))
                                        (get-varnames stack-machine)
                                        (get-consts stack-machine)
                                        (get-names stack-machine)
                                        (get-code stack-machine)
                                        (+ 1 (get-IC stack-machine))))
                         (if(equal? name_f 'STORE_FAST)
                            (run-stack-machine(make-stack-machine
                                       (pop (get-stack stack-machine))
                                       (hash-set (get-varnames stack-machine) argument (top (get-stack stack-machine)))
                                       (get-consts stack-machine)
                                       (get-names stack-machine)
                                       (cdr(get-code stack-machine))
                                       (+ 1 (get-IC stack-machine))))
                           ; (if(equal? name_f 'RETURN_VALUE)
                            (make-stack-machine
                             (get-stack stack-machine)
                             (get-varnames stack-machine)
                             (get-consts stack-machine)
                             (get-names stack-machine)
                             (get-code stack-machine)
                             (get-IC stack-machine))))))))

|#
                         

                    
 
                  
                
              
