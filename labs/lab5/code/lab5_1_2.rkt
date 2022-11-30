#lang racket

(display "\n***4.1***\n")
;Функцiя для зчитування чисел написаних користувачем
(define (userInput s)
  (display "Please enter number ")
  (display s)
  (display ": ")
  (define n (read))
  n)
;Додавання двох списків
(define (my-append lis1 lis2)
  (cond ((null? lis1)
         lis2)
        (else
         (cons (car lis1)
               (my-append (cdr lis1) lis2)))))
;Кількість елементів списку
(define (my-length lis)
   (cond ((null? lis)
          0)
         (else
          (+ 1 (my-length (cdr lis))))))
;Чисельник
(define (el1 x) (car x))
;Знаменник
(define (el2 x) (cdr x))
;Створення пари
(define (make-pair n d) (cons n d))
;Функцiя вiдображення дробних чисел
(define(print-pair-decimal x)
  (newline)
  (display (el1 x))
  (display "/")
  (display (el2 x)))   
;Функцiя заповнення списку дробiв користувачем
(define (fillFractionList n lis) 
  (cond
    ((not (= n 0))
     (fillFractionList (- n 1) (my-append lis (list (make-pair (userInput "numerator") (userInput "denominator")))))
     )
    (else
     lis
     )
    ))
;Функцiя вiдображення списку дробiв
(define (printFractionList lis)
  (cond
    ((not (= (my-length lis) 0))
     (print-pair-decimal (list-ref lis 0)) 
        (printFractionList (cdr lis))
     )
    )
)
;Функцiя множення дробiв
(define (multiplicationOfFractions pair1 pair2) =
    (make-pair (* (el1 pair1) (el1 pair2)) (* (el2 pair1) (el2 pair2)))
)
;Функцiя множення пар дробiв у списку
(define (pairMultiplication lis result)
  (cond
    ((not (= (my-length lis) 0))
     (pairMultiplication (cdr (cdr lis)) (my-append result (list (multiplicationOfFractions (list-ref lis 0) (list-ref lis 1)))))
     )
    (else
     result
     )
    )
  )
;Виклик усiх функцiй
(define n (userInput "n"))
(define empty (list))
(define lis (fillFractionList n empty))
(display "List: ")
(printFractionList lis)
(display "\nList with pair multiplication: ")
(define result (pairMultiplication lis empty))
(printFractionList result)

(display "\n***14.2***\n")
;Функцiя вiдображення комплексних чисел
(define(printComplex x)
  (newline)
  (display (el1 x))
  (display "+")
  (display (el2 x))
  (display "*i"))   
;Функцiя заповнення списку комплексних чисел користувачем
(define (fillComplexList n lis) 
  (cond
    ((not (= n 0))
     (fillComplexList (- n 1) (my-append lis (list (make-pair (userInput "numerator") (userInput "denominator")))))
     )
    (else
     lis
     )
    ))
;Функцiя вiдображення списку комплексних чисел
(define (printComplexList lis)
  (cond
    ((not (= (my-length lis) 0))
     (printComplex (list-ref lis 0)) 
        (printComplexList (cdr lis))
     )
    )
)
;Функцiя дiлення комплексних чисел
(define (complexFractions pair1 pair2)
  (make-pair (/ (+ (* (el1 pair1) (el1 pair2)) (* (* (el2 pair1) (* -1 (el2 pair2))) -1)) (+ (* (el1 pair2) (el1 pair2)) (* (el2 pair2) (el2 pair2)))) (/ (+ (* (el1 pair1) (* -1 (el2 pair2))) (* (el2 pair1) (el1 pair2))) (+ (* (el1 pair2) (el1 pair2)) (* (el2 pair2) (el2 pair2)))))
  )
;Функцiя дiлення пар комплексних чисел у списку
(define (pairComplexFractions lis result)
  (cond
    ((not (= (my-length lis) 0))
     (pairComplexFractions (cdr (cdr lis)) (my-append result (list (complexFractions (list-ref lis 0) (list-ref lis 1)))))
     )
    (else
     result
     )
    )
  )
;Виклик усiх функцiй
(define n2 (userInput "n"))
(define lis2 (fillComplexList n2 empty))
(display "List: ")
(printComplexList lis2)
(display "\nResult list: ")
(define result2 (pairComplexFractions lis2 empty))
(printComplexList result2)