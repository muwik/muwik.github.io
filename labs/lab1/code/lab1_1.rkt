#lang racket

(define row 0)   ;Іниціал. лічильника
(define element-in-row 0)
(define depth 0)

(define (pascal x y)  ;Обчислюємо значення елемента в трикутнику за місцем знаходження
  (set! depth (+ depth 1))  ;Глибина рекурсії
    (cond ((or (<= x 0) (<= y 0) (< x y )) 0)
          ((or (= 1 y) (= x y) ) 1)
          (else (+ (pascal (- x 1) y) (pascal (- x 1) (- y 1))))))


(define (row-elements a) ;Побудова елементів стовпичка
  (if (= a 0) ; Якщо == 0, добуд. стовпчик
    (begin
      (set! element-in-row 0)) ;Скидуємо лічільник
    (begin   ;Якщо ні, то:
      (display " ")
      (set! element-in-row (+ element-in-row 1))  ;Номер елемента в рядку
      (display (pascal row element-in-row)) ;Виводимо номер рядка та номер елемента в рядку в pascal для обчислення значення
      (row-elements (- a 1)))))


(define (triangle n) ;Буд. трикутника
  (if (= n 0) ; Якщо == 0, то трикутник побудован
    (begin
      (newline)
      (newline)
      (display "Pascal's triangle is done!")
      (newline)
      (display "Recursion level:")
      (display depth)) ; Відображ. глибину рекурсії
    (begin    ;інакше
      (newline)
      (set! row (+ row 1)) ;Значення номеру рядка
      (row-elements row) ; Передаємо номер рядка в row-elements для визначення елементів в рядку
      (triangle (- n 1))))) 

(display "Enter value:")
(define value(read))  ;Зчитуємо значення
(triangle value)  ;Передаємо значення в triangle