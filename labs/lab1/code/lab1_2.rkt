#lang racket
(define (main)  ;Основна ф-ія
  (define digit (read)) ; Зчитування даних з клавіатури
  (if (integer? digit) ;Чи ціле число
    (begin   ;Якщо так, то:
      (if(= digit 0) ;Перевірка чи == 0
        (display "") ;Стоп
        (begin  ;Якщо так, то
          (if (odd? digit) ;Перевірка чи число непарне
            (begin
              (display digit) ;Відображення результату
              (display " - odd digit")
              (newline)
              (main)) ;Продовження зчитування
            (begin
              (display (number->string digit 2))
              (newline)
              (main)))))) ;продовження зчитування
      (begin
        (display "Enter:")
        (main)))) ;Продовження зчитування


(main) ;Старт програми
