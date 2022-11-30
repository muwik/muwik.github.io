#lang racket
(define (my-append lis1 lis2)
  (cond ((null? lis1)
         lis2)
        (else
         (cons (car lis1)
               (my-append (cdr lis1) lis2)))))
;Створюємо список з кiлькiстю товарiв у точках
(define (allDemands points demandList num)
  (cond
    ((not (= points 0))
     (allDemands (- points 1) (my-append (list num) demandList) num)
     )
    (else
     demandList
     )
    )
  )
;Перевiрка на те чи достатньо товару у магазинi
(define (isThereEnoughProduct curProduct newcurProduct)
  (cond
    ((< newcurProduct 0)
     curProduct
     )
    (else
      newcurProduct
     )
    )
  )
;Перевiрка на те чи достатньо товару для точки
(define (isThereEnoughProductSpot curProduct newcurProduct newcurProductShop)
  (cond
    ((or (< newcurProduct 0) (< newcurProductShop 0))
     (display "\nНевдоволенний запит")
     curProduct
     )
    (else
     (display "\nЗапит вдоволено")
      newcurProduct
     )
    )
  )
;Функцiя виклику наступного дня
(define (nextDay day maxDays minusEachDay daysForDeliverToSpot daysForDeliverToShop expetcDaysForDeliver curProductSpot numberOfSpots curProductShop) 
    (cond
      ((not (= day maxDays))
        (display "\nДень ")
        (display day)
        (display ": ")
        (display "\nКожна точка витратила по ")
        (display minusEachDay)
        (display " товарiв")
        (cond
          ((= daysForDeliverToSpot 1)
           (display "\nКожна точка отримала по ")
           (display (* minusEachDay expetcDaysForDeliver))
           (display " товарiв")
           (cond
             ((= daysForDeliverToShop 1)
                (display "\nМагазин отримав ")
                (display (* 104 (* minusEachDay numberOfSpots)))
                (display " товарiв з заводу\n")
                (display "\nТовару на точках: ")
                (display (allDemands numberOfSpots empty (+ (- curProductSpot minusEachDay) (* minusEachDay expetcDaysForDeliver))))
                (display "\nТовару у магазинi: ")
                (display (+ (- curProductShop (* numberOfSpots (* minusEachDay expetcDaysForDeliver))) (* 104 (* minusEachDay numberOfSpots))))
                (nextDay (+ day 1) maxDays minusEachDay expetcDaysForDeliver 104 expetcDaysForDeliver (+ (- curProductSpot minusEachDay) (* minusEachDay expetcDaysForDeliver)) numberOfSpots (+ (- curProductShop (* numberOfSpots (* minusEachDay expetcDaysForDeliver))) (* 104 (* minusEachDay numberOfSpots))))
                )
             (else
                (display "\nТовару на точках: ")
                (display (allDemands numberOfSpots empty (isThereEnoughProductSpot (- curProductSpot minusEachDay) (+ (- curProductSpot minusEachDay) (* minusEachDay expetcDaysForDeliver)) (- curProductShop (* numberOfSpots (* minusEachDay expetcDaysForDeliver))))))
                (display "\nТовару у магазинi: ")
                (display (isThereEnoughProductSpot (- curProductSpot minusEachDay) (+ (- curProductSpot minusEachDay) (* minusEachDay expetcDaysForDeliver)) (- curProductShop (* numberOfSpots (* minusEachDay expetcDaysForDeliver)))))
                (nextDay (+ day 1) maxDays minusEachDay expetcDaysForDeliver (- daysForDeliverToShop 1) expetcDaysForDeliver (isThereEnoughProductSpot (- curProductSpot minusEachDay) (+ (- curProductSpot minusEachDay) (* minusEachDay expetcDaysForDeliver)) (- curProductShop (* numberOfSpots (* minusEachDay expetcDaysForDeliver)))) numberOfSpots (isThereEnoughProductSpot (- curProductSpot minusEachDay) (+ (- curProductSpot minusEachDay) (* minusEachDay expetcDaysForDeliver)) (- curProductShop (* numberOfSpots (* minusEachDay expetcDaysForDeliver)))))
                )
             )
           )
          (else
            (display "\nТовари на точках: " )
            (display (allDemands numberOfSpots empty (- curProductSpot minusEachDay)))
            (cond
             ((= daysForDeliverToShop 1)
                (display "\nТовару у магазинi: ")
                (display (+ curProductShop (*(* 104 minusEachDay) numberOfSpots)))
                (display "\nМагазин отримав ")
                (display (* (* 104 minusEachDay) numberOfSpots))
                (display " товарiв з заводу\n")
                (nextDay (+ day 1) maxDays minusEachDay (- daysForDeliverToSpot 1) 104 expetcDaysForDeliver (- curProductSpot minusEachDay) numberOfSpots (+ curProductShop (*(* 104 minusEachDay) numberOfSpots)))
                )
            (else
                (nextDay (+ day 1) maxDays minusEachDay (- daysForDeliverToSpot 1) (- daysForDeliverToShop 1) expetcDaysForDeliver (- curProductSpot minusEachDay) numberOfSpots curProductShop)
                )
            )
            )
          )
        )
      )
  )
;Кiлькicть днiв симуляцiї            
(define maxDays 360)
;Витрати кожного дня однiєї точки
(define minusEachDay 10)
;Скiльки днiв очiкується доставка
(define expetcDaysForDeliver 6)
;Кiлькiсть точок
(define numberOfSpots 6)
(nextDay 1 maxDays minusEachDay 1 1 expetcDaysForDeliver 0 numberOfSpots 0)