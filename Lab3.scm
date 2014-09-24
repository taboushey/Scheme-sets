; Tabetha Boushey
; CSCI 305
; Lab 3 - Scheming Sets

#lang scheme
(require racket/trace)

(define (f lst)
   ; (a) ;
   (if (null? lst)
      ; (b) ;
      '()
      ; c ;
      (cons (+ 1 (car lst)) (f (cdr lst)))))
(trace f)

(define (member? e lst)
  ; The if statement first checks to see if the linked list is null
   (if (null? lst)
       #f
       ; The if statement checks to see if e is equal to the first item in the linked list
       (if (equal? e (car lst)) 
           #t 
           ; it recursively calls the member function after taking off the first item in the linked list
           (member? e (cdr lst)))))


(define (set? lst)
  ; The if statement first checks to see if the linked list is null
  (if (null? lst)
      #t
      ; checks the first item to the rest in the list
      (if (member? (car lst) (cdr lst))
          #f
          ; recursively calls set with the rest of the list
          (set? (cdr lst)))))

(define (difference lst1 lst2)
  ; if the second list is empty return nothing
  (if (null? lst2)
      '()
      ; if the first item in the second list is in the first list, don't include it
      (if (member? (car lst2) lst1)
          ; recursively call with the duplicate taken out
          (difference lst1 (cdr lst2))
          ; constructs the list with the non-duplicates
          (cons (car lst2) (difference lst1 (cdr lst2))))))

(define (union lst1 lst2)
   ; adds the first list and the remaining items from the second list together
   (append lst1 (difference lst1 lst2)))

(define (intersect lst1 lst2)
  ; if the second list is empty return nothing
  (if (null? lst2)
      '()
      ; if the first item in the second list is not in the first list, don't include it
      (if (not (member? (car lst2) lst1))
          ; recursively call with the non-duplicate taken out
          (intersect lst1 (cdr lst2))
          ; constructs the list with the duplicates
          (cons (car lst2) (intersect lst1 (cdr lst2))))))
