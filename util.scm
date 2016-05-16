;--------------------------------------------------------------------
; util.scm: Generic scheme utility functions.
;
; Author: Lain Supe (lainproliant)
; Date: Monday May 9 2016
;--------------------------------------------------------------------

(module lain-util (empty? first first-or)
  (import chicken scheme)

  ;; Determine if the given list is empty.
  (define empty? (lambda (lst)
                         (and (list? lst) (equal? lst '()))))

  ;; A synonym for car
  (define first car)

  ;; Yields the first element of the list or #f if the list is empty.
  (define first-or (lambda (lst default)
                           (if (empty? lst)
                               default
                               (car lst))))
)

