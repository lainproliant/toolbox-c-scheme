;--------------------------------------------------------------------
; util.scm: Generic scheme utility functions.
;
; Author: Lain Supe (lainproliant)
; Date: Monday May 9 2016
;--------------------------------------------------------------------

(module toolbox-util *
  (import chicken scheme)
  (use posix)

  ;; Determine if the given list is empty.
  (define (empty? lst)
          (and (list? lst) (equal? lst '())))

  ;; A synonym for car
  (define first car)

  ;; Yields the first element of the list or #f if the list is empty.
  (define (first-or lst default)
          (if (empty? lst)
              default
              (car lst)))

  ;; Fetch an OS environment variable value, or #f if the variable
  ;; is not set.
  (define (getenv name)
          (assoc name (get-environment-variables)))

)

