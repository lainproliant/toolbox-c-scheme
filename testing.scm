;--------------------------------------------------------------------
; test.scm: A very simple unit testing framework.
;
; Author: Lain Supe (lainproliant)
; Date: Monday May 9 2016
;--------------------------------------------------------------------

(include "util.scm")

(module toolbox-test (test-suite *exn-rethrow*)
  (import chicken scheme extras toolbox-util)
  (use srfi-1 srfi-18)

  ;; A parameter which determines if exceptions raised by unit
  ;; tests should be raised outside of the 'test-suite' function.
  ;; It is false by default, meaning that exceptions raised
  ;; will simply fail tests.
  ;;
  ;; The value of this parameter can also be set by specifying
  ;; the 'TOOLBOX_EXN_RETHROW=1' environment variable prior to
  ;; execution.
  (define *exn-rethrow* (make-parameter
                         (equal? (getenv "TOOLBOX_EXN_RETHROW") "1")))

  ;; Execute a test, catching any exceptions thrown from it
  ;; Returns true if the test completes successfully,
  ;; false otherwise.
  (define (run-test test)
          (handle-exceptions exn (begin
                                  (print "[ERROR] Test threw an exception: " exn)
                                  (if (*exn-rethrow*)
                                      (raise exn)
                                      #f))
                             (test)
                             #t))

  ;; Executes a test as part of a folding operation.
  ;; Used by test-suite to execute and compute the number
  ;; of successful tests.
  (define (test-fold test count)
          (if (run-test test)
              (+ 1 count)
              count))

  ;; Executes the given set of lambdas as a test suite
  ;; and prints the results to standard out.
  ;; Each lambda should be 0-arity and need not return
  ;; a specific value.  In order for the test to fail it
  ;; must raise an error.
  (define (test-suite name . tests)
          (let* ([total (length tests)]
                 [pass (fold test-fold 0 tests)]
                 [fail (- total pass)])
                (if (= total pass)
                    (let ()
                         (printf "[~a] ~a tests PASSED.~n" name total)
                         #t)
                    (let ()
                         (printf "[~a] ~a/~a tests FAILED.~n" name fail total)
                         #f))))
  )
