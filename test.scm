;--------------------------------------------------------------------
; test.scm: A very simple unit testing framework.
;
; Author: Lain Supe (lainproliant)
; Date: Monday May 9 2016
;--------------------------------------------------------------------

(module lain-test (test-suite *exn-rethrow*)
  (import chicken scheme extras)
  (use srfi-1 srfi-18)

  (define *exn-rethrow* (make-parameter #f))

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
