(include "toolbox-scheme.scm")

(import (prefix lain-argv lain:))
(import (prefix lain-test lain:))

(lain:*exn-rethrow* #t)

(define (create-subcom)
  (lain:subcommands
    (["hello" (lambda (argv) (print "Hello there!"))]
     ["goodbye" (lambda (argv) (print "Goodbye there!"))])))

(lain:test-suite 'argv-tests
  (lambda ()
    (let ([subcom (create-subcom)])
      (lain:subcom-invoke subcom '("hello" "world"))
      (lain:subcom-invoke subcom '("goodbye" "world")))))

