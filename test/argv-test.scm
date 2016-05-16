(include "toolbox-scheme.scm")

(import (prefix toolbox-argv toolbox:))
(import (prefix toolbox-test toolbox:))

(toolbox:*exn-rethrow* #t)

(define (create-subcom)
  (toolbox:subcommands
    (["hello" (lambda (argv) (print "Hello there!"))]
     ["goodbye" (lambda (argv) (print "Goodbye there!"))])))

(toolbox:test-suite 'argv-tests
  (lambda ()
    (let ([subcom (create-subcom)])
      (toolbox:subcom-invoke subcom '("hello" "world"))
      (toolbox:subcom-invoke subcom '("goodbye" "world")))))

