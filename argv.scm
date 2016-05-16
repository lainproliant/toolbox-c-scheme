;--------------------------------------------------------------------
; argv.scm: Classes and functions to help dealing with command line
;           arguments and building command line apps.
;
; Author: Lain Supe (lainproliant)
; Date: Monday May 9 2016
;--------------------------------------------------------------------

(include "util.scm")

(module lain-argv (subcommands subcom-create subcom-invoke)
  (import chicken scheme lain-util)
  (use srfi-18 srfi-69)

  (define-record subcom map default)

  (define (subcom-add! sc name fun #!optional (default #f))
          (begin
           (if default
               (if (not (subcom-default sc))
                   (subcom-default-set! sc name)
                   (error "A default value was already specified."))
               #f)
           (hash-table-set! (subcom-map sc) name fun)))

  (define (subcom-invoke sc argv)
          (let* ([subcom-prov (first-or argv #f)]
                 [subcom (if (hash-table-exists? (subcom-map sc) subcom-prov)
                             subcom-prov
                             (subcom-default sc))]
                 [argv-rest (if (and (not (empty? argv)) (equal? (car argv) subcom))
                                (cdr argv)
                                argv)])
                (if (not subcom)
                    (raise 'unknown-command)
                    (apply (hash-table-ref (subcom-map sc) subcom) (list argv-rest)))))
   
  (define (subcom-create subcom-specs)
          (let ([sc (make-subcom (make-hash-table) #f)])
               (map (lambda (x) (apply subcom-add! (cons sc x))) subcom-specs)
               sc))

  (define-syntax subcommands
     (syntax-rules ()
        ((subcommands ((name func) ...))
         (subcom-create (map list (list name ...) (list func ...))))))
)
