;--------------------------------------------------------------------
; argv.scm: Classes and functions to help dealing with command line
;           arguments and building command line apps.
;
; Author: Lain Supe (lainproliant)
; Date: Monday May 9 2016
;--------------------------------------------------------------------

(include "util.scm")

(module toolbox-argv * 
  (import chicken scheme toolbox-util)
  (use srfi-18 srfi-69)
  
  ;; Define the subcommand map record type 'subcom'
  (define-record subcom map default)
   
  ;; Add a subcommand with the given name and function to the
  ;; subcommand map.  It is not recommended to use this function
  ;; directly, use the 'subcommands' macro instead.
  (define (subcom-add! sc name fun)
          (hash-table-set! (subcom-map sc) name fun))
   
  ;; Invoke the appropriate subcommand for the input provided.
  ;; The first parameter of the argument list is scanned to determine
  ;; if it corresponds to a subcommand defined in the map.  If so,
  ;; it is chopped off and the appropriate subcommand is invoked
  ;; with the remaining parameters.
  ;;
  ;; If no default subcommand is defined and the first parameter does
  ;; not match any defined subcommands, 'unknown-command' is raised.
  (define (subcom-invoke sc #!optional (argv (command-line-arguments)))
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
   
  ;; Create a subcommand map object with the given list of subcom-specs.
  ;; It is recommended not to call this function directly, but rather
  ;; to use the 'subcommands' macro.
  (define (subcom-create #!optional (subcom-specs '()))
          (let ([sc (make-subcom (make-hash-table) #f)])
               (map (lambda (x) (apply subcom-add! (cons sc x))) subcom-specs)
               sc))

  ;; Specify a default subcommand for the subcommand map, to be used
  ;; when no subcommands are provided in the argument list.
  ;; If the specified default command is not defined, 'unknown-command'
  ;; will be raised.
  (define (subcom-default! sc default)
          (if (hash-table-exists? (subcom-map sc) default)
              (subcom-default-set! sc default)
              (raise `unknown-command)))
   
  ;; Create a subcommand map with the given subcommands.
  ;; Each subcommand is specified by a string and a function
  ;; to be invoked when that command is encountered.  The function
  ;; should have 1-arity and is provided with the argument list,
  ;; modified to remove the specified command.
  ;;
  ;; EXAMPLE:
  ;;
  ;; (define subcom (subcommands
  ;;                  (["hello" (lambda (argv) (print "Hello " (car argv)))])
  ;;                  (["goodbye" (lambda (argv) (print "Goodbye " (car argv)))])))
  ;;
  ;; ;; Specify hello as the default subcommand.
  ;; (subcom-default! subcom "hello")
  (define-syntax subcommands
     (syntax-rules ()
        ((subcommands ((name func) ...))
         (subcom-create (map list (list name ...) (list func ...))))))
)
