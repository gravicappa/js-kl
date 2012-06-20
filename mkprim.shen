(trap-error
  (value top-dir)
  (/. E
      (set top-dir "modules/javascript/")))

(set implemented-ints [get-time = empty? boolean? vector? absvector?
                       absvector value set vector str intern n->string
                       string->n eval-kl open write-byte read-byte close
                       tlstr pr error simple-error error-to-string])

(define cut-shen-prefix
  (@s "shen-" S) -> S
  (@s "shen_" S) -> S
  S -> S)

(define js-name
  X -> (cn "shenjs_" (cut-shen-prefix (str X))))

(define mk-op-defs-n
  _ [] Acc -> Acc
  Args [D | Defs] Acc 
  -> (let F (func-name D)
          N (length Args)
          JF (js-name F)
          K (make-string "~A = [shen_type_func, ~A, ~A, []];~%" F JF N)
          A (make-string "~A~A~%" Acc K)
       (mk-op-defs-n Args Defs A))
                         where (element? D (value implemented-ints))
  Args [D | Defs] Acc -> (let K (js-from-kl [defun D Args [D | Args]])
                              A (make-string "~A~A~%" Acc K)
                           (mk-op-defs-n Args Defs A)))

(define mk-op-defs
  [] Acc -> Acc
  [[A | Ops] | R] Acc -> (mk-op-defs R (mk-op-defs-n A Ops Acc)))

(define mk-op-defs-to-file
  Filename -> (let S (mk-op-defs (value js-int-funcs) "")
                   F (open file Filename out)
                (do (pr S F)
                    (close F))))

\*(track js-from-kl)*\
(set js-skip-internals false)
(mk-op-defs-to-file (cn (value dst-dir) "primitives.js"))
(set js-skip-internals true)
