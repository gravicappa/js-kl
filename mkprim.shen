(trap-error
  (value top-dir)
  (/. E
      (set top-dir "modules/javascript/")))

(define cut-shen-prefix
  (@s "shen-" S) -> S
  (@s "shen_" S) -> S
  S -> S)

(define js-name
  X -> (cn "shenjs_" (cut-shen-prefix (str X))))

(define mk-op-defs-n
  F _ [] Acc -> Acc
  F Args [D | Defs] Acc -> (let T1 (output "mk-op-defs-n ~A~%" D)
                                K (js-from-kl [defun D Args [D | Args]])
                                \*A (make-string "~A~A~%" Acc K)*\
                                A Acc
                                T1 (pr (make-string "~A~%" K) F)
                             (mk-op-defs-n F Args Defs A)))

(define mk-op-defs
  F [] Acc -> Acc
  F [[A | Ops] | R] Acc -> (mk-op-defs F R (mk-op-defs-n F A Ops Acc)))

(define mk-op-defs-to-file
  Filename -> (let F (open file Filename out)
                   S (mk-op-defs F (value js-int-funcs) "")
                   \*T1 (pr S F)*\
                   T2 (close F)
                _))

\*(track js-from-kl)*\
(set js-skip-internals false)
(mk-op-defs-to-file (cn (value dst-dir) "primitives.js"))
(set js-skip-internals true)
