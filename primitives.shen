(package js [shenjs-eval klvm.reg klvm.runtime shen-]

(set int-funcs [[[X] | [hd tl not string? number? symbol? cons?
                        vector? absvector? value intern vector
                        read-byte close absvector str tlstr n->string
                        string->n empty? error simple-error
                        error-to-string]]
                [[X Y] | [+ - * / = > >= < <= cons set <-address
                         cn pos @p write-byte or and open]]
                [[X Y Z] | [address->]]])

(define count-int-funcs'
  _ [] Acc -> Acc
  0 [[_ | A] | R] Acc -> (count-int-funcs-aux 0 R (+ Acc (length A)))
  1 [[[X] | A] | R] Acc -> (count-int-funcs-aux 1 R (+ Acc (length A)))
  2 [[[X Y] | A] | R] Acc -> (count-int-funcs-aux 2 R (+ Acc (length A)))
  3 [[[X Y Z] | A] | R] Acc -> (count-int-funcs-aux 3 R (+ Acc (length A)))
  N [[_ | A] | Rest] Acc -> (count-int-funcs-aux N Rest Acc))

(define mkargs
  [X] Acc -> (cn Acc (expr2 X))
  [X | Xs] Acc -> (mkargs Xs (s [Acc (expr2 X) ", "])))

(define mkprim
  Name Args -> (s ["vm." Name "(" (mkargs Args "") ")"]))

(define prim-intern
  "true" -> "true"
  "false" -> "false"
  X -> (mkprim "intern" [X]))

(define prim-tuple
  X Y -> (make-string "[vm.fns['shen.tuple'], ~A, ~A]" X Y))

(define primitives
  [] -> "[]"
  [= X Y] -> (mkprim "is_equal" [X Y])
  [string? X] -> (make-string "(typeof(~A) === 'string')" (expr2 X))
  [number? X] -> (make-string "(typeof(~A) === 'number')" (expr2 X))
  [symbol? X] -> (make-string "(~A instanceof vm.Sym)" (expr2 X))
  [cons? X] -> (make-string "(~A instanceof vm.Cons)" (expr2 X))
  [vector? X] -> (mkprim "is_vector" [X])
  [absvector? X] -> (mkprim "is_absvector" [X])
  [empty? X] -> (mkprim "is_empty" [X])
  [function X] -> (make-string "vm.find_func(~A)" (expr2 X))
  [str X] -> (mkprim "str" [X])
  [tlstr X] -> (mkprim "tlstr" [X])
  [n->string X] -> (mkprim "str_from_n" [X])
  [string->n X] -> (mkprim "n_from_str" [X])
  [not X] -> (make-string "(!~A)" (expr2 X))
  [intern X] -> (prim-intern X)
  [hd X] -> (make-string "~A.head" (expr2 X))
  [tl X] -> (make-string "~A.tail" (expr2 X))
  [value X] -> (mkprim "value" [X])
  [set X Y] -> (mkprim "set" [X Y])
  [vector X] -> (make-string "vm.vector(~A)" (expr2 X))
  [absvector X] -> (make-string "vm.absvector(~A)" (expr2 X))
  [<-address V I] -> (mkprim "absvector_ref" [V I])
  \\[<-address V X] -> (make-string "~A[~A]" (expr2 V) (expr2 X))
  [address-> V I X] -> (mkprim "absvector_set" [V I X])
  [open Name Dir] -> (make-string "vm.io.open(~A, ~A, vm)"
                                  (expr2 Name) (expr2 Dir))
  [read-byte X] -> (make-string "~A.read_byte(vm)" (expr2 X))
  [write-byte X Y] -> (make-string "~A.write_byte(~A, vm)"
                                   (expr2 X) (expr2 Y))
  [close X] -> (make-string "~A.close(vm)" (expr2 X))
  [error X] -> (mkprim "error" [X])
  [simple-error X] -> (mkprim "error" [X])
  [error-to-string X] -> (mkprim "error_to_string" [X])
  [cn X Y] -> (make-string "(~A + ~A)" (expr2 X) (expr2 Y))
  [pos X Y] -> (make-string "~A[~A]" (expr2 X) (expr2 Y))
  [@p X Y] -> (prim-tuple (expr2 X) (expr2 Y))
  [cons X Y] -> (make-string "(new vm.Cons(~A, ~A))" (expr2 X) (expr2 Y))
  [/ X Y] -> (make-string "(~A / ~A)" (expr2 X) (expr2 Y))
  [and X Y] -> (make-string "(~A && ~A)" (expr2 X) (expr2 Y))
  [or X Y] -> (make-string "(~A || ~A)" (expr2 X) (expr2 Y))
  [Op X Y] -> (make-string "(~A ~A ~A)" (expr2 X) Op (expr2 Y))
              where (element? Op [+ - * / > < >= <=])
  [pr X Y] -> (mkprim "write_string" [X Y])
  [fail] -> "vm.fail_obj"
  _ -> (fail))

(define gen-prim-args
  N N Acc -> Acc
  I N Acc -> (gen-prim-args (+ I 1) N [[klvm.reg I] | Acc]))

(define generate-prim
  X Args -> (let Name (sym-js-from-shen (concat shen- X))
                 X' (esc-obj (str X))
                 C (mk-context Name 0 0 0 "" [])
                 Nargs (length Args)
                 Args' (gen-prim-args 0 Nargs [])
                 Code (primitives [X | Args'])
              (s ["  vm.defun_x(" X' ", " Nargs ", function " Name "(vm) {"
                  (endl)
                  "    var x = vm.fn_entry(" Name ", " Nargs ", " X' ");" (endl)
                  "    if (x !== vm.fail_obj) return x;" (endl)
                  "    var reg = vm.reg, sp = vm.sp;" (endl)
                  "    return vm.fn_return(" Code ", vm.next);" (endl)
                  "  });" (endl) (endl)])))

(define generate-primitives-n
  _ [] Acc -> Acc
  Args [X | R] Acc -> (let S (generate-prim X Args)
                           Acc (cn Acc S)
                        (generate-primitives-n Args R Acc)))

(define generate-primitives'
  [] Acc -> Acc
  [[Args | Prims] | R] Acc -> (let Acc (generate-primitives-n Args Prims Acc)
                                (generate-primitives' R Acc)))

(define generate-primitives
  -> (let S (s [(entry-tpl) (endl) (return-tpl)])
          S (generate-primitives' (value int-funcs) S)
       (with-global evaluated? false (freeze (from-kl' (klvm.runtime) S)))))
)
