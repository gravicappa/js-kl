(package js [shenjs-eval klvm.reg klvm.runtime klvm.entry shen- walk subst skip
              
             klvm.lambda klvm.call klvm.closure klvm.closure->
             klvm.closure-nargs klvm.entry klvm.func klvm.func-obj klvm.goto
             klvm.goto-next klvm.if klvm.if-nargs>0 klvm.nargs klvm.nargs-
             klvm.nargs-> klvm.nargs+ klvm.nargs-cond klvm.next klvm.next->
             klvm.nregs-> klvm.pop-error-handler klvm.push-error-handler
             klvm.put-closure-args klvm.reg klvm.reg-> klvm.ret klvm.ret->
             klvm.return klvm.s2.runtime klvm.sp+ klvm.sp- klvm.tailcall
             klvm.tailif klvm.thaw klvm.toplevel klvm.wipe
             ]

(set int-funcs [[[X] | [hd tl not string? number? symbol? cons?
                        vector? absvector? value intern vector
                        read-byte close absvector str tlstr n->string
                        string->n empty? error simple-error
                        error-to-string]]
                [[X Y] | [+ - * / = > >= < <= cons set <-address
                         cn pos @p write-byte or and open]]
                [[X Y Z] | [address->]]])

(define mkprim
  Name Args -> (@s Name "(" (arg-list Args) ")"))

(define prim-intern
  "true" -> "true"
  "false" -> "false"
  X -> (mkprim "vm.intern" [X]))

(define prim-tuple
  X Y -> (make-string "[vm.fns['shen.tuple'], ~A, ~A]" X Y))

(klvm.define-primitives prim
  (skip
   [klvm.entry F Nargs Name] C -> (func-entry F Nargs Name C)
   [klvm.goto L] C -> (@s "return " (str (block-name L C)))
   [klvm.goto-next] C -> "return vm.next"
   [klvm.call] C -> "return func.fn"
   [klvm.call lambda] C -> "return func"
   [klvm.nargs] _ -> "vm.nargs"
   [klvm.next] _ -> "vm.next"
   [klvm.ret] _ -> "vm.ret"
   [klvm.pop-error-handler] _ -> "vm.error_handlers.pop()"
   [klvm.put-closure-args] C -> (put-closure-args C)
   [klvm.next-> X] C -> (@s "vm.next = " (next-val X C)) where (number? X)
   [klvm.closure-> X] C -> (closure-> X C)
   [klvm.closure-nargs] _ -> "func.vars.length"
   [klvm.reg 0] _ -> "reg[sp]"
   [klvm.reg-> X Y] C -> (reg-> X Y C)
   [klvm.lambda X] _ -> (str (func-name X))
   [klvm.func-obj Func Nargs Name] _ -> (func-obj Func Nargs Name)
   [js.quote X] _ -> X
   [js.fn-args | Args] _ -> Args
   [klvm.nargs-cond N L E G] C -> (nargs-cond N L E G C)
   [klvm.if-nargs>0 Then Else] C -> (if-nargs>0 Then Else C)
   [fail] _ -> "vm.fail_obj"
   X _ -> (expr-obj X) where (const? X))

  (subst
   [] _ -> "[]"
   [function X] _ -> (mkprim "vm.find_func" [X])
   [= X Y] _ -> (mkprim "vm.is_equal" [X Y])
   [string? X] _ -> (make-string "(typeof(~A) === 'string')" X)
   [number? X] _ -> (make-string "(typeof(~A) === 'number')" X)
   [symbol? X] _ -> (make-string "(~A instanceof vm.Sym)" X)
   [cons? X] _ -> (make-string "(~A instanceof vm.Cons)" X)
   [vector? X] _ -> (mkprim "vm.is_vector" [X])
   [absvector? X] _ -> (mkprim "vm.is_absvector" [X])
   [empty? X] _ -> (mkprim "vm.is_empty" [X])
   [str X] _ -> (mkprim "vm.str" [X])
   [tlstr X] _ -> (mkprim "vm.tlstr" [X])
   [n->string X] _ -> (mkprim "vm.str_from_n" [X])
   [string->n X] _ -> (mkprim "vm.n_from_str" [X])
   [intern X] _ -> (prim-intern X)
   [hd X] _ -> (make-string "~A.head" X)
   [tl X] _ -> (make-string "~A.tail" X)
   [value X] _ -> (mkprim "vm.value" [X])
   [set X Y] _ -> (mkprim "vm.set" [X Y])
   [vector X] _ -> (mkprim "vm.vector" [X])
   [absvector X] _ -> (mkprim "vm.absvector" [X])
   [<-address V I] _ -> (mkprim "vm.absvector_ref" [V I])
   \\[<-address V X] _ -> (make-string "~A[~A]" V X)
   [address-> V I X] _ -> (mkprim "vm.absvector_set" [V I X])
   [open Name Dir] _ -> (make-string "vm.io.open(~A, ~A, vm)" Name Dir)
   [read-byte X] _ -> (make-string "~A.read_byte(vm)" X)
   [write-byte X Y] _ -> (make-string "~A.write_byte(~A, vm)" X Y)
   [close X] _ -> (make-string "~A.close(vm)" X)
   [error X] _ -> (mkprim "vm.error" [X])
   [simple-error X] _ -> (mkprim "vm.error" [X])
   [error-to-string X] _ -> (mkprim "vm.error_to_string" [X])
   [cn X Y] _ -> (make-string "(~A + ~A)" X Y)
   [pos X Y] _ -> (make-string "~A[~A]" X Y)
   [@p X Y] _ -> (prim-tuple X Y)
   [cons X Y] _ -> (make-string "(new vm.Cons(~A, ~A))" X Y)
   [not X] _ -> (make-string "(!~A)" X)
   [Op X Y] _ -> (make-string "(~A ~A ~A)" X Op Y)
                 where (element? Op [+ - * / > < >= <=])
   [pr X Y] _ -> (mkprim "vm.write_string" [X Y])

   [fail] _ -> "vm.fail_obj")

  (subst-ffi
   [js. | X] C -> (ffi-expr [js. | X] C)
   [js.call F | Args] C -> (ffi-expr [js.call  F | Args] C)
   [js.set Dst Src] C -> (ffi-expr [js.set Dst Src] C)
   [js.new Class | Args] C -> (ffi-expr [js.new Class | Args] C)
   [js.obj | Init] C -> (ffi-expr [js.obj Init] C)
   [js.arr | Init] C -> (ffi-expr [js.obj Init] C)
   [js.fn [js.fn-args | Args] Fn] C -> (ffi-expr [js.fn Args Fn] C)
   )

  (walk
   [and X Y] _ -> (make-string "(~A && ~A)" X Y)
   [or X Y] _ -> (make-string "(~A || ~A)" X Y)
   [klvm.next-> X] C -> (@s "vm.next = " X)
   [klvm.nregs-> X] C -> (nregs-> X C)
   [klvm.return X Next] C -> (func-return X Next C)
   [klvm.if If Then Else] C -> (expr-if If Then Else C)
   [klvm.push-error-handler X] C -> (push-error-handler X C)
   [klvm.ret-> X] _ -> (@s "vm.ret = " X)
   [klvm.nregs-> X] C -> (nregs-> X C)
   [klvm.sp+ X] _ -> (@s "sp += " X ";" (endl)
                         "    vm.sp = sp")
   [klvm.sp- X] _ -> (@s "sp -= " X ";" (endl)
                         "    vm.sp = sp")
   [klvm.nargs-> X] _ -> (@s "vm.nargs = " X)
   [klvm.nargs+ X] _ -> (@s "vm.nargs += " X)
   [klvm.nargs- X] _ -> (@s "vm.nargs -= " X)
   \\[klvm.wipe X] _ -> "reg.length = sp + vm.sp_top"
   [klvm.wipe X] _ -> ""
   [klvm.reg N] _ -> (@s "reg[sp + " N "]")

   _ _ -> (fail)))

(define gen-prim-args
  N N Acc -> Acc
  I N Acc -> (gen-prim-args (+ I 1) N [[klvm.reg I] | Acc]))

(define generate-prim
  X Args -> (let Name (sym-js-from-shen (concat shen- X))
                 X' (esc-obj (str X))
                 C (mk-context Name 0 0 0 "" [])
                 Nargs (length Args)
                 Args' (gen-prim-args 0 Nargs [])
                 Code (expr [X | Args'] 1 C)
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
       (with-global evaluated? false (freeze (from-kl' (klvm.runtime) S))))))
