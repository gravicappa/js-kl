\* Copyright 2010-2011 Ramil Farkhshatov

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
  
  Exception: 
  
  *\

(package js [klvm.dbg.show-code klvm.entry-template klvm.return-template
             klvm.lambda
             klvm.s2-from-kl klvm.walk-klvm
              
             klvm.call klvm.closure klvm.closure-> klvm.closure-nargs
             klvm.entry klvm.func klvm.func-obj klvm.goto klvm.goto-next
             klvm.if klvm.if-nargs>0 klvm.nargs klvm.nargs- klvm.nargs->
             klvm.nargs+ klvm.nargs-cond klvm.next klvm.next-> klvm.nregs->
             klvm.pop-error-handler klvm.push-error-handler
             klvm.put-closure-args klvm.reg klvm.reg-> klvm.ret klvm.ret->
             klvm.return klvm.s2.runtime klvm.sp+ klvm.sp- klvm.tailcall
             klvm.tailif klvm.thaw klvm.toplevel klvm.wipe

             -b null]

(defstruct context
  (func symbol)
  (nargs A)
  (nregs number)
  (indent 0)
  (toplevel string)
  (inline (list symbol)))

(define inline -> [])
(set evaluated? false)

\\# Strings and symbols conversions

(define ensure-str
  X -> X where (string? X)
  X -> (str X))

(define s'
  [] Acc -> Acc
  [X] Acc -> (cn Acc X) where (string? X)
  [X] Acc -> (cn Acc (str X))
  [X | Y] Acc -> (s' Y (cn Acc X)) where (string? X)
  [X | Y] Acc -> (s' Y (cn Acc (str X))))

(define s
  X -> (s' X ""))

(define str-js-from-shen*
  "" Acc -> Acc
  (@s "-" S) Acc -> (str-js-from-shen* S (cn Acc "_"))
  (@s "_" S) Acc -> (str-js-from-shen* S (cn Acc "$_"))
  (@s "$" S) Acc -> (str-js-from-shen* S (cn Acc "$$"))
  (@s "'" S) Acc -> (str-js-from-shen* S (cn Acc "$quote$"))
  (@s "`" S) Acc -> (str-js-from-shen* S (cn Acc "$bquote$"))
  (@s "/" S) Acc -> (str-js-from-shen* S (cn Acc "$slash$"))
  (@s "*" S) Acc -> (str-js-from-shen* S (cn Acc "$asterisk$"))
  (@s "+" S) Acc -> (str-js-from-shen* S (cn Acc "$plus$"))
  (@s "%" S) Acc -> (str-js-from-shen* S (cn Acc "$percent$"))
  (@s "=" S) Acc -> (str-js-from-shen* S (cn Acc "$eq$"))
  (@s "?" S) Acc -> (str-js-from-shen* S (cn Acc "$question$"))
  (@s "!" S) Acc -> (str-js-from-shen* S (cn Acc "$excl$"))
  (@s ">" S) Acc -> (str-js-from-shen* S (cn Acc "$gt$"))
  (@s "<" S) Acc -> (str-js-from-shen* S (cn Acc "$lt$"))
  (@s "." S) Acc -> (str-js-from-shen* S (cn Acc "$dot$"))
  (@s "|" S) Acc -> (str-js-from-shen* S (cn Acc "$bar$"))
  (@s "#" S) Acc -> (str-js-from-shen* S (cn Acc "$sharp$"))
  (@s "~" S) Acc -> (str-js-from-shen* S (cn Acc "$tilde$"))
  (@s ":" S) Acc -> (str-js-from-shen* S (cn Acc "$colon$"))
  (@s ";" S) Acc -> (str-js-from-shen* S (cn Acc "$sc$"))
  (@s "@" S) Acc -> (str-js-from-shen* S (cn Acc "$at$"))
  (@s "&" S) Acc -> (str-js-from-shen* S (cn Acc "$amp$"))
  (@s "{" S) Acc -> (str-js-from-shen* S (cn Acc "$cbraceopen$"))
  (@s "}" S) Acc -> (str-js-from-shen* S (cn Acc "$cbraceclose$"))
  (@s C S) Acc -> (str-js-from-shen* S (cn Acc C)))

\\ renaming all js-reserved keywords, functions, ...
(define str-js-from-shen
  X -> (str-js-from-shen* X ""))

(define sym-js-from-shen
  X -> (intern (str-js-from-shen (str X))))

(define backslash -> (n->string 92))
(define endl -> (n->string 10))
(define dquote -> (n->string 34))

(define esc-string
  "" Acc -> Acc
  (@s C S) Acc -> (esc-string S (cn Acc (cn (backslash) C)))
                  where (or (= C (backslash))
                            (= C (dquote)))
  (@s C S) Acc -> (esc-string S (cn Acc "\x0a"))
                  where (= (string->n C) 10)
  (@s C S) Acc -> (esc-string S (cn Acc "\x0d"))
                  where (= (string->n C) 13)
  (@s C S) Acc -> (esc-string S (cn Acc C)))

(define esc-obj
  X -> (cn (dquote) (cn (esc-string X "") (dquote))) where (string? X)
  X -> (sym-js-from-shen X) where (symbol? X)
  X -> (error "Object ~S cannot be escaped" X))

\\# Names

(define label-sym
  0 C -> (context-func C)
  X C -> (concat lb (concat X (concat - (context-func C)))))

(define block-name''
  0 N -> N
  I N -> (concat N (concat -b I)))

(define block-name'
  I N -> (concat $ (sym-js-from-shen (block-name'' I N))))

(define block-name
  I C -> (block-name' I (context-func C)))

(define func-name
  [js.quote X] -> X
  X -> (block-name' 0 X))

(define func-obj-name
  [] -> null
  [js.quote X] -> X
  Name -> Name where (string? Name)
  Name -> (str Name))

\\# Expressions

(define native
  [js.quote X] -> X
  X -> X)

(define const?
  true -> true
  false -> true
  [] -> true
  X -> true where (number? X)
  X -> true where (string? X)
  X -> true where (symbol? X)
  _ -> false)

(define expr-obj
  X -> (str X) where (number? X)
  true -> "true"
  false -> "false"
  X -> (esc-obj X) where (string? X)
  X -> (s ["vm.intern(" (esc-obj (str X)) ")"]) where (symbol? X)
  [] -> "[]"
  X -> (esc-obj X))

(define str-join'
  [] S R -> R
  [X | L] S "" -> (str-join' L S (make-string "~A" X))
  [X | L] S R -> (str-join' L S (cn R (cn S (make-string "~A" X)))))

(define str-join
  X S -> (str-join' X S ""))

(define arg-list
  X -> (str-join X ", "))

(define func-obj
  Func Arity Name -> (let Args [(func-obj-name Name) (native Arity)
                                (func-name Func)]
                       (s ["vm.partial_func(" (arg-list Args) ")"])))

(define lambda-obj
  Func Arity -> (let Args ["undefined" (native Arity) (func-name Func)]
                  (s ["new vm.Func(" (arg-list Args) ")"])))

(define expr2
  [klvm.closure-nargs] -> "func.vars.length"
  [klvm.func-obj Func Nargs Name] -> (func-obj Func Nargs Name)
  [klvm.reg 0] -> "reg[sp]"
  [klvm.reg N] -> (s ["reg[sp + " N "]"])
  [klvm.nargs] -> "vm.nargs"
  [klvm.next] -> "vm.next"
  [klvm.ret] -> "vm.ret"
  [klvm.quote X] -> X
  [klvm.lambda X] -> (func-name X)
  [fail] -> "vm.fail_obj"
  X <- (prim X)
  [X | Y] -> (error "Unexpected L2 expression ~S" [X | Y])
  X -> (expr-obj X))

(define expr-if
  If Then Else C -> (s ["if (" If ") {" (endl)
                        "      " Then ";" (endl)
                        "    } else {" (endl)
                        "      " Else ";" (endl)
                        "    }"]))

(define unwind-protect
  Thunk Restore -> (trap-error (let R (thaw Thunk)
                                    . (thaw Restore)
                                 R)
                               (/. E (do (thaw Restore)
                                         (error (error-to-string E))))))

(define with-global
  Var Value Thunk -> (let Prev (value Var)
                       (unwind-protect (freeze (do (set Var Value)
                                                   (thaw Thunk)))
                         (freeze (set Var Prev)))))

(define func-prelude
  -> "    var sp = vm.sp, reg = vm.reg")

(define entry-tpl
  -> (let Klvm (klvm.entry-template [js.quote "func"]
                                    [js.quote "func_arity"]
                                    [js.quote "func_name"])
          \\. (output "KLVM: ~S~%" Klvm)
          C (mk-context [js.quote "name"] func_arity 0 0 "" [entry])
       (s ["  vm.fn_entry = function(func, func_arity, func_name) {" (endl)
           "    var vm = this;" (endl)
           (func-prelude) ";" (endl)
           (expr Klvm 1 C)
           (endl) "    return vm.fail_obj;" (endl) "  };" (endl) (endl)])))

(define return-tpl
  -> (let Klvm (klvm.return-template [js.quote "retval"]
                                     [js.quote "retnext"])
          \\. (output "KLVM: ~S~%" Klvm)
          C (mk-context [js.quote "name"] func_nargs 0 0 "" [return])
       (s ["  vm.fn_return = function(retval, retnext) {" (endl)
           "    var vm = this;" (endl)
           (func-prelude) ";" (endl)
           (expr Klvm 1 C) (endl) "  };" (endl) (endl)])))

(define func-entry
  F Nargs Name C -> (let Args (arg-list [Nargs
                                         (esc-obj (func-obj-name Name))])
                         N (str (block-name 0 C))
                      (s ["var x = vm.fn_entry(" N ", " Args ");"
                          (endl)
                          "    if (x !== vm.fail_obj) return x;"
                          (endl)
                          (func-prelude)])))

(define func-return
  X Next _ -> (let Args (arg-list [X (s ["reg[sp + " Next "]"])])
                (s ["return vm.fn_return(" Args ")"])))

(define nargs-cond
  Arity L E G C -> (let A' (expr Arity 1 C)
                     (s ["    if (vm.nargs == " A' ") {" (endl)
                         (exprs E C "")
                         "    } else if (vm.nargs < " A' ") {" (endl)
                         (exprs L C "")
                         "    } else {" (endl)
                         (exprs G C "")
                         "    }"])))

(define if-nargs>0
  Then Else C -> (s ["    if (vm.nargs > 0) {" (endl)
                     (exprs Then C "")
                     "    } else {" (endl)
                     (exprs Else C "")
                     "    }"]))

(define push-error-handler
  X C -> (s ["vm.push_error_handler(" X ")"]))

(define put-closure-args
  C -> "vm.put_closure_args(func)")

(define next-val
  X C -> (str (block-name X C)) where (number? X)
  X C -> (prim X C))

(define reg->
  0 Y C -> (s ["reg[sp] = " (expr Y 1 C)])
  X Y C -> (s ["reg[sp + " (expr X 1 C) "] = " (expr Y 1 C)]))

(define nregs->
  X C -> (s ["var n = " X ";" (endl) "    vm.sp_top = sp + n"]))

(define closure->
  [klvm.lambda X] C -> (s ["var func = " (prim [klvm.lambda X] C)])
  X _ -> (let F (esc-obj (str X))
             (s ["var func = vm.find_func(" F ")"]))
           where (symbol? X)
  X C -> (s ["var func = " (expr X 1 C)]))

(define statement
  "" -> ""
  S -> (s ["    " S ";" (endl)]))

(define expr'
  C X 0 -> (statement (prim X C))
  C X _ -> (prim X C))

(define expr
  X L C -> (klvm.walk-klvm (expr' C) (/. X (prim-type X C)) L X))

(define exprs
  [] _ Acc -> Acc
  [X | Xs] C Acc -> (exprs Xs C (cn Acc (expr X 0 C))))

(define label
  N Code C Acc -> (s [Acc "  function " (str (block-name N C)) "(vm) {" (endl)
                      (if (= N 0)
                          ""
                          (s [(func-prelude) ";" (endl)]))
                      (exprs Code C "")
                      "  }" (endl) (endl)]))

(define labels
  [] _ Acc -> Acc
  [[N | L] | Ls] C Acc -> (labels Ls C (label N L C Acc)))

(define mkfunc
  Name Args Nregs Labels ->
  (let Nargs (length Args)
       C (mk-context Name Nargs Nregs 0 "" (inline))
    (labels Labels C "")))
    

(define def-func'
  Name Args -> (s ["  vm.defun_x(" (arg-list [(esc-obj (str Name))
                                              (length Args)
                                              (func-name Name)])
                   ");" (endl)]))

(define def-func
  Name Args -> (def-func' Name Args) where (not (value evaluated?))
  Name Args -> (s ["vm.ret = " (def-func' Name Args)]))

(define call-toplevel
  X -> (s ["  vm.nargs = 0;" (endl)
           "  toplevel_next = " X "(vm);" (endl)])
          where (value evaluated?)
  X -> (s ["  vm.call_toplevel(" X ");" (endl)]))

(define toplevel
  Name Args Nregs Code -> (let S (mkfunc Name Args Nregs Code)
                            (cn S (call-toplevel (func-name Name)))))

(define reg-lambda
  Name -> (s ["  vm.reg_lambda(" (str (block-name' 0 Name)) ");" (endl)
              (endl)]))

(define translate-toplevel
  \\X <- (do (output "KL: ~S~%" X) (fail))
  \\X <- (do (klvm.dbg.show-code [X]) (fail))
  
  [klvm.closure Name Args Nregs Code] -> (s [(mkfunc Name Args Nregs Code)
                                             (reg-lambda Name)])
  [klvm.toplevel Name Args Nregs Code] -> (toplevel Name Args Nregs Code)
  [klvm.func Name Args Nregs Code] -> (s [(mkfunc Name Args Nregs Code) (endl)
                                          (def-func Name Args) (endl)]))

(define js-toplevel
  X Vm -> (s ["(function(vm) {" (endl) X "})(" Vm ");" (endl) (endl)]))

(define from-klvm
  [] Acc -> Acc where (value evaluated?)
  [] Acc -> (js-toplevel Acc "shen")
  [X | Y] Acc -> (from-klvm Y (cn Acc (translate-toplevel X))))

(define from-kl'
  X Acc -> (from-klvm (klvm.s2-from-kl (/. X (prim-type X _))
                                       X
                                       (not (value evaluated?)))
                      Acc))

(define from-kl
  X -> (from-kl' X ""))

\\# Source translations

(define kl-from-shen
  X -> (let X (shen.walk (function macroexpand) X)
            X (if (shen.packaged? X)
                  (package-contents X)
                  X)
         (shen.elim-def (shen.proc-input+ X))))

(define from-shen
  X -> (from-kl (kl-from-shen X)))

(define from-string
  X -> (from-kl (kl-from-shen (read-from-string X))))

(set *silence* false)

(define read-shen
  File -> (unwind-protect (freeze (do (undefmacro shen.function-macro)
                                      (read-file File)))
            (freeze (shen.add-macro shen.function-macro))))

(define from-file
  File -> (let L (freeze (from-shen (read-file File)))
            (with-global evaluated? false L))
          where (or (file-extension? File ".kl")
                    (file-extension? File ".shen"))
  File -> (read-file-as-string File))

(define file-extension?
  "" Ext -> false
  (@s C Ext) Ext -> true
  (@s C Cs) Ext -> (file-extension? Cs Ext))

(define load-sources'
  [] Acc -> Acc
  [F | Files] Acc -> (load-sources' Files (append Acc (read-file F)))
                     where (or (file-extension? F ".kl")
                               (file-extension? F ".shen"))
  [F | Files] Acc -> (let Data (read-file-as-string F)
                       (load-sources' Files (append Acc [js.quote Data]))))

(define from-files'
  [] S -> S
  [File | Files] S -> (let S' (cn S (make-string "// ~A~%"  File))
                        (from-files' Files (cn S' (from-file File))))
                      where (or (file-extension? File ".kl")
                                (file-extension? File ".shen"))
  [File | Files] S -> (from-files' Files (cn S (read-file-as-string File))))

(define remove-duplicates'
  [] Acc -> (reverse Acc)
  [X | Xs] Acc -> (remove-duplicates' Xs (adjoin X Acc)))

(define remove-duplicates
  X -> (remove-duplicates' X []))

(define from-files
  Files -> (from-files' (remove-duplicates Files) ""))

(define save-from-files'
  [] F -> true
  [File | Files] F -> (let . (pr (from-file File) F)
                        (save-from-files' Files F)))

(define save-from-files
  Files Target -> (let F (open Target out)
                       Files' (remove-duplicates Files)
                    (unwind-protect 
                      (freeze (save-from-files' Files' F))
                      (freeze (close F))))))
