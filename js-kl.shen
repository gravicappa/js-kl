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
  along with this program.  If not, see <http://www.gnu.org/licenses/>. *\

(package js [js-from-kl js-from-shen js.dump register-dumper
             javascript all cli
             regkl.walk
             shen-user-lambda regkl.arg regkl.reg regkl.reg-> regkl.closure
             regkl.func regkl.trap-error regkl.freeze regkl.toplevel]

(defstruct context
  (nregs number)
  (toplevel string)
  (argname symbol)
  (varname symbol))

(define max
  X Y -> X where (> X Y)
  _ Y -> Y)

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

\* renaming all js-reserved keywords, functions, ... *\
(define str-js-from-shen
  X -> (str-js-from-shen* X ""))

(define sym-js-from-shen
  X -> (intern (str-js-from-shen (str X))))

(define backslash -> (n->string 92))
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

(define func-name
  X -> (sym-js-from-shen X) where (symbol? X)
  X -> X)

(define intfunc-name
  X -> (intern (cn "Shen." (str-js-from-shen (str X))))
       where (or (shen.sysfunc? X) (value shen.*installing-kl*))
  X -> (sym-js-from-shen X) where (symbol? X)
  X -> X)

(set int-funcs [[[X] | [hd tl not thaw string? number? symbol? cons?
                        vector? absvector? value intern vector
                        read-byte close absvector str tlstr n->string
                        string->n empty? get-time error simple-error
                        eval-kl error-to-string call-js]]
                [[X Y] | [+ - * / and or = > >= < <= cons set <-address
                         cn pos @p open write-byte]]
                [[X Y Z] | [address->]]])

(set internals [get-time = empty? boolean? vector? absvector?
                absvector value set vector str intern n->string
                string->n eval-kl open write-byte read-byte close
                tlstr error simple-error error-to-string shenjs-call-js])

(set tail-internals [= shenjs-call-js])

(define int-func-args*
  X [] -> []
  X [[V | Syms] | R] -> V where (element? X Syms)
  X [[_ | _] | R] -> (int-func-args* X R))

(define int-func-args
  X -> (int-func-args* X (value int-funcs)))

(define int-func?
  fail -> true
  X -> (not (empty? (int-func-args X))))

(define esc-obj
  X -> (cn "c#34;" (cn (esc-string X "") "c#34;")) where (string? X)
  X -> (sym-js-from-shen X) where (symbol? X)
  X -> (error "Object ~R cannot be escaped" X))

(define str-join*
  [] S R -> R
  [X | L] S "" -> (str-join* L S (make-string "~A" X))
  [X | L] S R -> (str-join* L S (cn R (cn S (make-string "~A" X)))))

(define str-join
  X S -> (str-join* X S ""))

(define arg-list
  X -> (str-join X ", "))

(define arg-name
  N C -> (make-string "~A_~A" (context-argname C) N))

(define tail-call-ret
  X -> (make-string "(function() {~%  return ~A;})" X))

(define tail-call-expr
  Code C -> (js-from-kl-expr Code false C))

(define cond-case
  X C -> (tail-call-expr X C))

(define emit-cond*
  [] Tail? C -> (error "cond failure: no default branch")
  [[true E] | Cases] Tail? C -> (js-from-kl-expr E Tail? C)
  [[X E] | Cases] Tail? C -> (make-string "((~A)~%  ? ~A~%  : ~A)"
                                          (cond-case X C)
                                          (js-from-kl-expr E Tail? C)
                                          (emit-cond* Cases Tail? C)))

(define emit-cond
  Cases Tail? C -> (emit-cond* Cases Tail? C))

(define emit-trap-error
  X EF false C -> (let S (js-from-kl-expr X false C)
                       EX (js-from-kl-expr EF false C)
                    (make-string "Shen.trap_error(~A, ~A)" S EX))
  X EF true C -> (tail-call-ret (emit-trap-error X EF false C)))

(define predicate-op
  number? X _ _ -> "true" where (number? X)
  string? X _ _ -> "true" where (string? X)
  boolean? true _ _ -> "true"
  boolean? false _ _ -> "true"
  boolean? X Tail? C -> (int-funcall boolean? [X] Tail? C)
  string? X _ C -> (make-string "(typeof(~A) == 'string')"
                                (js-from-kl-expr X false C))
  number? X _ C -> (make-string "(typeof(~A) == 'number')"
                                (js-from-kl-expr X false C))
  symbol? X _ C -> (make-string "Shen.is_type(~A, ~A)"
                                (js-from-kl-expr X false C)
                                "Shen.type_symbol")
  cons? X _ C -> (make-string "Shen.is_type(~A, ~A)"
                              (js-from-kl-expr X false C)
                              "Shen.type_cons")
  tuple? X _ C -> (make-string "Shen.is_type(~A, ~A)"
                               (js-from-kl-expr X false C)
                               "Shen.fns['shen.tuple']")
  vector? X Tail? C -> (int-funcall vector? [X] Tail? C)
  empty? X Tail? C -> (int-funcall empty? [X] Tail? C)
  absvector? X Tail? C -> (int-funcall absvector? [X] Tail? C)
  _ _ _ _ -> (fail))

(define math-op
  + [X Y] _ _ -> (str (+ X Y)) where (and (number? X) (number? Y))
  - [X Y] _ _ -> (str (- X Y)) where (and (number? X) (number? Y))
  * [X Y] _ _ -> (str (* X Y)) where (and (number? X) (number? Y))
  / [X Y] _ _ -> (str (/ X Y)) where (and (number? X) (number? Y)
                                          (not (= Y 0)))
  Op [X Y] Tail? C -> (make-string "(~A ~A ~A)"
                                   (js-from-kl-expr X false C)
                                   Op
                                   (js-from-kl-expr Y false C))
                      where (element? Op [+ - * /])
  _ _ _ _ -> (fail))

(define equality-op
  [X Y] _ _ -> (str (= X Y)) where (and (number? X) (number? Y))
  [X Y] _ _ -> (str (= X Y)) where (and (string? X) (string? Y))
  [X Y] _ _ -> (str (= X Y)) where (and (boolean? X) (boolean? Y))
  [X []] Tail? C -> (int-funcall empty? [X] Tail? C)
  [[] Y] Tail? C -> (int-funcall empty? [Y] Tail? C)
  [X Y] Tail? C -> (int-funcall = [X Y] Tail? C)
  _ _ _ -> (fail))

(define order-op
  Op [X Y] _ C -> (let X (js-from-kl-expr X false C)
                       Y (js-from-kl-expr Y false C)
                    (make-string "(~A ~A ~A)" X Op Y))
                  where (element? Op [> < >= <=])
  _ _ _ _ -> (fail))

(define logic-op
  not [false] _ _ -> "true"
  not [true] _ _ -> "false"
  not [X] _ C -> (make-string "(!~A)" (js-from-kl-expr X false C))
  and [false X] _ _ -> "false"
  or [true X] _ _ -> "true"
  and [X Y] _ C ->  (make-string "(~A && ~A)"
                                 (js-from-kl-expr X false C)
                                 (js-from-kl-expr Y false C))
  or [X Y] _ C -> (make-string "(~A || ~A)"
                               (js-from-kl-expr X false C)
                               (js-from-kl-expr Y false C))
  _ _ _ _ -> (fail))


(define emit-set*
  X V C true -> (let S (esc-obj (str X))
                  (make-string "(Shen.globals[~A] = ~A)" S V))
  X V C false -> (let X (js-from-kl-expr X false C)
                   (make-string "(Shen.globals[~A[1]] = ~A)" X V)))

(define emit-set
  X V C -> (emit-set* X (js-from-kl-expr V false C) C (symbol? X)))

(define emit-value
  X C true -> (make-string "(Shen.globals[~A])" (esc-obj (str X)))
  X C false -> (let X (js-from-kl-expr X false C)
                 (make-string "(Shen.globals[~A[1]])" X)))

(define basic-op
  intern ["true"] _ _ -> "true"
  intern ["false"] _ _ -> "false"
  intern [X] _ _ -> (make-string "[Shen.type_symbol, ~A]" (esc-obj X))
                    where (string? X)
  intern [X] Tail? C -> (int-funcall intern [X] Tail? C)
  cons [X Y] _ C -> (let X (js-from-kl-expr X false C)
                         Y (js-from-kl-expr Y false C)
                      (make-string "[Shen.type_cons, ~A, ~A]" X Y))
  @p [X Y] _ C -> (let X (js-from-kl-expr X false C)
                       Y (js-from-kl-expr Y false C)
                    (make-string "[Shen.fns['shen.tuple'], ~A, ~A]" X Y))
  set [X Y] _ C -> (emit-set X Y C)
  value [X] _ C -> (emit-value X C (symbol? X))
  thaw [X] Tail? C -> (emit-thaw X Tail? C)
  function [X] _ C -> (js-from-kl-expr X true C)
  hd [X] _ C -> (make-string "~A[1]" (js-from-kl-expr X false C))
  tl [X] _ C -> (make-string "~A[2]" (js-from-kl-expr X false C))
  cn [X Y] _ C -> (make-string "(~A + ~A)"
                               (js-from-kl-expr X false C)
                               (js-from-kl-expr Y false C))
  pos [X Y] _ C -> (make-string "~A[~A]"
                                (js-from-kl-expr X false C)
                                (js-from-kl-expr Y false C))
  address-> [V I X] _ C -> (make-string "Shen.absvector_set(~A, ~A, ~A)"
                                        (js-from-kl-expr V false C)
                                        (js-from-kl-expr I false C)
                                        (js-from-kl-expr X false C))
  <-address [V I] _ C -> (make-string "Shen.absvector_ref(~A, ~A)"
                                      (js-from-kl-expr V false C)
                                      (js-from-kl-expr I false C))
  _ _ _ _ -> (fail))

(define int-funcall*
  F Args true true C -> (int-funcall* F Args false false C)
  F Args true false C -> (let X (int-funcall* F Args false false C)
                           (make-string "Shen.unwind_tail(~A)" X))
  F Args false false C -> (let A (map (/. X (js-from-kl-expr X false C)) Args)
                               As (str-join A ", ")
                            (make-string "~A(~A)" (intfunc-name F) As))
  F Args false true C -> (tail-call-ret (int-funcall* F Args false false C)))

(define int-funcall
  F Args Tail? C -> (let Tcall? (element? F (value tail-internals))
                      (int-funcall* F Args Tcall? Tail? C)))

(define int-curry
  F Opargs Args C -> (let X (make-string "Shen.fns[c#34;~Ac#34;][1]" F)
                          A (map (/. X (js-from-kl-expr X false C)) Args)
                       (emit-func-obj (length Opargs) X A [])))

(define internal-op*
  Op Opargs Args Tail? C -> (int-funcall Op Args Tail? C)
                            where (= (length Opargs) (length Args))
  Op Opargs Args _ C -> (int-curry Op Opargs Args C))

(define internal-op
  Op Args Tail? C -> (let Opargs (int-func-args Op)
                          Name (intfunc-name Op)
                       (if (empty? Opargs)
                           (fail)
                           (internal-op* Op Opargs Args Tail? C))))

(define emit-do
  [X] Tail? C Acc -> (let Do (map (/. Y (js-from-kl-expr Y false C))
                                  (reverse Acc))
                          Sep (make-string ",~%  ")
                       (make-string "(~A,~%  ~A)"
                                    (str-join Do Sep)
                                    (js-from-kl-expr X Tail? C)))
  [X | Body] Tail? C Acc -> (emit-do Body Tail? C [X | Acc]))

(define std-op
  Pred [X] Tail? C <- (predicate-op Pred X Tail? C)
  Op X Tail? C <- (math-op Op X Tail? C)
  = X Tail? C <- (equality-op X Tail? C)
  Op X Tail? C <- (logic-op Op X Tail? C)
  Op X Tail? C <- (order-op Op X Tail? C)
  Op X Tail? C <- (basic-op Op X Tail? C)
  Op X Tail? C <- (internal-op Op X Tail? C) where (symbol? Op)
  regkl.trap-error [X Y] Tail? C -> (emit-trap-error X Y Tail? C)
  do Body Tail? C -> (emit-do Body Tail? C [])
  fail [] _ _ -> "Shen.fail_obj"
  _ _ _ _ -> (fail))

(define mk-regs-aux
  N N _ _ Acc -> Acc
  I N C Sep Acc -> (let S (@s Acc Sep (str (context-varname C)) (str I))
                     (mk-regs-aux (+ I 1) N C ", " S)))

\* MUST be called after js-from-kl-expr since uses context-nregs which is
   updated in translation state *\
(define mk-regs
  C -> (mk-regs-aux 0 (context-nregs C) C "var " ""))

(define mk-regs-str
  C -> "" where (= (context-nregs C) 0)
  C -> (make-string "~A;~%  " (mk-regs C)))

(define mk-args-str-aux
  I I _ _ Acc -> Acc
  End I C Sep Acc -> (let F "~A~A~A = ~A[~A]"
                          N (context-argname C)
                          V (arg-name I C)
                          S (make-string F Acc Sep V N I)
                       (mk-args-str-aux End (+ I 1) C ", " S)))

(define mk-args-str
  0 _ -> ""
  N C -> (make-string "~A;~%  " (mk-args-str-aux N 0 C "var " "")))

(define emit-func-obj
  Nargs Body Closure FN -> (let N (if (or (= FN "") (empty? FN))
                                      "undefined"
                                      FN)
                                C (@s "[" (str-join Closure ", ") "]")
                             (@s "[Shen.type_func, "
                                 (str-join [Body (str Nargs) C N] ", ")
                                 "]")))

(define emit-func-closure
  Nargs Fn Argsname -> (let F "[~A, ~A, ~A, ~A]"
                         Tp "Shen.type_func"
                         (make-string F Tp Fn Nargs Argsname)))

(define emit-func-body
  Name Nargs Code C -> (let Ln (func-name Name)
                            Argname (context-argname C)
                            O (emit-func-closure Nargs Ln Argname)
                            G (make-string "if (~A.length < ~A) return ~A"
                                           (context-argname C)
                                           Nargs
                                           O)
                            F "function ~A(~A) {~%  ~A;~%  ~A~Areturn ~A}"
                            X (js-from-kl-expr Code true C)
                            \* NB: after js-from-kl-expr *\
                            R (mk-regs-str C)
                            A (mk-args-str Nargs C)
                         (make-string F Ln Argname G A R X)))

(define emit-mk-func
  Name Args Code C -> (let Fn (esc-obj (str Name))
                           Name (func-name Name)
                           Nargs (length Args)
                           N (gensym shen-user-lambda)
                           X (emit-func-body N Nargs Code C)
                           Fo (emit-func-obj Nargs X [] Fn)
                        (make-string "Shen.fns[~A] = ~A;~%" Fn Fo)))

(define emit-mk-closure
  Args Init Code C -> (let TL (context-toplevel C)
                           Arg (protect Arg)
                           Nargs (+ (length Init) (length Args))
                           C1 (mk-context 0 TL (gensym Arg) (protect R))
                           N (gensym shen-user-lambda)
                           X (emit-func-body N Nargs Code C1)
                           _ (context-toplevel-> C (context-toplevel C1))
                           A (map (/. X (js-from-kl-expr X false C)) Init)
                        (emit-func-obj Nargs X A [])))

(define emit-mk-toplevel
  Code C -> (let Name (gensym shen-js-toplevel)
                 X (emit-func-body Name 0 Code C)
              (cn "Shen.call_toplevel(" (cn X ");"))))

(define emit-thaw
  X false C -> (make-string "Shen.unwind_tail(~A)" (emit-thaw X true C))
  X true C -> (make-string "Shen.thaw(~A)" (js-from-kl-expr X false C)))

(define emit-freeze
  Init Code C -> (emit-mk-closure [] Init Code C))

(define emit-get-arg
  N C -> (arg-name N C))

(define emit-set-reg
  N X C -> (let Y (js-from-kl-expr X false C)
                _ (context-nregs-> C (max (+ N 1) (context-nregs C)))
             (make-string "(~A~A = ~A)" (context-varname C) N Y)))

(define emit-get-reg
  N C -> (make-string "~A~A" (context-varname C) N))

(define func-arg
  C X -> (js-from-kl-expr X false C))

(define emit-funcall*
  F Args true C -> (let A (map (/. X (js-from-kl-expr X false C)) Args)
                        As (str-join A ", ")
                        Call "Shen.call_tail"
                     (tail-call-ret (make-string "~A(~A, [~A])" Call F As)))
  F Args false C -> (let A (map (/. X (js-from-kl-expr X false C)) Args)
                         As (str-join A ", ")
                      (make-string "Shen.call(~A, [~A])" F As)))

(define emit-funcall
  F Args Tail? C -> (let F' (esc-obj (str F))
                         F'' (make-string "Shen.fns[~A]" F')
                      (emit-funcall* F'' Args Tail? C)))

(define js-from-kl-expr
  X Tail? C -> (let R (js-from-kl-expr* X Tail? C)
                 (if (string? R)
                     R
                     (error "ERROR: expr ~R => ~R" X R))))

(define js-from-kl-expr*
  [] _ _ -> "[]"
  X _ _ -> (str X) where (number? X)
  X _ _ -> "shen_fail_obj" where (= X (fail))
  true _ _ -> "true"
  false _ _ -> "false"
  X _ _ -> (make-string "[Shen.type_symbol, ~S]" (str X)) where (symbol? X)
  | _ _ -> (make-string "[Shen.type_symbol, ~S]" "|")
  [cons X Y] _ C -> (make-string "[Shen.type_cons, ~A, ~A]"
                                 (js-from-kl-expr X false C)
                                 (js-from-kl-expr Y false C))
  [type Value _] Tail? C -> (js-from-kl-expr Value Tail? C)
  [cond | Cases] Tail? C -> (emit-cond Cases Tail? C)
  [if Expr Then Else] Tail? C -> (emit-cond [[Expr Then] [true Else]] Tail? C)
  [freeze X] _ C -> (error "Wrong freeze code!")
  [regkl.freeze _ Init X] _ C -> (emit-freeze Init X C)

  [regkl.arg N] _ C -> (emit-get-arg N C)
  [regkl.reg N] _ C -> (emit-get-reg N C)
  [regkl.reg-> N X] _ C -> (emit-set-reg N X C)
  [regkl.func Name Args _ Code] _ C -> (emit-mk-func Name Args Code C)
  [regkl.closure Args _ Init Code] _ C -> (emit-mk-closure Args Init Code C)
  [regkl.toplevel _ _ _ Code] _ C -> (emit-mk-toplevel Code C)

  [F | A] Tail? C <- (std-op F A Tail? C)
  [[X | Y] | Args] Tail? C -> (let F (js-from-kl-expr [X | Y] false C)
                                (emit-funcall* F Args Tail? C))
  [F | Args] Tail? C -> (emit-funcall F Args Tail? C)
  X _ _ -> (esc-obj X))

(define js-from-kl-toplevel-expr
  X C -> (make-string "~A;~%" (js-from-kl-expr X false C)) where (string? X)
  X C -> (let X (js-from-kl-expr X false C)
              Regs (mk-regs-str C) \* NB: after js-from-kl-expr *\
           (if (> (context-nregs C) 0)
               (make-string "((function() {~%  ~Areturn ~A})());~%" Regs X)
               (make-string "~A;" X))))

(define js-from-kl-toplevel
  [set X V] _ C -> (@s (emit-set X V C) ";c#10;")
  [regkl.func F | _] true _ -> "" where (int-func? F)
  [regkl.func | R] _ C -> (js-from-kl-expr [regkl.func | R] true C)
  [regkl.toplevel | R] _ C -> (js-from-kl-expr [regkl.toplevel | R] true C)
  [X] _ C -> (make-string "Shen.call_toplevel(~A)~%" (esc-obj (str X)))
             where (symbol? X)
  [X] _ C -> (error "Unexpected toplevel expression: ~R~%" X)
  X _ C -> (js-from-kl-toplevel-expr X C))

(define js-from-kl-toplevel-forms
  [] _ C Acc -> (@s (context-toplevel C) "c#10;" Acc "c#10;")
  [X | Xs] Skip? C Acc -> (let X (js-from-kl-toplevel X Skip? C)
                               A (cn Acc (cn X "c#10;"))
                            (js-from-kl-toplevel-forms Xs Skip? C A)))

(define js-from-kl*
  X Skip? C -> (js-from-kl-toplevel X Skip? C))

(define js-from-kl
  X -> (let C (mk-context 0 "" (gensym (protect Arg)) (protect R))
            Rx (regkl.walk [X] false)
            X (js-from-kl-toplevel-forms Rx (value skip-internals) C "")
         (@s (context-toplevel C) "c#10;" X "c#10;")))

(define js-from-kl-forms
  X -> (let Rx (regkl.walk X false)
            C (mk-context 0 "" (gensym (protect Arg)) (protect R))
         (js-from-kl-toplevel-forms Rx (value skip-internals) C "")))

(set skip-internals true)

(define kl-from-shen
  X -> (let X (shen.walk (function macroexpand) X)
            X (if (shen.packaged? X)
                  (package-contents X)
                  X)
         (shen.elim-def (shen.proc-input+ X))))

(define js-from-shen
  X -> (js-from-kl (kl-from-shen X)))

(set *silence* false)

(define translate-file
  File -> (js-from-kl-forms (kl-from-shen (read-file File))))

(define file-extension?
  "" Ext -> false
  (@s C Ext) Ext -> true
  (@s C Cs) Ext -> (file-extension? Cs Ext))

(define translate-files'
  [] S -> S
  [File | Files] S -> (translate-files' Files (cn S (translate-file File)))
                      where (or (file-extension? File ".kl")
                                (file-extension? File ".shen"))
  [File | Files] S -> (translate-files' Files
                                        (cn S (read-file-as-string File))))

(define translate-files
  Files -> (translate-files' Files ""))

(define js.dump
  Files Target -> (do (write-to-file Target (translate-files Files))
                      true))
)
