(package js [this]

(define cut-package'
  "" W -> W
  (@s "." Cs) W -> (cut-package' Cs "")
  (@s C Cs) W -> (cut-package' Cs (@s W C)))

(define cut-package
  S -> (cut-package' S "") where (string? S)
  S -> (intern (cut-package' (str S) "")))

(define klvm-sym-str?
  (@s "klvm." X) -> true
  _ -> false)

(define klvm-expr?
  [X | Y] -> true where (and (symbol? X)
                             (klvm-sym-str? (str X)))
  _ -> false)

(define flatten'
  [] R -> R
  [[X | Y] | Xs] R -> (flatten' Xs [} | (flatten' [X | Y] [{ | R])])
  [X | Xs] R -> (flatten' Xs [X | R]))

(define flatten
  X -> (reverse (flatten' X [])))

(defcc <item>
  Item := Item where (not (element? Item [{ }]));)

(defcc <tag>
  Tag := Tag where (symbol? Tag))

(defcc <tree-item>
  { <tag> <tree> } := [<tag> | <tree>];
  <item> := <item>;)

(defcc <tree>
  <tree-item> <tree> := [<tree-item> | <tree>];
  <e> := [];)

(define item
  X -> (str X) where (number? X)
  X -> (cut-package (str X)) where (symbol? X)
  X -> (esc-obj X) where (string? X)
  X -> (expr2 X))

(define ffi-call-args'
  [] R -> R
  [X | Xs] R -> (ffi-call-args' Xs (@s R ", " (ffi-expr X))))

(define ffi-call-args
  [] -> "()"
  [X] -> (@s "(" (ffi-expr X) ")")
  [X | Xs] -> (@s "(" (ffi-expr X) (ffi-call-args' Xs "") ")"))

(define ffi-call
  F Args -> (cn (ffi-expr F) (ffi-call-args Args)))

(define ffi-new
  Class Args -> (cn "new " (ffi-call Class Args)))

(define ffi-obj-key
  X -> (str X) where (symbol? X)
  X -> (error "~A is not appropriate js object key" X))

(define ffi-obj'
  [] Pairs -> (@s "{" (arg-list (reverse Pairs)) "}")
  [K V | Items] Pairs -> (let Pair (@s (ffi-obj-key K) ": " (ffi-expr V))
                           (ffi-obj' Items [Pair | Pairs])))

(define ffi-obj
  Items -> (ffi-obj' Items []))

(define ffi-arr
  Items -> (@s "[" (arg-list (map (function ffi-expr) Items)) "]"))

(define ffi-set
  Dst Src -> (@s (ffi-expr Dst) " = " (ffi-expr Src)))

(define ffi-chain-item
  X R -> (@s R "." (item X)) where (symbol? X)
  X R -> (@s R "[" (ffi-expr X) "]"))

(define ffi-chain
  [] R -> R
  [[js.call F | A] | Xs] R -> (ffi-chain Xs (cn (ffi-chain-item F R)
                                                (ffi-call-args A)))
  [X | Xs] R -> (ffi-chain Xs (ffi-chain-item X R)))

(define ffi-expr
  [. X | Xs] -> (ffi-chain Xs (ffi-expr X))
  [js.call F | Args] -> (ffi-call F Args)
  [js.set Dst Src] -> (ffi-set Dst Src)
  [js.new Class | Args] -> (ffi-new Class Args)
  [js.obj | Xs] -> (ffi-obj Xs)
  [js.arr | Xs] -> (ffi-arr Xs)
  [js.fn Args F] -> (ffi-func Args F)
  X -> (item X))

(define ffi-func-obj
  F -> (esc-obj (str F)) where (symbol? F)
  [function F] -> (esc-obj (str F)) where (symbol? F)
  F -> (expr2 F))

(define ffi-func'
  Js-args F Shen-args -> (@s "function(" (arg-list Js-args)
                              ") {return vm.call(" (ffi-func-obj F) ", ["
                              (arg-list Shen-args) "]);}"))

(define ffi-func
  [X | Args] F -> (ffi-func' Args F [this | Args])
                  where (= (cut-package X) this)
  Args F -> (ffi-func' Args F Args))

(define normalize-ffi-macro
  [] -> []
  X -> X where (klvm-expr? X)
  [X | Xs] -> [X | (map (function normalize-ffi-macro) Xs)]
              where (element? X [. js.call js.set new obj arr])
  [X | Xs] -> [js.call | (map (function normalize-ffi-macro) [X | Xs])]
  X -> X)

(define chain-macro-fn
  [X | Xs] -> (flatten (normalize-ffi-macro [X | Xs])))

(defmacro chain-macro
  [. | X] -> (chain-macro-fn [. | X])
  [js.call | X] -> (chain-macro-fn [js.call | X])
  [js.set | X] -> (chain-macro-fn [js.set | X])
  [new | X] -> (chain-macro-fn [new | X])
  [obj | X] -> (chain-macro-fn [obj | X])
  [arr | X] -> (chain-macro-fn [arr | X])
  [fn | X] -> (chain-macro-fn [fn | X])))
