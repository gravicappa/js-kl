(package js [this]

(define cut-package'
  "" W -> W
  (@s "." Cs) W -> (cut-package' Cs "")
  (@s C Cs) W -> (cut-package' Cs (@s W C)))

(define cut-package
  S -> (cut-package' S "") where (string? S)
  S -> (intern (cut-package' (str S) "")))

(define item
  X _ -> (cut-package (str X)) where (symbol? X)
  X _ -> (str X) where (number? X)
  X _ -> (esc-obj X) where (string? X)
  [X | Xs] C -> (expr [X | Xs] 1 C))

(define ffi-call-args'
  [] _ R -> R
  [X | Xs] C R -> (ffi-call-args' Xs C (@s R ", " (ffi-expr X C))))

(define ffi-call-args
  [] _ -> "()"
  [X] C -> (@s "(" (ffi-expr X C) ")")
  [X | Xs] C -> (@s "(" (ffi-expr X C) (ffi-call-args' C Xs "") ")"))

(define ffi-call
  F Args C -> (cn (ffi-expr F C) (ffi-call-args Args C)))

(define ffi-new
  Class Args C -> (cn "new " (ffi-call Class Args C)))

(define ffi-obj-key
  X -> (str X) where (symbol? X)
  X -> (error "~A is not appropriate js object key" X))

(define ffi-obj'
  [] _ Pairs -> (@s "{" (arg-list (reverse Pairs)) "}")
  [K V | Items] C Pairs -> (let Pair (@s (ffi-obj-key K) ": " (ffi-expr V C))
                             (ffi-obj' Items C [Pair | Pairs])))

(define ffi-obj
  Items C -> (ffi-obj' Items C []))

(define ffi-arr
  Items C -> (@s "[" (arg-list (map (/. X (ffi-expr X C)) Items)) "]"))

(define ffi-set
  Dst Src C -> (@s "(" (ffi-expr Dst C) " = " (ffi-expr Src C) ")" ))

(define ffi-chain-item
  X C R -> (@s R "." (item X C)) where (symbol? X)
  X C R -> (@s R "[" (ffi-expr X C) "]"))

(define ffi-chain
  [] _ R -> R
  [[js.call F | A] | Xs] C R -> (ffi-chain Xs C (cn (ffi-chain-item F C R)
                                                    (ffi-call-args A C)))
  [X | Xs] C R -> (ffi-chain Xs C (ffi-chain-item X C R)))

(define ffi-func-obj
  F _ -> (esc-obj (str F)) where (symbol? F)
  [function F] _ -> (esc-obj (str F)) where (symbol? F)
  F C -> (expr F 1 C))

(define ffi-func'
  Js-args F Shen-args C -> (@s "function(" (arg-list Js-args)
                               ") {return vm.call(" (ffi-func-obj F C) ", ["
                               (arg-list Shen-args) "]);}"))

(define ffi-func
  [X | Args] F C -> (ffi-func' Args F [this | Args] C)
                  where (= (cut-package X) this)
  Args F C -> (ffi-func' Args F Args C))

(define ffi-expr
  [js. X | Xs] C -> (ffi-chain Xs C (ffi-expr X C))
  [js.call F | Args] C -> (ffi-call F Args C)
  [js.set Dst Src] C -> (ffi-set Dst Src C)
  [js.new Class | Args] C -> (ffi-new Class Args C)
  [js.obj | Xs] C -> (ffi-obj Xs C)
  [js.arr | Xs] C -> (ffi-arr Xs C)
  [js.fn Args F] C -> (ffi-func Args F C)
  X C -> (item X C))

(define klvm-sym-str?
  (@s "klvm." X) -> true
  _ -> false)

(define klvm-expr?
  [X | Y] -> true where (and (symbol? X)
                             (klvm-sym-str? (str X)))
  _ -> false)

(define normalize-chain-item
  [] -> []
  X -> X where (klvm-expr? X)
  [X | Xs] -> [X | (map (function normalize-chain-item) Xs)]
              where (element? X [js. js.call js.set js.new js.obj js.arr
                                 js.fn])
  [X | Xs] -> [js.call | (map (function normalize-chain-item) [X | Xs])]
  X -> (cut-package X) where (and (symbol? X)
                                  (not (element? X [js. js.call js.set js.new
                                                    js.obj js.arr js.fn])))
  X -> X)

(define normalize-chain
  X -> (map (function normalize-chain-item) X))

(define chain-macro
  [js. | X] -> (normalize-chain [js. | X])
  [js.call | X] -> (normalize-chain [js.call | X])
  [js.set | X] -> (normalize-chain [js.set | X])
  [js.new | X] -> (normalize-chain [new | X])
  [js.obj | X] -> (normalize-chain [obj | X])
  [js.arr | X] -> (normalize-chain [arr | X])
  [js.fn | X] -> (normalize-chain [fn | X])
  X -> X))
