(define js-test.prim
  -> (let Dir/ "shen-js/"
          . (write-to-file (@s Dir/ "primitives.js") (js.generate-primitives))
       true))

(define js-test.t1
  -> (let Code (value klvm-test.*code1*)
          Dir/ "shen-js/tests/"
          Js (js.from-kl Code)
          . (write-to-file (@s Dir/ "test1.js") Js)
        true))
