(use-modules [javascript])

(trap-error
  (value top-dir)
  (/. E
      (set top-dir "modules/javascript/")))

(set src-dir (cn (value top-dir) ""))
(set dst-dir (cn (value top-dir) "js/"))

(set files ["kl/core.kl"
            "kl/declarations.kl"
            "kl/load.kl"
            "kl/macros.kl"
            "kl/prolog.kl"
            "kl/reader.kl"
            "kl/sequent.kl"
            "kl/sys.kl"
            "kl/toplevel.kl"
            "kl/track.kl"
            "kl/t-star.kl"
            "kl/types.kl"
            "kl/writer.kl"
            "kl/yacc.kl"])

(define string-reverse*
  {string --> string --> string}
  "" A -> A
  (@s C S) A -> (string-reverse* S (cn C A)))

(define string-reverse
  {string --> string}
  S -> (string-reverse* S ""))

(define path-strip-extension*
  {string --> string --> string}
  S "" -> S
  S (@s "/" _) -> S
  _ (@s "." R) -> (string-reverse R)
  S (@s C R) -> (path-strip-extension* S R))

(define path-strip-extension
  {string --> string}
  F -> (path-strip-extension* F (string-reverse F)))

(define path-tail*
  {string --> string --> string}
  S "" A -> A
  _ (@s "/" R) "" -> (string-reverse R)
  _ (@s "/" R) A -> A
  S (@s C R) A -> (path-tail* S R (@s C A)))

(define path-tail
  {string --> string}
  F -> (path-tail* F (string-reverse F) ""))

(define write-string
  X P Out -> (trap-error (do (pr (pos X P) Out)
                             (write-string X (+ P 1) Out))
                         (/. E true)))

(define process-file
  Src -> (let Dst (make-string "~A~A.js"
                               (value dst-dir)
                               (path-strip-extension (path-tail Src)))
              Src (cn (value src-dir) Src)
           (do (output "~A -> ~A~%" Src Dst)
               (js-dump-to-file (read-file Src) Dst))))

(define unwind-protect
  F End -> (trap-error (let R (thaw F)
                         (do (thaw End)
                         R))
                       (/. E (do (thaw End)
                                 (error (error-to-string E))))))

(define call-with-install-flags
  Mpss Inst F -> (let Prev (value *maximum-print-sequence-size*)
                   (unwind-protect
                     (freeze (do (set shen-*installing-kl* Inst)
                                 (set *maximum-print-sequence-size* Mpss)
                                 (thaw F)))
                     (freeze (do (set shen-*installing-kl* false)
                                 (set *maximum-print-sequence-size* Prev))))))

(trap-error (do (shen-out)
                (set kl-from-shen (function shen-out)))
            (/. E (set kl-from-shen (function shen-shen-out))))

(define dump-js
  Dir Src -> (let Dst (make-string "~A/~A.js" Dir (path-strip-extension
                                                 (path-tail Src)))
                  Kl (map (value kl-from-shen) (read-file Src))
                  R (map (/. X (write-string (make-string "~R~%~%" X) 0 F))
                         Kl)
               (do (output "~A -> ~A~%" Src Dst)
                   (js-dump-to-file Kl Dst))))

(register-dumper js all dump-js)
(dump-module javascript js cli (value dst-dir))

(call-with-install-flags
  -1 true (freeze (load (cn (value src-dir) "mkprim.shen"))))
(call-with-install-flags -1 true (freeze (map (function process-file)
                                              (value files))))
