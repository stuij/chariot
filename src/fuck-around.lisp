(in-package :chariot)

;; (def-space-n-blocks fuck-around
;;   (fa-block :base-address 10000)) ;; this base address not used atm

;; (in-asm-space fuck-around)
;; (in-block fa-block)

;; *current-asm-block*

;; (assemble 'arm9 'arm '((and tmp-4 tmp-4 tmp-5)))

;; (clear-current-block)

#- (and) (liards::nds-compile
          (assemble 'arm9 'arm (emit-arm-fns))
          liards::*arm7-bin*
          "fuck-around.nds")

(defword test-word ()
  lit 0 break 0branch -4 break
  ;; word find break >cfa
  ;; lit won't be automatically prepended
  )

(make-eval-forth "7 6 -")