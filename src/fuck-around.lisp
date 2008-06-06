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
  ;; word find break >dfa
  4 ;; lit will be automatically prepended
  )

(set-asm-final-routines
  (emit-asm

   :init-at-end
   ;; SETUP CODE
   ;; setting up the regs we need to set up
   (ldr sb *ps-base*)
   (mov sp sb)
   (ldr tib *tib-base*)
   (ldr rb *rs-base*)
   (mov rp rb)

   ;; setting up user-vars
   ;; latest
   (ldr tmp-1 (address :latest-var))
   (ldr tmp-2 (ia link))
   (str tmp-2 (tmp-1))
   ;; here
   (ldr tmp-1 (address :here-var))
   (ldr tmp-2 (address :code-end))
   (add tmp-2 tmp-2 4) ;; skip past the eternal loop that is now the arm7 code. this is a hack 
   (str tmp-2 (tmp-1))

   
   ;; preparing the stack for testing functionality
   (ldr tmp-2 (address :tib-base))
   (push-ps tmp-2)

   ;; put ip at beginning of simulated word
   (ldr ip (address :words))

   (b :start-forth) ;; first we jump over non-instructions

   
   ;; SETUP DATA
   pool
   
   ;; simulating part of a word
   :words

   (word (address :test-word))
   (word (address :eternal))

   
   ;; temporary hackish tib-base
   :tib-base
   "KEY "
   :tib-top
   align
   
   ;; ENTRY POINT
   :start-forth
   ;; then we break the debugger JUST before the test-code
   
   next))

(set-asm-init-routines
  (emit-asm
   ;; var to hold the last link address in the word chain
   (def-asm-param link 0)

   (b :init-at-end)))