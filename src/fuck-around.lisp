(in-package :chariot)

(def-space-n-blocks fuck-around
  (fa-block :base-address 10000)) ;; this base address not used atm

(in-asm-space fuck-around)
(in-block fa-block)

(clear-current-block)

*current-asm-block* 

(liards::nds-compile
 (assemble 'arm9 'arm (emit-arm-fns))
 liards::*arm7-bin*
 "fuck-around.nds")

(assemble 'arm9 'arm '(:blob (ldr r7 (address :blob))))

(set-asm-init-routines
  (emit-asm
   ;; var to hold the last link address in the word chain
   (def-asm-param link 0)

   ;; setting up the regs we need to set up
   (ldr sb *ps-base*)
   (mov sp sb)
   (ldr tib *tib-base*)
   (ldr rb *rs-base*)
   (mov rp rb)

   ;; to test next functionality. put ip at beginning of simulated word
   (ldr ip (address :words))

   (ldr tmp-1 1)
   (ldr tmp-2 2)
   (ldr tmp-3 3)
   
   (push-ps tmp-1)
   (push-ps tmp-2)
   (push-ps tmp-3)

   (b :init-break) ;; first we jump over non-instructions

   ;; simulating part of a word
   :words
   (word (address :test1))
   (word (address :test2))
   
   pool
   
   :init-break
   ;; then we break the debugger JUST before the test-code
   (bkpt 1)

   next))

(defcode test1 ()
  (pop-ps tmp-1))

(defcode test2 ()
  (pop-ps tmp-1)
  (pop-ps tmp-1))