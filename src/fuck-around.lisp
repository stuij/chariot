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

(assemble 'arm9 'arm '((def-asm-param link 0) (set-asm-param link (address :blob))
                       :blob (word link)))

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

   (b :init-break) ;; first we jump over non-instructions

   ;; simulating part of a word
   :words
   (word (address :test1))
   (word (address :test2))
   (word (address :test3))
   (word (address :test4))
      
   pool
   
   :init-break
   ;; then we break the debugger JUST before the test-code
   (bkpt 0)

   next))

(def-forth-var test1 () 3)
(def-forth-var test2 ())

(def-forth-const test3 () imm-flag)
(def-forth-const test4 () (address :test1))


(defcode test1 ()
  (pop-ps tmp-1))

(defcode test2 ()
  (pop-ps tmp-1)
  (pop-ps tmp-1))