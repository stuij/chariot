(in-package :chariot)

(def-space-n-blocks fuck-around
  (fa-block :base-address 10000)) ;; this base address not used atm

(in-asm-space fuck-around)
(in-block fa-block)

*current-asm-block*

(assemble 'arm9 'arm '((movmi r3 r4))) 

(clear-current-block)

(liards::nds-compile
 (assemble 'arm9 'arm (emit-arm-fns))
 liards::*arm7-bin*
 "fuck-around.nds")

(set-asm-init-routines
  (emit-asm
   (bkpt 0)
   (bl :link)
   :link
   (str r14 (r10 -4)!)
   (mov r1 1)
   (mov r2 2)
   (mov r3 3)
   :loop
   (b :loop)))

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

   (word (address :word))
   (word (address :eternal))

   pool

   :tib-base
   "                      \\uncouch 
       cow jumps over the wretched hen, and eats her alive afterwards"
   :tib-top
   align
   
   :init-break
   ;; then we break the debugger JUST before the test-code
   (bkpt 0)

   next))
