(in-package :chariot)

;;;; sketch of forth-impl. related mem. area
;;   taken from the figforth book "forth for professionals"

;;   --------------    higher mem
;;    return stack
;;         |
;;         V           (rp)
;;
;;         ^           (tib)
;;         |
;;    terminal input
;;    buffer (tib)
;;   --------------
;;
;;    trace/scratch   didn't this one involve a pointer
;;    area            can't remember
;;
;;   --------------
;;    parameter
;;    stack
;;         |
;;         V           (sp)
;;
;;         ^           (dp)
;;         |
;;         |
;;    dictionary
;;    (extended)
;;
;;   --------------
;;
;;
;;    dictionary
;;    (predefined)
;;
;;
;;   --------------
;;
;;    user variables   (jr)  (from jr the variables are referenced)
;;
;;   --------------
;;
;;    fixed data
;;
;;   --------------
;;
;;    assembly/
;;    system
;;    routines
;;
;;   --------------

(in-asm-space chariot)
(in-block chariot-core)

;; general asm
;; macros
(def-asm-macro-lite next
  (ldr tmp-5 (ip) 4)   ;; load cfa of next word in w and point ip to next word
  (ldr w (tmp-5))      ;; load pfa/codeword or assembly of that next word
  (mov pc w))          ;; branch to it

;; fns
(def-asm-fn %docol
  (push-rs ip)
  (add w tmp-5 4) ;; increment to point to first word in definition (to which tmp-5 still references from the previous next)
  (mov ip w)      ;; put word in ip so we can call next on it
  next)

(set-asm-init-routines
  (emit-asm
   ;; var to hold the last link address in the word chain
   (def-asm-param link 0)

   (b :init-at-end)))

(defun make-eval-forth (string)
  (setf *simul-input* string)
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

     ;; put ip at beginning of simulated word
     (ldr ip (address :ip-start))
     next

     ;; SETUP DATA   

     pool ;; just to be sure. perhaps the tmp tib becomes to big for ldr to fetch it's data

     ;; start of ip
     :ip-start
     (word (address :quit))

     ;; temporary hackish tib-base. For now functions at uninteractive input prompt
     :tib-base
     (ea *simul-input*)
     " ETERNAL " ;; so we don't have to think about appending a space to get a valid last input word
     :tib-top
     align))
  (liards::nds-compile
   (assemble 'arm9 'arm (emit-arm-fns))
   liards::*arm7-bin*
   "fuck-around.nds"))

       ;; SETUP DATA
       
       ;; jr reachables
       (liards::dump-jr-data)
       align

       pool ;; just to be sure. perhaps the tmp tib becomes to big for ldr to fetch it's data

       
       ;; font writing data
       :char-x-data
       (bin 8 liards::*char-x-data*)
       align
       
       :char-y-data
       (bin 8 liards::*char-y-data*)
       align

       :char-sizes
       (bin 8 liards::*char-sizes*)
       align

       :char-widths
       (bin 8 liards::*char-widths*)
       align

       :char-offsets
       (bin 32 liards::*char-offsets*)
       align

       
       ;; start of ip
       :ip-start
       (word (address :quit))

       
       ;; temporary hackish tib-base. For now functions at uninteractive input prompt
       :tib-base
       (ea fth-content)
       " ETERNAL " ;; so we don't have to think about appending a space to get a valid last input word
       :tib-top
       align)))
  
  (armish::get-asm-space 'chariot)
  
  (liards::nds-compile
   (assemble 'arm9 'arm (emit-arm-fns (armish::get-cluster 'chariot-ds :in 'chariot)))
   liards::*arm7-bin*
   "fuck-around.nds"))
