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

(set-asm-init-routines
  (emit-asm
   
   ;; var to hold the last link address in the word chain
   (def-asm-param link 0)
   (b :init-at-end)))

(defun make-eval-forth (&optional fth-file)
  (let ((fth-content (strcat (read-string-from-file (concatenate 'string (namestring *forth-source-dir*) "/"  "lib.fth") :external-format :latin-1)
                             " "
                             (when fth-file
                               (read-string-from-file (concatenate 'string (namestring *forth-source-dir*) "/"  fth-file) :external-format :latin-1)))))
    (set-asm-final-routines
      (emit-asm
       
       :init-at-end
       ;; SETUP CODE

       ;; setup jr
       (ldr jr (address :jr-base))
       
       ;; init screens
       (b-and-l :init-system)

       ;; setting up the chariot regs
       (ldr sb *ps-base*)
       (mov sp sb)
       (ldr tib *tib-base*)
       (ldr rb *rs-base*)
       (mov rp rb)
       (ldr ip (address :ip-start)) ;; put ip at beginning of simulated word

       ;; setting up user-vars
       ;; latest
       (ldr tmp-2 (ia link))
       (store-jr tmp-2 latest)
       ;; here
       (ldr tmp-2 (address :code-end))
       (add tmp-2 tmp-2 4) ;; skip past the eternal loop that is now the arm7 code. this is a hack 
       (store-jr tmp-2 here)

       ;; enter the forth interpret loop
       next
       

       ;; SETUP DATA
       
       ;; jr reachables
       (dump-jr-data)
       align

       pool ;; just to be sure. perhaps the tmp tib becomes to big for ldr to fetch it's data

       
       ;; font writing data
       :char-x-data
       (bin 8 *char-x-data*)
       align
       
       :char-y-data
       (bin 8 *char-y-data*)
       align

       :char-sizes
       (bin 8 *char-sizes*)
       align

       :char-widths
       (bin 8 *char-widths*)
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
       " CR ETERNAL " ;; so we don't have to think about appending a space to get a valid last input word
       :tib-top
       align)))
  
  (armish::get-asm-space 'chariot)
    (liards::nds-compile
   (assemble 'arm9 'arm (emit-arm-fns (armish::get-cluster 'chariot-ds :in 'chariot)))
   liards::*arm7-bin*
   "fuck-around.nds"))
