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
(in-block chariot-setup)

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

;; general asm
;; macros
(def-asm-macro-lite next
  (ldr tmp-5 (ip) 4)   ;; load cfa of next word in w and point ip to next word
  (ldr w (tmp-5))      ;; load pfa/codeword or assembly of that next word
  (mov pc w))      ;; branch to it

(def-asm-macro push-rs (reg &key cond)
  ;; todo: needs bounds checking but can't be bothered with error handling right now
  (let ((store (if cond
                   (concat-symbol 'str cond)
                   'str)))
    `((,store ,reg (rp -4)!))))

(def-asm-macro pop-rs (reg &key cond)
  ;; todo: needs bounds checking but can't be bothered with error handling right now
  (let ((load (if cond
                  (concat-symbol 'ldr cond)
                  'ldr)))
    `((,load ,reg (rp) 4))))

(def-asm-macro push-ps (reg &key cond)
  ;; todo: needs bounds checking but can't be bothered with error handling right now
  (let ((store (if cond
                   (concat-symbol 'str cond)
                   'str)))
    `((,store ,reg (sp -4)!))))

(def-asm-macro pop-ps (reg &key cond)
  ;; todo: needs bounds checking but can't be bothered with error handling right now
  (let ((load (if cond
                  (concat-symbol 'ldr cond)
                  'ldr)))
    `((,load ,reg (sp) 4))))

;; to circumvent no$gba problems
(def-asm-macro b-and-l (label)
  `((mov lr pc)
    (b ,label)))

(def-asm-macro b-and-l-ne (label)
  `((movne lr pc)
    (bne ,label)))

;; fns
(def-asm-fn %docol
  (push-rs ip)
  (add w tmp-5 4) ;; increment to point to first word in definition (to which tmp-5 still references from the previous next)
  (mov ip w)  ;; put word in ip so we can call next on it
  next)
