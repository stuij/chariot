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

;; when done with testing, put asm init and final routines here

;; general asm
;; macros
(def-asm-macro-lite next
  (ldr w (ip) 4)   ;; load cfa of next word in w and point ip to next word
  (ldr w (w))      ;; load pfa/codeword or assembly of that next word
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

;; fns
(def-asm-fn docol
  (push-rs ip)
  (add w w 4) ;; w already points to codeword of word thanks to next. Increment to point to first word in definition
  (mov ip w)  ;; put word in ip so we can call next on it
  next)
