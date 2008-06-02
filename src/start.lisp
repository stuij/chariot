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


;; setup

(def-asm-space chariot)
(def-block chariot-setup :in chariot)


(def-space-n-blocks chariot
  (chariot-setup :base-address 0))

(in-asm-space chariot)

;; helpers
(defmacro set-fth-regs (&body regs)
  "just a shortcut to define register globals"
  `(progn ,@(loop for i-spec in regs
               collect `(defparameter ,(car i-spec) (quote ,(cadr i-spec))))))

(defmacro def-fth-prim (name &body instrs)
  "A wrapper for forth primitive definitions. Not so sure yet how to frame this mess."
  `(def-asm-fn-raw ,name ()
     (gather ,@instrs)))

(setf *jr* 'r9)               ; because it's r9 in mandel. So completely random.

;; forth register aliases
(set-fth-regs
  (pc  pc)              ; instruction pointer (as in hardware, not the forth ip)
  (lr r14)                              ; hw link register
  (sp r13)                              ; parameter stack pointer
  (sb r12)                              ; parameter stack base
  (tib r11)                             ; terminal input buffer pointer
  (rp r10)                              ; return stack pointer
  (rb r9)                               ; return stack base
  (dp r8)                               ; pointer to last dictionary entry
  (ip r7) ; points to next word to be executed in chain of cfa(code field address)'s
  (w  r6) ; word address register. first points to cfa then points to p[arameter]fa/code.
  (tmp-5 r5)
  (tmp-4 r4)
  (tmp-3 r3)
  (tmp-2 r2)
  (tmp-1 r1)
  (jr r0)                               ; for referencing user variables
  ) 



(setf *base-address* #x02000000)

(defparameter *ps-base* (+ *base-address* #x10000))
(defparameter *tib-base* (+ *ps-base* #x1000))
(defparameter *rs-base* (+ *tib-base* #x4000))
(defvar imm-flag #x80)
(defvar hidden-flag #x20)
(defvar lenmask-flag #x1f)


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
   
   (b :init-fn) ;; to be defined
   ;; first we jump over the pool
   pool
   ))


;; general asm
(def-asm-macro-lite next
  (ldr w (ip) 4)   ;; load cfa of next word in w and point ip to next word
  (ldr w (w))      ;; load pfa/codeword or assembly of that next word
  (mov pc w))      ;; branch to it

(def-asm-fn docol
  (push-rs ip)
  (add w w 4) ;; w already points to codeword of word thanks to next. Increment to point to first word in definition
  (mov ip w)  ;; put word in ip so we can call next on it
  next)

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