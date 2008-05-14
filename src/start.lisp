(in-package :chariot)

;; helpers
(defmacro set-fth-regs (&body regs)
  "just a shortcut to define register globals"
  `(progn ,@(loop for i-spec in regs
              collect `(defparameter ,(car i-spec) (quote ,(cadr i-spec))))))

(defmacro def-fth-prim (name &body instrs)
  "A wrapper for forth primitive definitions. Not so sure yet how to frame this mess."
  `(def-asm-fn ,name ()
     (emit-asm ,@instrs)))

(setf *jr* 'r9)  ; because it's r9 in mandel. So completely random.

;; forth register aliases
(set-fth-regs
  (pc r15) ; instruction pointer
  (rp r14) ; return pointer
  (sp r13) ; variable stack
  (rs r12) ; return stack
  (dp r10) ; pointer to last dictionary entry
  (jr  r9) ; for referencing user variables
  (tib r8) ; terminal input buffer pointer
  (ip  r7) ; points to next word to be executed in chain of cfa(code field address)'s
  (w   r6) ; word address register. first copy of cfa then points to p[arameter]fa.
  ) 


;;;; sketch of forth-impl. related mem. area
;;   taken from the figforth book "forth for professionals"

;;   --------------   higher mem
;;    return stack
;;         |
;;         V           (rp)
;;
;;         ^           (tib)
;;         |
;;    terminal input
;;    buffer (tib)
;;   --------------
;;    parameter
;;    stack
;;         |
;;         V           (sp)
;;
;;         ^           (dp)
;;         |
;;    dictionary
;;    (extended)
;;   --------------
;;
;;    dictionary
;;    (predefined)
;;
;;   --------------
;;
;;    user variables   (jr)  (from jr the variables are referenced)
;;
;;   --------------
;;
;;    trace/scratch   didn't this one involve a pointer
;;    area            can't remember
;;
;;   --------------
;;
;;    assembly/
;;    system
;;    routines
;;
;;   --------------
;;
;;    bootstrap
;;    instructions
;;
;;   --------------

(def-fth-prim bla
  (ldr jr 2))