(in-package :chariot)

(set-armish-string-encoding :latin-1)

;; TODO: global var should be removed. base addres should only be defined in block.
(setf *base-address* #x02000000)

(def-space-n-clusters-n-blocks chariot
  ((chariot-ds :base-address *base-address*
                 :blocks (chariot-core liards::liards-common liards::liards-writer)))
  (chariot-core))

;; forth register aliases
(defmacro set-fth-regs (&body regs)
  "just a shortcut to define register globals"
  `(progn ,@(loop for i-spec in regs
               collect `(defparameter ,(car i-spec) (quote ,(cadr i-spec))))))

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

(setf *jr* 'r0)               ; because it's r9 in mandel. So completely random.

(setf *base-address* #x02000000)

(defparameter *ps-base* (+ *base-address* #x10000))
(defparameter *tib-base* (+ *ps-base* #x1000))
(defparameter *rs-base* (+ *tib-base* #x4000))
(defparameter *tob-base* *rs-base*)
(defparameter *tob-max* (+ *tob-base* #x1000))

(defvar *imm-flag* #x80)
(defvar *hidden-flag* #x20)
(defvar *lenmask-flag* #x1f)

(defparameter *forth-source-dir*
  (merge-pathnames #p"forth-src" 
                   (asdf:component-pathname (asdf:find-system :chariot))))