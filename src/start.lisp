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

;; helpers
(defmacro set-fth-regs (&body regs)
  "just a shortcut to define register globals"
  `(progn ,@(loop for i-spec in regs
               collect `(defparameter ,(car i-spec) (quote ,(cadr i-spec))))))

(defmacro def-fth-prim (name &body instrs)
  "A wrapper for forth primitive definitions. Not so sure yet how to frame this mess."
  `(def-asm-fn ,name ()
     (gather ,@instrs)))

(setf *jr* 'r9)               ; because it's r9 in mandel. So completely random.

;; forth register aliases
(set-fth-regs
  (pc  pc)              ; instruction pointer (as in hardware, not the forth ip)
  (lr r14)                              ; hw link register
  (sp r13)                              ; parameter stack pointer
  (stack-base r12)                      ; parameter stack base
  (rp r11)                              ; return stack pointer
  (return-stack-base r10)               ; return stack base
  (dp r9)                               ; pointer to last dictionary entry
  (jr r8)                               ; for referencing user variables
  (ip r7) ; points to next word to be executed in chain of cfa(code field address)'s
  (w  r6) ; word address register. first points to cfa then points to p[arameter]fa/code.
  (tmp-1 r5)
  (tmp-2 r4)
  (tmp-3 r3)
  (tmp-4 r2)) 

(defvar *return-stack-base-addr*)
(defvar *parameter-stack-base-addr*)
(defvar tib-base)
(defvar imm-flag #x80)
(defvar hidden-flag #x20)
(defvar lenmask-flag #x1f)


(setf *base-address* #x200000)

(set-asm-init-routines
  (emit-asm
   ;; first we make some global variables
  
   ;; var to hold the last link address in the word chain
   (def-asm-param link 0)))

(def-asm-fn-lite tmp
  ;; just some to be referenced areas which still need a proper place in some way or form
  :tib-base
  )

(def-asm-fn bla ()
  (emit-asm
   (def-asm-param imm-flag #x80)))

(def-asm-macro-lite next
  (ldr w (ip) 4) ;; load cfa of next word in w and point ip to next word
  (ldr w w)      ;; load pfa/codeword or assembly of that next word
  (mov pc w))    ;; branch to it

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

(def-asm-fn-lite docol
  (push-rs ip)
  (add w w 4) ;; w already points to codeword of word thanks to next. Increment to point to first word in definition
  (mov ip w)  ;; put word in ip so we can call next on it
  next)

;; forth words

;; some macros
(defparameter *forth-words* (make-hash-table :test 'equal))

(defmacro defword-builder (name (&key (flags 0) forth-name) &rest body)
  (let* ((word-name (if forth-name
                        forth-name
                        (symbol-name name)))
         (word-length (length word-name))
         (label (intern (symbol-name name) :keyword))
         (link-label (intern (symbol-name (concat-symbol label "-LINK")) :keyword)))
    (assert (<= word-length 32) (word-length)
            "word lenght of ~a is bigger than 32" word-length)
    `(progn
       (setf (gethash ,word-name *forth-words*) (cons ,label ,link-label))
       (def-asm-fn-lite ,(intern (symbol-name link-label))
         (word link)
         (byte ,(+ flags word-length))
         (string ,word-name)
         (align)
         ,label
         ,@body
         (set-asm-param link (address ,label))))))

(defmacro defword (name more-params &body words)
  (let ((word-list (loop for word in words
                      collect `(word (address ,(intern (symbol-name word) :keyword))))))
    `(defword-builder ,name ,more-params
       (word (address :docol))
       ,@word-list
       (word (address :exit)))))

(defmacro defcode (name more-params &body code)
  (let* ((label (intern (symbol-name name) :keyword))
         (code-label (intern (symbol-name (concat-symbol label "-CODE")) :keyword)))
    `(defword-builder ,name ,more-params
       (word (address ,code-label))
       ,code-label
       ,@code
       next)))

(defmacro def-forth-var (name more-params &optional val)
  (let* ((label (intern (symbol-name name) :keyword))
         (code-label (intern (symbol-name (concat-symbol label "-CODE")) :keyword))
         (var-label (intern (symbol-name (concat-symbol label "-VAR")) :keyword)))
    `(defword-builder ,name ,more-params
       (word (address ,code-label))
       ,code-label
       (ldr tmp-1 (address ,var-label))
       (push-ps tmp-1)
       next
       pool
       ,var-label
       (word ,val))))

(defmacro def-forth-const (name more-params &optional val)
  (let* ((label (intern (symbol-name name) :keyword))
         (code-label (intern (symbol-name (concat-symbol label "-CODE")) :keyword)))
    `(defword-builder ,name ,more-params
       (word (address ,code-label))
       ,code-label
       (push-ps ,val)
       next
       pool)))

;; built-in variables
(def-forth-var state ())
(def-forth-var latest ())
(def-forth-var here ())
(def-forth-var base () 10)

;; constants
(def-forth-const version () 1)
(def-forth-const docol () (address :docol))
(def-forth-const imm-flag () imm-flag)
(def-forth-const hidden-flag () hidden-flag)
(def-forth-const lenmask-flag () hidden-flag)

;; some words
(defcode drop ()
  (pop-ps tmp-1))

(defcode 2drop ()
  (pop-ps tmp-1)
  (pop-ps tmp-1))

(defcode dup ()
  (str tmp-1 (rp -4))
  (push-ps tmp-1))

(defcode 2dup ()
  (str tmp-1 (rp -4))
  (str tmp-2 (rp -8))
  (push-ps tmp-2)
  (push-ps tmp-1))

(defcode over ()
  (str tmp-1 (rp -8))
  (push-ps tmp-1))

;; perhaps todo: swap, rot, -rot, etc are perhaps better (faster) served with
;; multiple register load and/or stores. we however might need
;; some boundschecking later, and so a macro, and so I'm lazy
(defcode swap ()
  (pop-ps tmp-1)
  (pop-ps tmp-2)
  (push-ps tmp-1)
  (push-ps tmp-2))

(defcode 2swap ()
  (pop-ps tmp-1)
  (pop-ps tmp-2)
  (pop-ps tmp-3)
  (pop-ps tmp-4)
  (push-ps tmp-2)
  (push-ps tmp-1)
  (push-ps tmp-4)
  (push-ps tmp-3))

(defcode rot ()
  (pop-ps tmp-1)
  (pop-ps tmp-2)
  (pop-ps tmp-3)
  (push-ps tmp-1)
  (push-ps tmp-3)
  (push-ps tmp-2))

(defcode -rot ()
  (pop-ps tmp-1)
  (pop-ps tmp-2)
  (pop-ps tmp-3)
  (push-ps tmp-2)
  (push-ps tmp-1)
  (push-ps tmp-3))

(defcode ?dup ()
  (str tmp-1 (rp -4))
  (teq tmp-1 0)
  (push-ps tmp-1 :cond ne))

(defcode 1+ ()
  (pop-ps tmp-1)
  (add tmp-1 tmp-1 1)
  (push-ps tmp-1))

(defcode 1- ()
  (pop-ps tmp-1)
  (sub tmp-1 tmp-1 1)
  (push-ps tmp-1))

(defcode 4+ ()
  (pop-ps tmp-1)
  (add tmp-1 tmp-1 4)
  (push-ps tmp-1))

(defcode 4- ()
  (pop-ps tmp-1)
  (sub tmp-1 tmp-1 4)
  (push-ps tmp-1))

(defcode + ()
  (pop-ps tmp-1)
  (pop-ps tmp-2)
  (add tmp-3 tmp-1 tmp-2)
  (push-ps tmp-3))

(defcode - ()
  (pop-ps tmp-1)
  (pop-ps tmp-2)
  (sub tmp-3 tmp-2 tmp-1)
  (push-ps tmp-3))

(defcode * ()
  (pop-ps tmp-1)
  (pop-ps tmp-2)
  (mul tmp-3 tmp-1 tmp-2)
  (push-ps tmp-3))

#-(and) (defcode /mod ()
          "remains unimplemented for now. have to see how to best handle division on arm")

;; comparation fn's
(defcode = ()
  (pop-ps tmp-1)
  (pop-ps tmp-2)
  (teq tmp-1 tmp-2)
  (moveq tmp-3 1 :lsl 1)
  (movne tmp-3 0)
  (push-ps tmp-3))

(defcode <> ()
  (pop-ps tmp-1)
  (pop-ps tmp-2)
  (teq tmp-1 tmp-2)
  (movne tmp-3 1 :lsl 1)
  (moveq tmp-3 0)
  (push-ps tmp-3))

(defcode < ()
  (pop-ps tmp-1)
  (pop-ps tmp-2)
  (cmp tmp-1 tmp-2)
  (movlt tmp-3 1 :lsl 1)
  (movge tmp-3 0)
  (push-ps tmp-3))

(defcode > ()
  (pop-ps tmp-1)
  (pop-ps tmp-2)
  (cmp tmp-1 tmp-2)
  (movle tmp-3 1 :lsl 1)
  (movgt tmp-3 0)
  (push-ps tmp-3))

(defcode <= ()
  (pop-ps tmp-1)
  (pop-ps tmp-2)
  (cmp tmp-1 tmp-2)
  (movle tmp-3 1 :lsl 1)
  (movgt tmp-3 0)
  (push-ps tmp-3))

(defcode >= ()
  (pop-ps tmp-1)
  (pop-ps tmp-2)
  (cmp tmp-1 tmp-2)
  (movge tmp-3 1 :lsl 1)
  (movlt tmp-3 0)
  (push-ps tmp-3))

;; compare to 0
(defcode 0= ()
  (pop-ps tmp-1)
  (teq tmp-1 0)
  (moveq tmp-3 1 :lsl 1)
  (movne tmp-3 0)
  (push-ps tmp-3))

(defcode 0<> ()
  (pop-ps tmp-1)
  (teq tmp-1 0)
  (movne tmp-3 1 :lsl 1)
  (moveq tmp-3 0)
  (push-ps tmp-3))

(defcode 0< ()
  (pop-ps tmp-1)
  (cmp tmp-1 0)
  (movlt tmp-3 1 :lsl 1)
  (movge tmp-3 0)
  (push-ps tmp-3))

(defcode 0> ()
  (pop-ps tmp-1)
  (cmp tmp-1 0)
  (movle tmp-3 1 :lsl 1)
  (movgt tmp-3 0)
  (push-ps tmp-3))

(defcode 0<= ()
  (pop-ps tmp-1)
  (cmp tmp-1 0)
  (movle tmp-3 1 :lsl 1)
  (movgt tmp-3 0)
  (push-ps tmp-3))

(defcode 0>= ()
  (pop-ps tmp-1)
  (cmp tmp-1 0)
  (movge tmp-3 1 :lsl 1)
  (movlt tmp-3 0)
  (push-ps tmp-3))

;; logical
(defcode and ()
  (pop-ps tmp-1)
  (pop-ps tmp-2)
  (and tmp-3 tmp-1 tmp-2)
  (push-ps tmp-3))

(defcode or ()
  (pop-ps tmp-1)
  (pop-ps tmp-2)
  (orr tmp-3 tmp-1 tmp-2)
  (push-ps tmp-3))

(defcode xor ()
  (pop-ps tmp-1)
  (pop-ps tmp-2)
  (eor tmp-3 tmp-1 tmp-2)
  (push-ps tmp-3))

(defcode xor ()
  (pop-ps tmp-1)
  (pop-ps tmp-2)
  (eor tmp-3 tmp-1 tmp-2)
  (push-ps tmp-3))

(defcode invert ()
  (pop-ps tmp-1)
  (mvn tmp-2 tmp-1)
  (push-ps tmp-3))

;; cool functions
(defcode exit ()
  (pop-rs ip))

(defcode lit ()
  (ldr ip (ip) 4)
  (push-ps ip))

;; memory manipulation!
(defcode ! ()
  (pop-ps tmp-1)
  (pop-ps tmp-2)
  (str tmp-1 (tmp-2)))

(defcode @ ()
    (pop-ps tmp-1)
  (ldr tmp-2 (tmp-1))
  (push-ps tmp-2))

(defcode +! ()
  (pop-ps tmp-1)
  (pop-ps tmp-2)
  (ldr tmp-3 (tmp-1))
  (add tmp-4 tmp-3 tmp-2)
  (str tmp-4 (tmp-1)))

(defcode +! ()
  (pop-ps tmp-1)
  (pop-ps tmp-2)
  (ldr tmp-3 (tmp-1))
  (add tmp-4 tmp-3 tmp-2)
  (str tmp-4 (tmp-1)))

(defcode c! ()
  (pop-ps tmp-1)
  (pop-ps tmp-2)
  (strb tmp-1 (tmp-2)))

(defcode c@ ()
  (pop-ps tmp-1)
  (ldrb tmp-2 (tmp-1))
  (push-ps tmp-2))

;; block copying. will look at it later
#-(and) ((defcode c@c!)
         (defcode cmove))

;; return stack manipulation
(defcode >r ()
  (pop-ps tmp-1)
  (push-rs tmp-1))

(defcode r> ()
  (pop-rs tmp-1)
  (push-ps tmp-1))

(defcode rsp@ ()
  (push-ps rp))

(defcode rsp! ()
  (pop-ps rp))

(defcode rdrop ()
  (pop-rs tmp-1))

;; parameter (return) stack manipulation
(defcode dsp@ ()
  (push-ps sp))

(defcode dsp! ()
  (pop-ps sp))

;; in- and output
(defcode key ()
  (bl :%key)
  (push-ps tmp-1))

(def-asm-fn-lite %key
  (ldr tmp-2 :curr-key)
  (ldr tmp-3 :buff-top)
  (cmp tmp-2 tmp-3)      ;; no more input
  (bge :get-input)       ;; get some more input
  (ldrb tmp-1 (tmp-2) 1) ;; otherwise read byte and increment curr-key
  (str tmp-2 :curr-key)  ;; store curr key back in mem location
                         ;; and please hack the assembler to make this store form valid
  (mov lr pc)            ;; and branch back to key
  
  :key-input
  ;; unimplementable, cause don't know how
  ;; waiting for DSerial and we'll see how to interface the tib with some input
  
  :curr-key
  (word tib-base)
  :buff-top
  (word tib-base))

(defcode emit ()
  ;; emits a byte to output, where-ever that is.
  ;; is going to be implemented once I've got more of a clue
  )

