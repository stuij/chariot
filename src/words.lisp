(in-package :chariot)

;; first some macros
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
       (def-asm-fn ,(intern (symbol-name link-label))
         (word link)
         (byte ,(+ flags word-length))
         (string ,word-name)
         align
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
(def-forth-var base () 16)

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

(def-asm-fn %key
  (ldr tmp-2 :curr-key)
  (ldr tmp-3 :buff-top)
  (cmp tmp-2 tmp-3)      ;; no more input
  (bge :get-input)       ;; get some more input
  (ldrb tmp-1 (tmp-2) 1) ;; otherwise read byte and increment curr-key
  (str tmp-2 :curr-key)  ;; store curr key back in mem location
  ;; and please hack the assembler to make this store form valid
  (mov pc lr) ;; and branch back to key
  
  :key-input
  ;; unimplementable, cause don't know how
  ;; waiting for DSerial and we'll see how to interface the tib with some input
  
  :curr-key
  (word *tib-base*)
  :buff-top
  (word *tib-base*))

(defcode emit ()
  ;; emits a byte to output, where-ever that is.
  ;; is going to be implemented once I've got more of a clue
  )

(defcode word ()
  (bl :%word)
  (push-ps tmp-3)  ;; word base address
  (push-ps tmp-2)) ;; word length

(def-asm-fn %word
  (bl :%key)
  (teq tmp-1 #\\)
  (beq :skip-comment)
  (teq tmp-1 #\space)
  (beq :%word)

  (ldr tmp-2 (address :word-buffer))
  (mov tmp-3 tmp-2)
  
  :search-word-end
  (strb tmp-1 (tmp-2) 1) ;; put char in word-buffer and increment
  (bl :%key)
  (teq tmp-1 #\space)
  (bne :search-word-end)

  (sub tmp-2 tmp-2 tmp-3) ;; determine word lenght
  
  :skip-comment
  (bl :%key)
  (teq #\newline)
  (bne :skip-comment)
  (beq :%word)

  :word-buffer
  (byte 32)
  pool)

;; parsing numbers
(defcode number ()
  (pop-ps tmp-3) ;; length of string
  (pop-ps tmp-4) ;; start of string
  (bl :%number)
  (push tmp-2)   ;; parsed nr
  (push tmp-3))  ;; nr of unparsed chars (0 = error)

(def-asm-fn %number
  (mov tmp-1 1)
  (mov tmp-2 1)

  (tst tmp-3 tmp-3) ;; check if string length is 0
  (beq :return-nr)

  (ldr tmp-5 (address :base-var)) ;; load the
  (ldr tmp-5 (tmp-5))             ;; current base
  
  ;; check if first char is #\-
  (push-ps tmp-1) ;; put 1 on stack indicating positive
  
  (ldrb tmp-1 (tmp-4) 1) ;; load char and increment
  (teq tmp-1 #\-)
  (bne :convert-to-nr)

  (pop-ps tmp-1)    ;; take away positive indicator
  (mov tmp-1 0)     ;; turns out nr is negative
  (push-ps tmp-1)   ;; put negative indicator on stack

  (subs tmp-3 tmp-3 1)
  (bpi :loop-min-mult) ;; if more digits loop for them
  (pop-ps tmp-1) ;; otherwise we're stuck with only a '-' which is no digit at all
  (mv lr pc)     ;; and return

  :loop-for-digits
  (mul tmp-2 tmp-2 tmp-5) ;; tmp-2 (*= tmp-2 base), so shift nr, sort of

  :loop-min-mult
  (ldrb tmp-1 (tmp-4) 1)
  
  :convert-to-nr
  (subs tmp-1 tmp-1 #\0)
  (blt :wrap-up-nr) ;; aka error
  (cmp tmp-1 10)    ;; check if lower than '9'
  (blt :base-overflow-p)
  (subs tmp-1 tmp-1 17) ;; check if lower than 'A'
  (blt :wrap-up-nr)     ;; aka error
  (cmp tmp-1 26)        ;; check if nr is in range 'A'-'Z'
  (addlt tmp-1 tmp-1 10) ;; if so add 10
  (blt :add-to-nr)       ;; and branch to overflow
  (subs tmp-1 tmp-1 33)  ;; otherwise add 26 plus 7 to get to 'a'-'z'
  (blt :wrap-up-nr)      ;; lower than that is error
  (add tmp-1 tmp-1 10) ;; otherwise leave it up to base-overflow to see if we went over 'z'
  
  :base-overflow-p
  (cmp tmp-5 tmp-1) ;; compare nr to base
  (bge :wrap-up-nr) ;; to big? error

  :add-to-nr
  (add tmp-2 tmp-2 tmp-1)
  (subs tmp-3 tmp-3 1)
  (bgt :loop-for-digits)
  
  :wrap-up-nr ;; done, negate nr and get out of here
  (pop-rs tmp-5)
  (tst tmp-5 tmp-5)
  (rsbeq tmp-2 tmp-2 0)

  :return-nr
  (mov pc lr)
    
  pool)