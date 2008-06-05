(in-package :chariot)

(in-asm-space chariot)
(in-block chariot-setup)

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
         (word (ia link))
         (byte ,(+ flags word-length))
         (string ,word-name)
         align
         ,label
         ,@body
         (set-asm-param link (address ,link-label))))))

(defmacro defword (name more-params &body words)
  (let ((word-list (loop for word in words
                      append (etypecase word 
                                (symbol `((word (address ,(intern (symbol-name word) :keyword)))))
                                (number `((word (address :lit))
                                          (word ,word)))))))
    `(defword-builder ,name ,more-params
       (word (address :%docol))
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

(defmacro def-forth-var (name more-params &optional (val 0))
  (let* ((label (intern (symbol-name name) :keyword))
         (code-label (intern (symbol-name (concat-symbol label "-CODE")) :keyword))
         (var-label (intern (symbol-name (concat-symbol label "-VAR")) :keyword)))
    `(defword-builder ,name ,more-params
       (word (address ,code-label))
       ,code-label
       (ldr tmp-1 (address ,var-label))
       (push-ps tmp-1)
       next
       ,var-label
       (word ,val)
       pool)))

(defmacro def-forth-const (name more-params val)
  (let* ((label (intern (symbol-name name) :keyword))
         (code-label (intern (symbol-name (concat-symbol label "-CODE")) :keyword))
         (var-label (intern (symbol-name (concat-symbol label "-VAR")) :keyword)))
    `(defword-builder ,name ,more-params
       (word (address ,code-label))
       ,code-label
       (ldr tmp-1 ,var-label)
       (push-ps tmp-1)
       next
       ,var-label
       (word ,val))))

;; built-in variables
(def-forth-var state ())
(def-forth-var latest ())
(def-forth-var here ())
(def-forth-var base () 16)

;; constants
(def-forth-const version () 1)
(def-forth-const docol () (address :%docol))
(def-forth-const imm-flag () imm-flag)
(def-forth-const hidden-flag () hidden-flag)
(def-forth-const lenmask-flag () hidden-flag)


;; cool functions
(defcode exit ()
  (pop-rs ip))

(defcode lit ()
  (ldr tmp-1 (ip) 4)
  (push-ps tmp-1))

(defcode eternal ()
  :eternal-loop
  (b :eternal-loop))

;; the mundane
(defcode drop ()
  (pop-ps tmp-1))

(defcode dup ()
  (ldr tmp-1 (sp))
  (push-ps tmp-1))

(defcode 2dup ()
  (ldr tmp-1 (sp))
  (ldr tmp-2 (sp 4))
  (push-ps tmp-2)
  (push-ps tmp-1))

(defcode over ()
  (str tmp-1 (sp 4))
  (push-ps tmp-1))

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
  (str tmp-1 (sp))
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
  (mvneq tmp-3 0)
  (movne tmp-3 0)
  (push-ps tmp-3))

(defcode <> ()
  (pop-ps tmp-1)
  (pop-ps tmp-2)
  (teq tmp-1 tmp-2)
  (mvnne tmp-3 0)
  (moveq tmp-3 0)
  (push-ps tmp-3))

(defcode < ()
  (pop-ps tmp-2)
  (pop-ps tmp-1)
  (cmp tmp-1 tmp-2)
  (mvnlt tmp-3 0) ;; lower than = true
  (movge tmp-3 0)
  (push-ps tmp-3))

(defcode > ()
  (pop-ps tmp-2)
  (pop-ps tmp-1)
  (cmp tmp-1 tmp-2)
  (mvngt tmp-3 0)
  (movle tmp-3 0)
  (push-ps tmp-3))

(defcode <= ()
  (pop-ps tmp-2)
  (pop-ps tmp-1)
  (cmp tmp-1 tmp-2)
  (mvnle tmp-3 0)
  (movgt tmp-3 0)
  (push-ps tmp-3))

(defcode >= ()
  (pop-ps tmp-2)
  (pop-ps tmp-1)
  (cmp tmp-1 tmp-2)
  (mvnge tmp-3 0)
  (movlt tmp-3 0)
  (push-ps tmp-3))

;; compare to 0
(defcode 0= ()
  (pop-ps tmp-1)
  (teq tmp-1 0)
  (mvneq tmp-3 0)
  (movne tmp-3 0)
  (push-ps tmp-3))

(defcode 0<> ()
  (pop-ps tmp-1)
  (teq tmp-1 0)
  (mvnne tmp-3 0)
  (moveq tmp-3 0)
  (push-ps tmp-3))

(defcode 0< ()
  (pop-ps tmp-1)
  (cmp tmp-1 0)
  (mvnlt tmp-3 0)
  (movge tmp-3 0)
  (push-ps tmp-3))

(defcode 0> ()
  (pop-ps tmp-1)
  (cmp tmp-1 0)
  (mvngt tmp-3 0)
  (movle tmp-3 0)
  (push-ps tmp-3))

(defcode 0<= ()
  (pop-ps tmp-1)
  (cmp tmp-1 0)
  (mvnle tmp-3 0)
  (movgt tmp-3 0)
  (push-ps tmp-3))

(defcode 0>= ()
  (pop-ps tmp-1)
  (cmp tmp-1 0)
  (mvnge tmp-3 0)
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

(defcode invert ()
  (pop-ps tmp-1)
  (mvn tmp-2 tmp-1)
  (push-ps tmp-3))



;; memory manipulation!
(defcode ! ()
  (pop-ps tmp-1)
  (pop-ps tmp-2)
  (str tmp-2 (tmp-1)))

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

(defcode -! ()
  (pop-ps tmp-1)
  (pop-ps tmp-2)
  (ldr tmp-3 (tmp-1))
  (sub tmp-4 tmp-3 tmp-2)
  (str tmp-4 (tmp-1)))

(defcode c! ()
  (pop-ps tmp-1)
  (pop-ps tmp-2)
  (strb tmp-2 (tmp-1)))

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
  (mov tmp-1 sp)
  (push-ps tmp-1))

(defcode dsp! ()
  (pop-ps sp))



;; in- and output
(defcode key ()
  (b-and-l :%key)
  (push-ps tmp-1))

(def-asm-fn %key
  (ldr tmp-2 :curr-key)
  (ldr tmp-3 :buff-top)
  (cmp tmp-2 tmp-3)      ;; no more input
  (bge :get-input)       ;; get some more input
  (ldrb tmp-1 (tmp-2) 1) ;; otherwise read byte and increment curr-key
  (str tmp-2 :curr-key)  ;; store curr key back in mem location
  (mov pc lr)            ;; and branch back to caller
  
  :get-input
  (b :get-input)
  ;; unimplementable, cause don't know how
  ;; waiting for DSerial and we'll see how to interface the tib with some input

  ;; we peg a line of text as a temporary tib, until we've got the whole machinery
  ;; going
  
  :curr-key
  (word (address :tib-base))
  :buff-top
  (word (address :tib-top)))

(defcode emit ()
  ;; emits a byte to output, where-ever that is.
  ;; is going to be implemented once I've got more of a clue
  )

(defcode word ()
  (b-and-l :%word)
  (push-ps tmp-3)  ;; word base address
  (push-ps tmp-2)) ;; word length

(def-asm-fn %word
  (push-ps lr)
  (b-and-l :%key)
  (pop-ps lr)
  (teq tmp-1 #\\)
  (beq :skip-comment)
  (teq tmp-1 #\space)
  (beq :%word)

  (ldr tmp-2 (address :word-buffer))
  (mov tmp-3 tmp-2)
  
  :search-word-end
  (strb tmp-1 (tmp-2) 1) ;; put char in word-buffer and increment
  (stmfd sp! (tmp-2 tmp-3 lr))
  (b-and-l :%key)
  (ldmfd sp! (tmp-2 tmp-3 lr))
  (teq tmp-1 #\space)
  (bne :search-word-end)

  (sub tmp-2 tmp-2 tmp-3) ;; determine word lenght
  (mov pc lr)             ;; and return
  
  :skip-comment
  (push-ps lr)
  (b-and-l :%key)
  (pop-ps lr)
  (teq tmp-1 #\newline)
  (bne :skip-comment)
  (beq :%word)

  :word-buffer
  (space 32)
  pool)



;; parsing numbers
(defcode number ()
  (pop-ps tmp-3) ;; length of string
  (pop-ps tmp-4) ;; start of string
  (b-and-l :%number)
  (push-ps tmp-2)   ;; parsed nr
  (push-ps tmp-3))  ;; nr of unparsed chars (0 = error)

(def-asm-fn %number
  (mov tmp-1 1)
  (mov tmp-2 0)

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
  (push-ps tmp-2)   ;; put negative indicator on stack

  (subs tmp-3 tmp-3 1)
  (bpl :loop-for-digits) ;; if more digits loop for them
  (pop-ps tmp-1) ;; otherwise we're stuck with only a '-' which is no digit at all
  (mov lr pc)    ;; and return

  :loop-for-digits
  (ldrb tmp-1 (tmp-4) 1)
  
  :convert-to-nr
  (subs tmp-1 tmp-1 #\0)
  (blt :wrap-up-nr) ;; aka error
  (cmp tmp-1 10)    ;; check if lower than '9'
  (blt :base-overflow-p) ;; if so, it's a nr between 0 and 9 check for base overflow
  (subs tmp-1 tmp-1 17)  ;; check if lower than 'A'
  (blt :wrap-up-nr)      ;; aka error
  (cmp tmp-1 26)         ;; check if nr is in range 'A'-'Z'
  (addlt tmp-1 tmp-1 10) ;; if so add 10
  (blt :add-to-nr)       ;; and branch to overflow
  (subs tmp-1 tmp-1 32)  ;; otherwise add 26 plus 6 to get to 'a'-'z'
  (blt :wrap-up-nr)      ;; lower than that is error
  (add tmp-1 tmp-1 10) ;; otherwise leave it up to base-overflow to see if we went over 'z'
  
  :base-overflow-p
  (cmp tmp-1 tmp-5) ;; compare nr to base
  (bge :wrap-up-nr) ;; to big? error

  :add-to-nr
  (mul tmp-2 tmp-2 tmp-5) ;; tmp-2 (*= tmp-2 base), so shift nr, sort of
  (add tmp-2 tmp-2 tmp-1)
  (subs tmp-3 tmp-3 1)
  (bgt :loop-for-digits)
  
  :wrap-up-nr ;; done, negate nr and get out of here
  (pop-ps tmp-5)
  (tst tmp-5 tmp-5)
  (rsbeq tmp-2 tmp-2 0)

  :return-nr
  (mov pc lr)
    
  pool)


;; finding words and word offsets

(defcode find ()
  (pop-ps tmp-1) ;; length
  (pop-ps tmp-2) ;; address
  (b-and-l :%find)
  (push-ps tmp-3)) ;; address of dictionary entry or 0

(def-asm-fn %find
  (ldr tmp-3 (address :latest-var))
  (ldr tmp-3 (tmp-3))

  :find-try-again
  (tst tmp-3 tmp-3) ;; lastest is 0?
  (beq :word-not-found)

  ;; check if words are te same
  ;; first their length
  (ldrb tmp-4 (tmp-3 4))
  (mov tmp-5 (ea (and (logior hidden-flag lenmask-flag))))
  (and tmp-4 tmp-4 tmp-5)
  (teq tmp-4 tmp-1)
  (bne :next-word) ;; not same length or hidden, go to next word

  ;; if their lenght is the same, check the individual characters
  ;; tmp-1 = countdown
  ;; tmp-2 = address control string
  ;; tmp-3 = address link string
  ;; tmp-4 = char control string
  ;; tmp-5 = char link string
  (stmfd sp! (tmp-1 tmp-2 tmp-3)) 
  (add tmp-3 tmp-3 5) ;; setup address control string correctly
  (sub tmp-1 tmp-1 1)
  
  :word-match-loop
  (ldrb tmp-4 (tmp-2) 1)
  (ldrb tmp-5 (tmp-3) 1)
  (teq tmp-4 tmp-5)
  (bne :restore-regs-and-next)
  (subs tmp-1 tmp-1 1)
  (bpl :word-match-loop)

  (ldmfd sp! (tmp-1 tmp-2 tmp-3))
  (mov pc lr)
  
  :restore-regs-and-next
  (ldmfd sp! (tmp-1 tmp-2 tmp-3))

  :next-word
  (ldr tmp-3 (tmp-3)) ;; go to next word in link
  (b :find-try-again)
  
  :word-not-found
  (mov pc lr)

  pool)

(defcode >cfa ()
  (pop-ps tmp-1)
  (b-and-l :%>cfa)
  (push-ps tmp-1))

(def-asm-fn %>cfa
  (add tmp-1 tmp-1 4) ;; skip past link word
  (ldrb tmp-2 (tmp-1) 1) ;; load and skip past length/flags byte
  (mov tmp-3 lenmask-flag) ;; just the
  (and tmp-2 tmp-2 tmp-3) ;; length
  (add tmp-1 tmp-1 tmp-2) ;; and skip past
  ;; word align
  (mvn tmp-3 3)
  (add tmp-1 tmp-1 3)
  (and tmp-1 tmp-1 tmp-3)
  ;; and return
  (mov pc lr))

(defword >dfa ()
  >cfa 4+)