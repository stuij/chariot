(in-package :chariot)

(in-asm-space chariot)
(in-block chariot-core)

;; first some macros and a fn for some reason or other)
(def-asm-macro-lite next
  (ldr tmp-5 (ip) 4)   ;; load cfa of next word in w and point ip to next word
  (ldr w (tmp-5))      ;; load pfa/codeword or assembly of that next word
  (mov pc w))          ;; branch to it

(def-asm-fn %docol
  (push-rs ip)
  (add w tmp-5 4) ;; increment to point to first word in definition (to which tmp-5 still references from the previous next)
  (mov ip w)      ;; put word in ip so we can call next on it
  next)

(defmacro defword-builder (name (&key (flags 0) forth-name) &rest body)
  (let* ((word-name (if forth-name
                        forth-name
                        (symbol-name name)))
         (word-length (length word-name))
         (label (intern (symbol-name name) :keyword))
         (link-label (intern (symbol-name (concat-symbol label "-LINK")) :keyword)))
    (assert (<= word-length 32) (word-length)
            "word lenght of ~a is bigger than 32" word-length)
    `(def-asm-fn ,(intern (symbol-name link-label))
       (word (ia link))
       (byte (ea (+ ,flags ,word-length)))
       (string ,word-name)
       align
       ,label
       ,@body
       (set-asm-param link (address ,link-label)))))

(defmacro defword (name (&key (flags 0) forth-name) &body words)
  (let ((word-list (loop for word in words
                      collect (etypecase word 
                                (symbol `(word (address ,(intern (symbol-name word) :keyword))))
                                (number `(word ,word))
                                (character `(word ,(aref (string-to-octets (format nil "~a" word) armish::*string-encoding*) 0)))))))
    `(defword-builder ,name (:flags ,flags :forth-name ,forth-name)
       (word (address :%docol))
       ,@word-list
       (word (address :exit)))))

(defmacro defcode (name (&key (flags 0) forth-name) &body code)
  (let* ((label (intern (symbol-name name) :keyword))
         (code-label (intern (symbol-name (concat-symbol label "-CODE")) :keyword)))
    `(defword-builder ,name (:flags ,flags :forth-name ,forth-name)
       (word (address ,code-label))
       ,code-label
       ,@code
       next
       pool)))

(defmacro def-forth-var (name (&key (flags 0) forth-name) &optional (val 0))
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (add-jr-reachable (list ',name ,val)))
     (defcode ,name (:flags ,flags :forth-name ,forth-name)
       (load-jr-address tmp-1 ,name)
       (push-ps tmp-1))))

(defmacro def-forth-const (name (&key (flags 0) forth-name) val)
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (add-jr-reachable (list ',name ,val)))
     (defcode ,name (:flags ,flags :forth-name ,forth-name)
       (load-jr tmp-1 ,name)
       (push-ps tmp-1))))


;; user variables
(def-forth-var state ())
(def-forth-var latest ())
(def-forth-var here ())
(def-forth-var base () 16)
(def-forth-var tobp () *tob-base*)


;; constants
(def-forth-const version () 1)
(def-forth-const rs-base () *rs-base*)
(def-forth-const ps-base () *ps-base*)
(def-forth-const tob-base () *tob-base*)
(def-forth-const tob-max ()  *tob-max*)
(def-forth-const docol () '(address :%docol))
(def-forth-const imm-flag () *imm-flag*)
(def-forth-const hidden-flag () *hidden-flag*)
(def-forth-const lenmask-flag () *lenmask-flag*)


;; cool functions
(defcode exit ()
  (pop-rs ip))

(defcode lit ()
  (ldr tmp-1 (ip) 4)
  (push-ps tmp-1))

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



;; input
;; TODO: implement get-input
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

(defcode word ()
  (b-and-l :%word)
  (push-ps tmp-2)  ;; word base address
  (push-ps tmp-1)) ;; word length

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
  (strb tmp-1 (tmp-3) 1) ;; put char in word-buffer and increment
  (stmfd sp! (tmp-3 tmp-2 lr))
  (b-and-l :%key)
  (ldmfd sp! (tmp-3 tmp-2 lr))
  (teq tmp-1 #\space)
  (bne :search-word-end)

  (sub tmp-1 tmp-3 tmp-2) ;; determine word lenght
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

;; output
;; not yet implemented

(defcode emit ()
  (pop-ps tmp-1)
  (load-jr tmp-3 tobp)

  (teq tmp-1 #xA)
  (beq :emit-write-setup)
  (teq tmp-1 #xD)
  (beq :emit-write-setup)
  
  (strb tmp-1 (tmp-3) 1)
  (store-jr tmp-3 tobp)

  (load-jr tmp-4 tob-max)
  (cmp tmp-3 tmp-4)
  (bpl :emit-write-setup)

  (b :to-next)

  :emit-write-setup
  (load-jr tmp-4 tob-base)
  (push-ps tmp-4)
  (sub tmp-5 tmp-3 tmp-4)
  (push-ps tmp-5)

  (store-jr tmp-4 tobp) ;; set pointer to base of tob again
  
  (b-and-l :write-string)

  :to-next)

(defword test-emit ()
  lit #\a emit lit #\b emit)

;; literal strings
(defcode litstring ()
  (ldr tmp-1 (ip) 4)
  (push-ps ip)
  (push-ps tmp-1)
  (add ip ip tmp-1)
  (add ip ip 3)
  (bic ip ip 3))

(defcode tell ()
  ;; prints a string, but how?
  ;; I'll look into it when concidering general output
  )


;; parsing numbers
(defcode number ()
  (pop-ps tmp-1) ;; length of string
  (pop-ps tmp-2) ;; start of string
  (b-and-l :%number)
  (push-ps tmp-4)   ;; parsed nr
  (push-ps tmp-1))  ;; nr of unparsed chars (0 = NO error)

(def-asm-fn %number
  (mov tmp-3 1)
  (mov tmp-4 0)

  (tst tmp-1 tmp-1) ;; check if string length is 0
  (beq :return-nr)

  (load-jr tmp-5 base) ;; load the current base
  
  ;; check if first char is #\-
  (push-ps tmp-3) ;; put 1 on stack indicating positive
  
  (ldrb tmp-3 (tmp-2) 1) ;; load char and increment
  (teq tmp-3 #\-)
  (bne :convert-to-nr)

  (pop-ps tmp-3)    ;; take away positive indicator
  (push-ps tmp-4)   ;; put negative indicator on stack

  (subs tmp-1 tmp-1 1)
  (bpl :loop-for-digits) ;; if more digits loop for them
  (pop-ps tmp-3) ;; otherwise we're stuck with only a '-' which is no digit at all
  (mov lr pc)    ;; and return

  :loop-for-digits
  (ldrb tmp-3 (tmp-2) 1)
  
  :convert-to-nr
  (subs tmp-3 tmp-3 #\0)
  (blt :wrap-up-nr) ;; aka error
  (cmp tmp-3 10)    ;; check if lower than '9'
  (blt :base-overflow-p) ;; if so, it's a nr between 0 and 9 check for base overflow
  (subs tmp-3 tmp-3 17)  ;; check if lower than 'A'
  (blt :wrap-up-nr)      ;; aka error
  (cmp tmp-3 26)         ;; check if nr is in range 'A'-'Z'
  (addlt tmp-3 tmp-3 10) ;; if so add 10
  (blt :add-to-nr)       ;; and branch to overflow
  (subs tmp-3 tmp-3 32)  ;; otherwise add 26 plus 6 to get to 'a'-'z'
  (blt :wrap-up-nr)      ;; lower than that is error
  (add tmp-3 tmp-3 10) ;; otherwise leave it up to base-overflow to see if we went over 'z'
  
  :base-overflow-p
  (cmp tmp-3 tmp-5) ;; compare nr to base
  (bge :wrap-up-nr) ;; to big? error

  :add-to-nr
  (mul tmp-4 tmp-4 tmp-5) ;; tmp-4 (*= tmp-4 base), so shift nr, sort of
  (add tmp-4 tmp-4 tmp-3)
  (subs tmp-1 tmp-1 1)
  (bgt :loop-for-digits)
  
  :wrap-up-nr ;; done, negate nr and get out of here
  (pop-ps tmp-5)
  (tst tmp-5 tmp-5)
  (rsbeq tmp-4 tmp-4 0)

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
  (load-jr tmp-3 latest)

  :find-try-again
  (tst tmp-3 tmp-3) ;; lastest is 0?
  (beq :word-not-found)

  ;; check if words are te same
  ;; first their length
  (ldrb tmp-4 (tmp-3 4))
  (and tmp-4 tmp-4 (ea (and (logior *hidden-flag* *lenmask-flag*))))
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
  (pop-ps tmp-3)
  (b-and-l :%>cfa)
  (push-ps tmp-1))

(def-asm-fn %>cfa
  ;; please don't modify tmp-3. interpret counts on it not changing
  ;; this might be ugly but saves us a few instructions
  ;; ungh! premature optimization... assembly fucks with your head
  (add tmp-1 tmp-3 4)    ;; skip past link word
  (ldrb tmp-2 (tmp-1) 1) ;; load and skip past length/flags byte
  (and tmp-2 tmp-2 *lenmask-flag*) ;; length
  (add tmp-1 tmp-1 tmp-2)          ;; and skip past
  ;; word align
  (add tmp-1 tmp-1 3)
  (bic tmp-1 tmp-1 3)
  ;; and return
  (mov pc lr))

(defword >dfa ()
  >cfa 4+)


;; compiling!
(defcode create ()
  (pop-ps tmp-1) ;; length of name
  (pop-ps tmp-2) ;; address of name

  (load-jr tmp-3 here) ;; load 'here'
  (push-ps tmp-3)          ;; save a copy for later
  
  (load-jr tmp-4 latest) ;; load 'latest'

  (str tmp-4 (tmp-3) 4)  ;; store link
  (strb tmp-1 (tmp-3) 1) ;; store lenght byte (flags are all 0)

  ;; store name
  (sub tmp-1 tmp-1 1)

  :store-name-loop
  (ldrb tmp-5 (tmp-2) 1)
  (strb tmp-5 (tmp-3) 1)
  (subs tmp-1 tmp-1 1)
  (bpl :store-name-loop)

  ;; align
  (add tmp-3 tmp-3 3) ;; to next
  (bic tmp-3 tmp-3 3) ;; 4-byte boundry

  ;; save new 'here' and 'latest'
  ;; not super opcode-efficient, but clearer than camming all the logic in
  ;; the previous load here/latest dance

  ;; get the 'here' and 'latest' store addresses back
  (pop-ps tmp-2)
  
  ;; now the relevant register situation looks like this
  ;; tmp-3 = now here
  ;; tmp-5 = starting here pointer address, now link address
  
  ;; so lets store the new values
  (store-jr tmp-3 here)
  (store-jr tmp-2 latest))

(defcode comma (:forth-name ",")
  (pop-ps tmp-1)
  (b-and-l :%comma))

(def-asm-fn %comma
  ;; if ya can, don't disturb tmp-4. Interpret expects this
  (load-jr tmp-3 here) ;; actual address of here
  (str tmp-1 (tmp-3) 4)
  (store-jr tmp-3 here) ;; store new incrememted next free byte back in here
  (mov pc lr)
  pool)

;; could also give names [ and ] on lisp side
;; but parenscript hates that
(defcode lbrac (:forth-name "[" :flags *imm-flag*)
  (mov tmp-2 0)
  (store-jr tmp-2 state)) ;; get (from immedate mode) into compiling mode

(defcode rbrac (:forth-name "]")
  (mov tmp-2 1)
  (store-jr tmp-2 state)) ;; go into immediate mode from compiling mode

(defcode immediate (:flags *imm-flag*)
  (load-jr tmp-1 latest)
  (add  tmp-1 tmp-1 4)
  (ldrb tmp-2 (tmp-1))
  (eor  tmp-2 tmp-2 *imm-flag*)
  (strb tmp-2 (tmp-1)))

(defcode hidden ()
  (pop-ps tmp-1)
  (add  tmp-1 tmp-1 4)
  (ldrb tmp-2 (tmp-1))
  (eor  tmp-2 tmp-2 *hidden-flag*)
  (strb tmp-2 (tmp-1)))

(defword hide ()
  word find hidden)

(defcode tick (:forth-name "'")
  ;; works only in compiled code
  ;; and what's the difference between this and lit anyway???
  ;; should try in word form, as immediate, with: word find >cfa (push on stack)
  (ldr tmp-1 (ip) 4)
  (push-ps tmp-1))

;; the king and queen of forth word compiling
(defword colon (:forth-name ":")
  word create
  docol comma
  latest @ hidden
  rbrac)

(defword semicolon (:forth-name ";" :flags *imm-flag*)
  lit exit comma
  latest @ hidden
  lbrac)

;; branching
(defcode branch ()
  (ldr tmp-1 (ip))
  (add ip ip tmp-1))

(defcode 0branch ()
  (pop-ps tmp-1)
  (tst tmp-1 tmp-1)
  (beq :branch-code)
  (add ip ip 4))


;; interpreter!!
(defword quit ()
  rs-base rsp! interpret branch -8)

(defcode interpret ()
  (b-and-l :%word)
  ;; tmp-1 = word length
  ;; tmp-2 = word base address
  
  (mov tmp-3 0)
  (str tmp-3 :litp)
  (b-and-l :%find)
  
  (tst tmp-3 tmp-3) ;; testing tmp-3 specifically for literal is purely accidental
  (beq :do-literal)
  ;; word is in dictionary
  (b-and-l :%>cfa)
  
  ;; cfa of word should now be in tmp-1
  (ldrb tmp-3 (tmp-3 4))
  (ands tmp-3 tmp-3 *imm-flag*)
  (bne :execute-it) ;; flag set? then imm! start executing! On the double!
  (beq :compiling-or-executing) ;; eq/ne tests seem inverted to the non-trained
  ;; assembly programmer, but seems to be on level

  :do-literal
  (mov tmp-3 1)
  (str tmp-3 :litp)
  ;; values in tmp-1 and tmp-2 were preserved
  (b-and-l :%number)
  ;; tmp-1 = amount of not parsed chars
  ;; tmp-2 is still address of nr
  ;; tmp-4 is parsed nr
  (tst tmp-1 tmp-1) ;; see if tmp
  (bne :interpret-error)
  ;; otherwise put code address of lit in address expected
  (ldr tmp-1 (address :lit))
  
  :compiling-or-executing
  ;; tmp-2, tmp-3 are now free
  ;; tmp-1 holds either the lit address or the word address
  ;; tmp-4 holds the number value if word was a nr
  (load-jr tmp-3 state)
  (tst tmp-3 tmp-3) ;; if state is 0, we're in interpret mode,
  (beq :execute-it) ;; so execute
  ;; otherwise we're compiling, so compile the word in tmp-1
  (b-and-l :%comma)
  ;; and check if we have to compile a nr as well
  (ldr tmp-3 :litp)
  (tst tmp-3 tmp-3)
  (movne tmp-1 tmp-4)        ;; if so compile
  (b-and-l :%comma :cond ne) ;; the nr as well
  next
  
  :execute-it
  (ldr tmp-3 :litp)  ;; if we're dealing
  (tst tmp-3 tmp-3)  ;; with a literal,
  (bne :push-literal-on-stack) ;; push it on the stack

  ;; otherwise jump to it
  (mov tmp-5 tmp-1) ;; line up tmp-5 to point to cfa, in simulation of next call. needed if next word is a high level word
  (ldr tmp-1 (tmp-1))
  (mov pc tmp-1) ;; which will call next, which will bring us back to 'quit'

  :push-literal-on-stack
  (push-ps tmp-4)
  next
  
  "interpret-error"
  align
  :interpret-error
  ;; once we have output, we'll implement something useful
  (b :write-error-code)
  
  :litp
  (word 0))


;; misc

;; debugging
(defcode break ()
  ;; halts debugger
  (bkpt 0))

(defcode eternal ()
  :eternal-loop
  (b :eternal-loop))

(let* ((string "\"And so you see, in each moment you must be catching up the distance between us, and yet I, at the same time, will be adding a new distance, however small, for you to catch up again.\"

\"Indeed, it must be so,\" said Achilles wearily.

\"And so you can never catch up,\" the Tortoise concluded sympathetically.

\"You are right, as always,\" said Achilles sadly, and conceded the race.
but then he thought..")
       (string-length (length string))
       (string2 "Interpret error
prolly a typo somewhere")
       (string-length2 (length string2)))


  (defcode test-write ()
    (ldr tmp-2 (address :test-string))
    (push-ps tmp-2)
    (ldr tmp-1 string-length)
    (push-ps tmp-1)
    (b-and-l :write-string))
  
  (defcode write-error ()
    (ldr tmp-2 (address :test-string2))
    (push-ps tmp-2)
    (ldr tmp-1 string-length2)
    (push-ps tmp-1)
    (b-and-l :write-string))
  
  (def-asm-fn some-test-strings
    :test-string
    (ea string)
    align
    
    :test-string2
    (ea string2)
    align
    pool))