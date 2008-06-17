(in-package :chariot)

;; (def-space-n-blocks fuck-around
;;   (fa-block :base-address 10000)) ;; this base address not used atm

;; (in-asm-space fuck-around)
;; (in-block fa-block)

;; *current-asm-block*

;; (assemble 'arm9 'arm `((add r1 r1 4 :lsl 1)))

;; (clear-current-block)

#- (and) (liards::nds-compile
          (assemble 'arm9 'arm (emit-arm-fns))
          liards::*arm7-bin*
          "fuck-around.nds")

(defword test-word ()
  lit 0 break 0branch -4 break
  ;; word find break >cfa
  ;; lit won't be automatically prepended
  )

(let* ((string "\"And so you see, in each moment you must be catching up the distance between us, and yet I, at the same time, will be adding a new distance, however small, for you to catch up again.\"

\"Indeed, it must be so,\" said Achilles wearily.

\"And so you can never catch up,\" the Tortoise concluded sympathetically.

\"You are right, as always,\" said Achilles sadly, and conceded the race.
but then he thought..")
       (string-length (length string)))

  (defcode test-write ()
    (ldr tmp-2 (address :test-string))
    (push-ps tmp-2)
    (ldr tmp-1 string-length)
    (push-ps tmp-1)
    (b-and-l :write-string))

  (def-asm-fn some-test-strings
    :test-string
    (ea string)
    align
    pool))

(defword test-string ()
  break litstring 3 "bla " tell)

*jr-reachables*

(make-eval-forth "test.fth")