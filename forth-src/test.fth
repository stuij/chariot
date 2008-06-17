-6 ( <--numerator ) 1 ( <--denominator ) /MOD

( : PLUS-2 2 + ;
 
: DEFERRED-PLUS-2 ' PLUS-2 EXECUTE ;
66 ( bla bla ) DEFERRED-PLUS-2

(bcs :sdiv-handle-neg-quotient)       ; if (sign[31]) d=-d;
    

:sdiv-handle-neg-quotient
     
    (teq r r)
    (beq :sdiv-neg-quotient-return)
    (load-jr tmp base)
    (sub d d 1)
    (sub r tmp r)
    
    :sdiv-neg-quotient-return
    (ldmfd sp! (pc))
)