(define (varsList expr)
    (cond
        ((null? expr) '())
        ((and (char>=? (car expr) #\a) (char<=? (car expr) #\z)) 
            (cons (car expr) (varsList (cdr expr)))
        )
        (#T (varsList (cdr expr)))
    )
)

(define (getVarValPairs vars vals)
    (cond
        ((and (null? vars) (null? vals)) '())
        ((or (null? vars) (null? vals))
            (display "error: check variables and its values one more time!\n")
            (exit 0)
        )
        (#T 
            (cons 
                (cons (car vars) (string->number (car vals))) 
                (getVarValPairs (cdr vars) (cdr vals)))
            )
    )
)

(define (takeNum expr exprHeadNum)
    (cond
        ((null? expr) (cons (string->number exprHeadNum) expr))
        ((and (char>=? (car expr) #\0) (char<=? (car expr) #\9))
            (takeNum 
                (cdr expr) 
                (string-append exprHeadNum (string (car expr)))
            )
        )
        (#T (cons (string->number exprHeadNum) expr))
    )
)

(define (takeVarVal var varValPairs)
    (cond
        ((null? varValPairs) 
            (display "error: can't find this variable value!\n")
            (exit 0)
        )
        ((char=? (car (car varValPairs)) var)
            ; (write (cdr (car varValPairs)))
            (cdr (car varValPairs))
        )
        (#T (takeVarVal var (cdr varValPairs)))
    )
)

(define (makeExpr symExpr varValPairs)
    (cond
        ((null? symExpr) '())
        ((char? (car symExpr))
            (cond
                (
                    (and 
                        (char>=? (car symExpr) #\a) 
                        (char<=? (car symExpr) #\z)
                    )
                        (makeExpr 
                            (cons 
                                (takeVarVal 
                                    (car symExpr) 
                                    varValPairs
                                ) 
                                (cdr symExpr)
                            ) 
                            varValPairs
                        )
                )
                (
                    (and 
                        (char>=? (car symExpr) #\0) 
                        (char<=? (car symExpr) #\9)
                    )
                    (makeExpr (takeNum symExpr "") varValPairs)
                )
                ((char=? (car symExpr) #\space)
                    (makeExpr (cdr symExpr) varValPairs)
                )
                (#T 
                    (cons 
                        (car symExpr) 
                        (makeExpr (cdr symExpr) varValPairs)
                    )
                )
            )
        )
        (#T 
            (cons 
                (car symExpr) 
                (makeExpr (cdr symExpr) varValPairs)
            )
        )
    )
)

(define (allNums expr)
    (cond
        ((null? expr) #T)
        ((number? (car expr)) (allNums (cdr expr)))
        (#T #F)
    )
)

(define (calcSimpleExpr expr sign arg1 arg2 numStack)
    (cond
        ((char=? sign #\+)
            (calcExpr
                (cdr expr) 
                (cons (+ arg1 arg2) numStack)
            )
        )
        ((char=? sign #\-)
            (calcExpr
                (cdr expr) 
                (cons (- arg1 arg2) numStack)
            )
        )
        ((char=? sign #\*)
            (calcExpr
                (cdr expr) 
                (cons (* arg1 arg2) numStack)
            )
        )
        ((char=? (car expr) #\/)
            (cond
                ((=? arg2 0)
                    (display "error: it is impossible to divide by zero!\n")
                    (exit 0)
                )
                (#T
                    (calcExpr
                        (cdr expr) 
                        (cons (quotient arg1 arg2) numStack)
                    )
                )
            )
        )
        ((char=? (car expr) #\%)
            (cond
                ((=? arg2 0)
                    (display "error: it is impossible to divide by zero!\n")
                    (exit 0)
                )
                (#T
                    (calcExpr
                        (cdr expr) 
                        (cons (remainder arg1 arg2) numStack)
                    )
                )
            )
        )
        (#T 
            (display "error: check expression one more time!\n")
            (exit 0)
        )
    )
)

(define (calcExpr expr numStack)
    (cond 
        ((null? expr)
            (cond
                ((null? numStack)
                    (display "error: check expression one more time!\n")
                    (exit 0)
                )
                ((null? (cdr numStack)) numStack)
                (#T 
                    (display "error: check expression one more time!\n")
                    (exit 0)
                )
            )
        )
        ((number? (car expr))
            (calcExpr
                (cdr expr)
                (cons (car expr) numStack)
            )
        )
        ((char? (car expr))
            (cond
                ((or 
                    (null? numStack)
                    (null? (cdr numStack)))
                        (display "error: check expression one more time!\n")
                        (exit 0)
                )
                (#T
                    (calcSimpleExpr 
                        expr 
                        (car expr) 
                        (car (cdr numStack))
                        (car numStack)
                        (cdr (cdr numStack))
                    )
                )
            )
        )
        (#T 
            (display "error: check expression one more time!")
            (exit 0)
        )
    )
)

(define (getValsList vals)
    (cond
        ((null? vals) '())
        ((number? (car vals)) 
            (cons (number->string (car vals)) (getValsList (cdr vals)))
        )
        ((char=? (car vals) #\space) 
            (getValsList (cdr vals))
        )
        ((or (char=? (car vals) #\-) (char=? (car vals) #\+))
            (cond
                ((null? (cdr vals))
                    (display "error: check string of variables values!\n")
                    (exit 0)
                )
                ((and 
                    (char>=? (car (cdr vals)) #\0) 
                    (char<=? (car (cdr vals)) #\9))
                        (getValsList (takeNum (cdr vals) (string (car vals))))
                )
                (#T 
                    (display "error: check string of variables values!\n")
                    (exit 0)
                )
            )
        )
        ((and (char>=? (car vals) #\0) (char<=? (car vals) #\9))
            (getValsList (takeNum vals ""))
        )
        (#T
            (display "error: check string of variables values!\n")
            (exit 0)
        )
    )
)

(define (getExprVal symExpr varsVals)
    (car 
        (calcExpr 
            (makeExpr 
                (string->list symExpr) 
                (getVarValPairs 
                    (varsList (string->list symExpr))
                    varsVals
                )
            )
            '()
        )
    )
)

(define (checkForVals expr)
    (cond
        ((varsList (string->list expr)) 
            (printExprValWithVals expr)
        )
        (#T (printExprVal expr))
    )
)

(define (printExprVal expr)
    (display (getExprVal expr '()))
    (display "\n")
)

(define (printExprValWithVals expr)
    (display 
        (getExprVal
            expr
            (let ((vals (read-line)))
                (cond
                    ((eof-object? vals)
                        (exit) 
                    )
                    (#T (getValsList (string->list vals)))
                )
            )
        )
    )
    (display "\n")
    (printExprValWithVals expr)
)

(use extras)
(checkForVals (car (command-line-arguments)))