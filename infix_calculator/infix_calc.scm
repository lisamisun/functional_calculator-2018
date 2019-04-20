(define (varsList expr)
    (cond
        ((null? expr) '())
        ((and (char>=? (car expr) #\a) (char<=? (car expr) #\z))
            (cons (car expr) (varsList (cdr expr)))
        )
        (#T (varsList (cdr expr)))
    )
)

(define (delDupls sortedList)
    (cond
        ((null? sortedList) '())
        ((null? (cdr sortedList)) sortedList)
        ((char=? (car sortedList) (car (cdr sortedList)))
            (delDupls (cdr sortedList))
        )
        (#T (cons (car sortedList) (delDupls (cdr sortedList))))
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
                (getVarValPairs (cdr vars) (cdr vals))
            )
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
            (display "error: can't find this variable value!")
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
                ((and (char>=? (car symExpr) #\a) (char<=? (car symExpr) #\z))
                    (makeExpr 
                        (cons 
                            (takeVarVal (car symExpr) varValPairs) 
                            (cdr symExpr)
                        ) 
                        varValPairs
                    )
                )
                ((and (char>=? (car symExpr) #\0) (char<=? (car symExpr) #\9))
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
        (#T (cons (car symExpr) (makeExpr (cdr symExpr) varValPairs)))
    )
)

(define (allNums expr)
    (cond
        ((null? expr) #T)
        ((number? (car expr)) (allNums (cdr expr)))
        (#T #F)
    )
)

(define (calcSimpleExpr expr sign arg1 arg2)
    (cond
        ((char=? sign #\+) 
            (calcExpr
                (cons
                    (+ arg1 arg2)
                    (calcExpr expr)
                )
            )
        )
        ((char=? sign #\-) 
            (calcExpr
                (cons
                    (- arg1 arg2)
                    (calcExpr expr)
                )
            )
        )
        ((char=? sign #\*) 
            (calcExpr
                (cons
                    (* arg1 arg2)
                    (calcExpr expr)
                )
            )
        )
        ((char=? sign #\/) 
            (calcExpr
                (cond
                    ((= arg2 0)
                        (display "error: it is impossible to divide by zero!")
                        (exit 0)
                    )
                    (T (cons (quotient arg1 arg2) (calcExpr expr)))
                )
            )
        )
        ((char=? sign #\%) 
            (calcExpr
                (cond
                    ((= arg2 0)
                        (display "error: it is impossible to divide by zero!")
                        (exit 0)
                    )
                    (#T (cons (remainder arg1 arg2) (calcExpr expr)))
                )
            )
        )
        (#T 
            (display "error: unknown operation sign!")
            (exit 0)
        )
    )
)

(define (calcExpr expr)
    (cond
        ((null? expr) '())
        ((char? (car expr))
            (cond
                ((or (null? (cdr expr)) (null? (cdr (cdr expr))))
                    (display "error: check expression one more time!")
                    (exit)
                )
                (
                    (and 
                        (number? (car (cdr expr))) 
                        (number? (car (cdr (cdr expr))))
                    )
                        (calcSimpleExpr 
                            (cdr (cdr (cdr expr)))
                            (car expr) 
                            (car (cdr expr)) 
                            (car (cdr (cdr expr)))
                        )
                )
                (#T (calcExpr (cons (car expr) (calcExpr (cdr expr)))))
            )
        )
        (#T 
            (cond
                ((allNums expr) expr)
                (#T (calcExpr (cons (car expr) (calcExpr (cdr expr)))))
            )
        )
    )
)

(define (getExprValue symExpr varsVals)
    (car 
        (calcExpr 
            (makeExpr 
                (string->list symExpr) 
                (getVarValPairs 
                    (delDupls 
                        (sort (varsList (string->list symExpr)) char<?)
                    ) 
                    varsVals
                )
            )
        )
    )
)

(use extras)
(display 
    (getExprValue 
        (car (command-line-arguments)) 
        (cdr (command-line-arguments))
    )
)
(display "\n")
