#!/usr/local/bin/sbcl --script

(defun varsList (expr)
    (cond
        ((null expr) '())
        ((and (char>= (car expr) #\a) (char<= (car expr) #\z)) 
            (cons (car expr) (varsList (cdr expr)))
        )
        (T (varsList (cdr expr)))
    )
)

(defun getVarValPairs (vars vals)
    (cond
        ((and (null vars) (null vals)) '())
        ((or (null vars) (null vals))
            (princ "error: check variables and its values one more time!")
            (terpri)
            (exit)
        )
        (T 
            (cons 
                (cons (car vars) (parse-integer (car vals))) 
                (getVarValPairs (cdr vars) (cdr vals))
            )
        )
    )
)

(defun takeNum (expr exprHeadNum)
    (cond
        ((null expr) (cons (parse-integer exprHeadNum) expr))
        ((and (char>= (car expr) #\0) (char<= (car expr) #\9))
            (takeNum 
                (cdr expr) 
                (concatenate 'string exprHeadNum (string (car expr)))
            )
        )
        (T (cons (parse-integer exprHeadNum) expr))
    )
)

(defun takeVarVal (var varValPairs)
    (cond
        ((null varValPairs) 
            (princ "error: can't find this variable value!")
            (terpri)
            (exit)
        )
        ((char= (car (car varValPairs)) var)
            ; (write (cdr (car varValPairs)))
            (cdr (car varValPairs))
        )
        (T (takeVarVal var (cdr varValPairs)))
    )
)

(defun makeExpr (symExpr varValPairs)
    (cond
        ((null symExpr) '())
        ((characterp (car symExpr))
            (cond
                (
                    (and 
                        (char>= (car symExpr) #\a) 
                        (char<= (car symExpr) #\z)
                    )
                    (makeExpr 
                        (cons 
                            (takeVarVal (car symExpr) varValPairs) 
                            (cdr symExpr)
                        ) 
                        varValPairs
                    )
                )
                (
                    (and 
                        (char>= (car symExpr) #\0) 
                        (char<= (car symExpr) #\9)
                    )
                    (makeExpr (takeNum symExpr "") varValPairs)
                )
                ((char= (car symExpr) #\space)
                    (makeExpr (cdr symExpr) varValPairs)
                )
                (T 
                    (cons 
                        (car symExpr) 
                        (makeExpr (cdr symExpr) varValPairs)
                    )
                )
            )
        )
        (T 
            (cons 
                (car symExpr) 
                (makeExpr (cdr symExpr) varValPairs)
            )
        )
    )
)

(defun allNums (expr)
    (cond
        ((null expr) T)
        ((numberp (car expr)) (allNums (cdr expr)))
        (T '())
    )
)

(defun calcSimpleExpr (expr sign arg1 arg2 numStack)
    (cond
        ((char= sign #\+)
            (calcExpr
                (cdr expr) 
                (cons (+ arg1 arg2) numStack)
            )
        )
        ((char= sign #\-)
            (calcExpr
                (cdr expr) 
                (cons (- arg1 arg2) numStack)
            )
        )
        ((char= sign #\*)
            (calcExpr
                (cdr expr) 
                (cons (* arg1 arg2) numStack)
            )
        )
        ((char= (car expr) #\/)
            (cond
                ((= arg2 0)
                    (princ "error: it is impossible to divide by zero!")
                    (terpri)
                    (exit)
                )
                (T
                    (calcExpr
                        (cdr expr) 
                        (cons (truncate arg1 arg2) numStack)
                    )
                )
            )
        )
        ((char= (car expr) #\%)
            (cond
                ((= arg2 0)
                    (princ "error: it is impossible to divide by zero!")
                    (terpri)
                    (exit)
                )
                (T
                    (calcExpr
                        (cdr expr) 
                        (cons (rem arg1 arg2) numStack)
                    )
                )
            )
        )
        (T 
            (princ "error: check expression one more time!")
            (terpri)
            (exit)
        )
    )
)

(defun calcExpr (expr numStack)
    (cond 
        ((null expr)
            (cond
                ((null numStack)
                    (princ "error: check expression one more time!")
                    (terpri)
                    (exit)
                )
                ((null (cdr numStack)) numStack)
                (T 
                    (princ "error: check expression one more time!")
                    (terpri)
                    (exit)
                )
            )
        )
        ((numberp (car expr))
            (calcExpr
                (cdr expr)
                (cons (car expr) numStack)
            )
        )
        ((characterp (car expr))
            (cond
                ((or 
                    (null numStack)
                    (null (cdr numStack)))
                        (princ "error: check expression one more time!")
                        (terpri)
                        (exit)
                )
                (T
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
        (T 
            (princ "error: check expression one more time!")
            (terpri)
            (exit)
        )
    )
)

(defun getValsList (vals)
    (cond
        ((null vals) '())
        ((numberp (car vals)) 
            (cons (write-to-string (car vals)) (getValsList (cdr vals)))
        )
        ((char= (car vals) #\space) 
            (getValsList (cdr vals))
        )
        ((or (char= (car vals) #\-) (char= (car vals) #\+))
            (cond
                ((null (cdr vals))
                    (princ "error: check string of variables values!")
                    (terpri)
                    (exit)
                )
                ((and 
                    (char>= (car (cdr vals)) #\0) 
                    (char<= (car (cdr vals)) #\9))
                        (getValsList (takeNum (cdr vals) (string (car vals))))
                )
                (T 
                    (princ "error: check string of variables values!")
                    (terpri)
                    (exit)
                )
            )
        )
        ((and (char>= (car vals) #\0) (char<= (car vals) #\9))
            (getValsList (takeNum vals ""))
        )
        (T
            (princ "error: check string of variables values!")
            (terpri)
            (exit)
        )
    )
)

(defun getExprVal (symExpr varsVals)
    (car 
        (calcExpr 
            (makeExpr 
                (coerce symExpr 'list) 
                (getVarValPairs 
                    (varsList (coerce symExpr 'list))
                    varsVals
                )
            )
            '()
        )
    )
)

(defun checkForVals (expr)
    (cond
        ((varsList (coerce expr 'list)) 
            (printExprValWithVals expr)
        )
        (T (printExprVal expr))
    )
)

(defun printExprVal (expr)
    (princ (getExprVal expr '()))
    (terpri)
)

(defun printExprValWithVals (expr)
    (princ 
        (getExprVal
            expr
            (let ((vals (read-line *standard-input* nil nil)))
                (cond
                    ((null vals)
                        (exit) 
                    )
                    (T (getValsList (coerce vals 'list)))
                )
            )
        )
    )
    (terpri)
    (printExprValWithVals expr)
)

(checkForVals (car (cdr *posix-argv*)))