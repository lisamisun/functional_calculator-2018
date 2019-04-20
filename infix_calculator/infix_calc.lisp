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

(defun delDupls (sortedList)
    (cond
        ((null sortedList) '())
        ((null (cdr sortedList)) sortedList)
        ((char= (car sortedList) (car (cdr sortedList)))
            (delDupls (cdr sortedList))
        )
        (T (cons (car sortedList) (delDupls (cdr sortedList))))
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
                ((and (char>= (car symExpr) #\a) (char<= (car symExpr) #\z))
                    (makeExpr 
                        (cons 
                            (takeVarVal (car symExpr) varValPairs) 
                            (cdr symExpr)
                        ) 
                        varValPairs
                    )
                )
                ((and (char>= (car symExpr) #\0) (char<= (car symExpr) #\9))
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
        (T (cons (car symExpr) (makeExpr (cdr symExpr) varValPairs)))
    )
)

(defun allNums (expr)
    (cond
        ((null expr) T)
        ((numberp (car expr)) (allNums (cdr expr)))
        (T '())
    )
)

(defun calcSimpleExpr (expr sign arg1 arg2)
    (cond
        ((char= sign #\+) 
            (calcExpr
                (cons
                    (+ arg1 arg2)
                    (calcExpr expr)
                )
            )
        )
        ((char= sign #\-) 
            (calcExpr
                (cons
                    (- arg1 arg2)
                    (calcExpr expr)
                )
            )
        )
        ((char= sign #\*) 
            (calcExpr
                (cons
                    (* arg1 arg2)
                    (calcExpr expr)
                )
            )
        )
        ((char= sign #\/) 
            (calcExpr
                (cond
                    ((= arg2 0)
                        (princ "error: it is impossible to divide by zero!")
                        (terpri)
                        (exit)
                    )
                    (T (cons (truncate arg1 arg2) (calcExpr expr)))
                )
            )
        )
        ((char= sign #\%) 
            (calcExpr
                (cond
                    ((= arg2 0)
                        (princ "error: it is impossible to divide by zero!")
                        (terpri)
                        (exit)
                    )
                    (T (cons (rem arg1 arg2) (calcExpr expr)))
                )
            )
        )
        (T 
            (princ "error: unknown operation sign!")
            (terpri)
            (exit)
        )
    )
)

(defun calcExpr (expr)
    (cond
        ((null expr) '())
        ((characterp (car expr))
            (cond
                (
                    (or
                        (null (cdr expr))
                        (null (cdr (cdr expr)))
                    )
                        (princ "error: check expression one more time!")
                        (terpri)
                        (exit)
                )
                (
                    (and 
                        (numberp (car (cdr expr))) 
                        (numberp (car (cdr (cdr expr))))
                    )
                        (calcSimpleExpr 
                            (cdr (cdr (cdr expr)))
                            (car expr) 
                            (car (cdr expr)) 
                            (car (cdr (cdr expr)))
                        )
                )
                (T (calcExpr (cons (car expr) (calcExpr (cdr expr)))))
            )
        )
        (T 
            (cond
                ((allNums expr) expr)
                (T (calcExpr (cons (car expr) (calcExpr (cdr expr)))))
            )
        )
    )
)

(defun getExprValue (symExpr varsVals)
    (car 
        (calcExpr 
            (makeExpr 
                (coerce symExpr 'list) 
                (getVarValPairs 
                    (delDupls 
                        (sort (varsList (coerce symExpr 'list)) #'char-lessp)
                    ) 
                    varsVals
                )
            )
        )
    )
)

(princ 
    (getExprValue 
        (car (cdr *posix-argv*)) 
        (cdr (cdr *posix-argv*))
    )
)
(terpri)