;;;Program by Zak Manley, 2018
;;;Comissioned by Simpson Shaw Serveyors Ltd.
;;;Revision 2.0.0

(defun c:3Zshrink (/ osnapOld echoOld 3ZPolyName 3ZnVert 3ZShrinkAmount 3Zoldmod 3Znewmod ci)
  (princ "\n")
;(setq osnapOld (getvar "OSMODE"))                                                                        ;get OSnap

(setq echoOld (getvar "cmdecho"))                                                                        ;turn off echo
(setvar "cmdecho" 0)

(command "_.UNDO" "BEGIN")
(princ "\n")


  ;;Get Polyline from user input.
  (setq 3ZPolyName (entsel "Select line to shrink: "))
  (while (NULL 3ZPolyName)
	(princ "No Object\n")
	(setq 3ZPolyName (entsel "Select line to shrink: "))
  )
  (while (or (/= (cdr(assoc 0 (entget(car 3ZPolyName)))) "LWPOLYLINE") (/= (cdr(assoc 0 (entget(car 3ZPolyName)))) "LINE"))
	(princ "Object must be a 2D Line or 2D Polyline\n")
	(setq 3ZPolyName (entsel "Select line to shrink: "))
  )
  (if (= (cdr(assoc 0 (entget(car 3ZPolyName)))) "LWPOLYLINE")
    (progn (setq shrinktype 0) (princ "Selected polyline\n"))
	(progn (setq shrinktype 1) (princ "Selected line\n"))
  )
  
;(setvar "OSMODE" 0)
  
  (setq 3ZnVert 0)
  (if (= shrinktype 0)
    (setq 3ZnVert (cdr(assoc 90 (entget(car 3ZPolyName)))))
    (if (= 3ZnVert 1)
      (princ (strcat "\nPolyline has " (rtos 3ZnVert) " vertex.\n"))
      (princ (strcat "\nPolyline has " (rtos 3ZnVert) " vertices.\n"))
    )
  )
  (if (> 3ZnVert 2)
    (progn
	  (princ "\nPolyline has too many vertices! Must have a maximum of 2. Exiting.\n")
	  ;(setvar "OSMODE" osnapOld)
      (setvar "cmdecho" echoOld)
	  (command "_.UNDO" "END")
      (princ "\n")
	  (princ)
	  (exit)
	)
  )
  
  (setq ci (3ZgetPreamble 3ZPolyName))
  ;(print ci)
  
  (if (and (/= (nth 2 (nth ci (entget(car 3ZPolyName)))) (nth 2 (nth (+ ci (- (* 3ZnVert 4) 4)) (entget(car 3ZPolyName))))) 
           (/= (nth 1 (nth ci (entget(car 3ZPolyName)))) (nth 1 (nth (+ ci (- (* 3ZnVert 4) 4)) (entget(car 3ZPolyName)))))
	  );set for outlier
    (progn
      (princ "\nPolyline must be 90 degrees polar. Exiting.\n")
      (setvar "cmdecho" echoOld)
	  (command "_.UNDO" "END")
      (princ "\n")
	  (princ)
	  (exit)
	)
  )
  
  (setq 3ZShrinkAmount '())
  (setq 3ZShrinkAmountT 0)
  (setq 3ZShrinkAmountT (getstring "Amount to shrink? (applies to both ends): "))
  ;(setq 3ZShrinkAmount (atoi 3ZShrinkAmountT))
  
  (setq j1 0)
  (setq j2 0)
  (if (= (nth 2 (nth ci (entget(car 3ZPolyName)))) (nth 2 (nth (+ ci (- (* 3ZnVert 4) 4)) (entget(car 3ZPolyName)))));set for x
    (progn
	  (if (> (nth 1 (nth ci (entget(car 3ZPolyName)))) (nth 1 (nth (+ ci (- (* 3ZnVert 4) 4)) (entget(car 3ZPolyName)))))
        (setq j1 (- 0 (atoi 3ZShrinkAmountT)))
		(setq j1 (atoi 3ZShrinkAmountT))
      )
	  (print j1)
      (setq 3ZShrinkAmount (consappend j1 3ZShrinkAmount))
      (setq 3ZShrinkAmount (consappend 0 3ZShrinkAmount))
	  (print 3ZShrinkAmount)
    )
  )
  (if (= (nth 1 (nth ci (entget(car 3ZPolyName)))) (nth 1 (nth (+ ci (- (* 3ZnVert 4) 4)) (entget(car 3ZPolyName)))));set for y
    (progn
	  (if (> (nth 2 (nth ci (entget(car 3ZPolyName)))) (nth 2 (nth (+ ci (- (* 3ZnVert 4) 4)) (entget(car 3ZPolyName)))))
        (setq j2 (- 0 (atoi 3ZShrinkAmountT)))
		(setq j2 (atoi 3ZShrinkAmountT))
      )
	  (print j2)
      (setq 3ZShrinkAmount (consappend 0 3ZShrinkAmount))
      (setq 3ZShrinkAmount (consappend j2 3ZShrinkAmount))
	  (print 3ZShrinkAmount)
    )
  )
  
  ;(print 3ZShrinkAmount)
  
  (setq newtfst '())
  (setq newtfst (consappend 10 newtfst))
  (setq newtfst (consappend (+ (nth 1 (nth ci (entget(car 3ZPolyName)))) (nth 0 3ZShrinkAmount)) newtfst))
  (setq newtfst (consappend (+ (nth 2 (nth ci (entget(car 3ZPolyName)))) (nth 1 3ZShrinkAmount)) newtfst))
  ;(print newtfst)
  
  (setq newtlst '())
  (setq newtlst (consappend 10 newtlst))
  (setq newtlst (consappend (- (nth 1 (nth (+ ci (- (* 3ZnVert 4) 4)) (entget(car 3ZPolyName)))) (nth 0 3ZShrinkAmount)) newtlst))
  (setq newtlst (consappend (- (nth 2 (nth (+ ci (- (* 3ZnVert 4) 4)) (entget(car 3ZPolyName)))) (nth 1 3ZShrinkAmount)) newtlst))
  ;(print newtlst)
  
  (princ "\nShrinking...")
  (setq 3Zoldmod (nth ci (entget(car 3ZPolyName))))
  (setq 3Znewmod newtfst)
  (3ZmodPoly 3ZPolyName 3Zoldmod 3Znewmod)
  (setq 3Zoldmod (nth (+ ci (- (* 3ZnVert 4) 4)) (entget(car 3ZPolyName))))
  (setq 3Znewmod newtlst)
  (3ZmodPoly 3ZPolyName 3Zoldmod 3Znewmod)
  
  ;(command "_.ERASE" 3ZPolyName "")
  
;(setvar "OSMODE" osnapOld)
(setvar "cmdecho" echoOld)
(command "_.UNDO" "END")
(princ "\n")
  (princ "\nDone.")
  (princ)  
)

(defun 3ZgetPreamble (3ZPolyName / asock i megacount)
  (setq asock 0)
  (setq i 0)
  (while (/= asock 10)
    (setq asock (nth 0 (nth i (entget(car 3ZPolyName)))))
	(setq i (+ i 1))
  )
  (setq megacount (- i 1))
  megacount
)

(defun 3ZmodPoly (3ZPolyName 3Zoldmod 3Znewmod / 3Zmodlist)
  (setq 3Zmodlist (entget (car 3ZPolyName)))
  (setq 3Zmodlist (subst 3Znewmod 3Zoldmod 3Zmodlist))
  (entmod 3Zmodlist)
)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  