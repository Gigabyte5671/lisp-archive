;;;Program by Zak Manley, 2018
;;;Comissioned by Simpson Shaw Serveyors Ltd.

(defun c:3Zrev (/ osnapOld echoOld 3ZPolyName 3ZnVert)
  (princ "\n")
(setq osnapOld (getvar "OSMODE"))                                                                        ;get OSnap

(setq echoOld (getvar "cmdecho"))                                                                        ;turn off echo
(setvar "cmdecho" 0)

(command "_.UNDO" "BEGIN")
(princ "\n")


  ;;Get Polyline from user input.
  (setq 3ZPolyName (entsel "Select polyline to reverse: "))
  (while (NULL 3ZPolyName)
	(princ "No Object\n")
	(setq 3ZPolyName (entsel "Select polyline to reverse: "))
  )
  (while (/= (cdr(assoc 0 (entget(car 3ZPolyName)))) "LWPOLYLINE")
	(princ "Object must be a 2D Polyline\n")
	(setq 3ZPolyName (entsel "Select polyline to reverse: "))
  )
  (princ "Selected polyline\n")
  
(setvar "OSMODE" 0)
  
  (setq 3ZnVert (cdr(assoc 90 (entget(car 3ZPolyName)))))
  (if (= 3ZnVert 1)
    (princ (strcat "\nPolyline has " (rtos 3ZnVert) " vertex.\n"))
    (princ (strcat "\nPolyline has " (rtos 3ZnVert) " vertices.\n"))
  )
  
  (setq 3ZvertList (3ZgetVertices 3ZPolyName 3ZnVert))
  
  (3ZdrawPoly 3ZvertList)
  
  (command "_.ERASE" 3ZPolyName "")
  
(setvar "OSMODE" osnapOld)
  (princ "\nDone.")
  (princ)  
)


(defun 3ZgetVertices (3ZPolyName 3ZnVert / vert i j k 3ZvertList)
  (setq j 0)
  (setq k 0)
  (while (/= j 10)
    (setq j (car (nth k (entget(car 3ZPolyName)))))
	(setq k (+ k 1))
  )

  (setq 3ZvertList '())
  (setq i 0)
  (while (< i (* 3ZnVert 4))
    ;(print (+ i k))
    (setq vert '())
    (setq vert (consappend (nth 1 (nth (+ i (- k 1)) (entget(car 3ZPolyName)))) vert))
	(setq vert (consappend (nth 2 (nth (+ i (- k 1)) (entget(car 3ZPolyName)))) vert))
	(setq 3ZvertList (consappend vert 3ZvertList))
	(setq i (+ i 4))
  )
  (setq 3ZvertList (reverse 3ZvertList))
  3ZvertList
)

(defun 3ZdrawPoly (3ZvertList / 3Zclayer 3Zlayer Pt 3ZplinegenOld)
  ;(setq 3Zclayer (getvar "CLAYER"))
  ;(setq 3Zlayer (cdr(assoc 8 (entget(car 3ZPolyName)))))
  ;(setvar "CLAYER" 3Zlayer)
  (setq 3ZplinegenOld (getvar "PLINEGEN"))
  (if (= (cdr(assoc 70 (entget(car 3ZPolyName)))) 128)
    (setvar "PLINEGEN" 1)
  )
  
  (command "_pline")
  (foreach Pt 3ZvertList (command Pt))
  (command "")
  
  (command "chprop" "_last" "" "la" (cdr(assoc 8 (entget(car 3ZPolyName)))) "")
  (if (assoc 6 (entget(car 3ZPolyName)))
    (command "chprop" "_last" "" "lt" (cdr(assoc 6 (entget(car 3ZPolyName)))) "")
  )
  (if (assoc 48 (entget(car 3ZPolyName)))
    (command "chprop" "_last" "" "s" (cdr(assoc 48 (entget(car 3ZPolyName)))) "")
  )
  (if (assoc 370 (entget(car 3ZPolyName)))
    (command "chprop" "_last" "" "lw" (cdr(assoc 370 (entget(car 3ZPolyName)))) "")
  )
  (setvar "PLINEGEN" 3ZplinegenOld)
)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  