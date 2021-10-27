;;;Find the collision point of two curves and move the first curve to the tangent.
;;;Program by Zak Manley, 2018
;;;Comissioned by Simpson Shaw Serveyors Ltd.

(defun c:PZC (/ osnapOld 3ZCirc1Name 3ZCirc2Name 3ZpolyXYlist 3ZRad1 3ZRad2)

(princ "\n")

  (setq osnapOld (getvar "OSMODE"))
  (setvar "CMDECHO" 0)
  
  (command "_.UNDO" "BEGIN")
  (princ "\n")

  (setq 3ZCirc1Name (entsel "Select First Circle/Arc: "))                                                     ;Get first Circle/Arc from user input.
  (while (NULL 3ZCirc1Name)
	(princ "No Object\n")
	(setq 3ZCirc1Name (entsel "Select First Circle/Arc: "))
  )
  (while (and (/= (cdr(assoc 0 (entget(car 3ZCirc1Name)))) "CIRCLE") (/= (cdr(assoc 0 (entget(car 3ZCirc1Name)))) "ARC"))
	(princ "Object must be a Circle/Arc\n")
	(setq 3ZCirc1Name (entsel "Select First Circle/Arc: "))
  )
  (princ "Selected ")
  (princ (strcase (cdr(assoc 0 (entget(car 3ZCirc1Name)))) T))
  (princ ".\n")
  
  (setq 3ZCirc2Name (entsel "Select Second Circle/Arc: "))                                                    ;Get second Circle/Arc from user input.
  (while (NULL 3ZCirc2Name)
	(princ "No Object\n")
	(setq 3ZCirc2Name (entsel "Select Second Circle/Arc: "))
  )
  (while (and (/= (cdr(assoc 0 (entget(car 3ZCirc2Name)))) "CIRCLE") (/= (cdr(assoc 0 (entget(car 3ZCirc2Name)))) "ARC"))
	(princ "Object must be a Circle/Arc\n")
	(setq 3ZCirc2Name (entsel "Select Second Circle/Arc: "))
  )
  (princ "Selected ")
  (princ (strcase (cdr(assoc 0 (entget(car 3ZCirc2Name)))) T))
  (princ ".\n")
  
  (setq 3ZRad1 (3ZgetRad (car 3ZCirc1Name)))                                                                  ;Get the radius of the first Circle/Arc
  (princ (strcat "\nRadius 1: " (rtos 3ZRad1) "\n"))
  (setq 3ZRad2 (3ZgetRad (car 3ZCirc2Name)))                                                                  ;Get the radius of the second Circle/Arc
  (princ (strcat "\nRadius 2: " (rtos 3ZRad2) "\n"))
  
  (setq 3ZDist 0)
  (setq 3ZDist (3ZCalcDist (car 3ZCirc1Name) (car 3ZCirc2Name)))
  (princ (strcat "\nDistance between: " (rtos 3ZDist) "\n"))
  
  (if (<= 3ZDist (+ 3ZRad1 3ZRad2))
    (3ZCE osnapOld)
  )
  
  (setq 3ZDirection 0)                                                                                        ;Get movement direction from user input.
  (while (and (/= 3ZDirection 1) (/= 3ZDirection 2) (/= 3ZDirection 3) (/= 3ZDirection 4) (/= 3ZDirection 6) (/= 3ZDirection 7) (/= 3ZDirection 8) (/= 3ZDirection 9))
    (setq 3ZDirection (getstring "Direction to move?: "))
    (setq 3ZDirection (atoi 3ZDirection))
    (if (and (/= 3ZDirection 1) (/= 3ZDirection 2) (/= 3ZDirection 3) (/= 3ZDirection 4) (/= 3ZDirection 6) (/= 3ZDirection 7) (/= 3ZDirection 8) (/= 3ZDirection 9))
      (princ "\nInvalid direction, please re-enter.\n")
    )
  )
  
  (setvar "OSMODE" 0)
  
  (3Zmove (car 3ZCirc1Name) 3ZDist 3ZRad1 3ZRad2 3ZDirection)
  
  (setvar "OSMODE" osnapOld)
  (command "_.UNDO" "END")
  (setvar "CMDECHO" 1)
  
  (princ "\nDone.\n")
  (princ)
)

(defun 3ZgetRad (3ZEntity / assoc40)                                                                          ;Routine to get the radius of the Circle/Arc
  (setq assoc40 (cdr (assoc 40 (entget 3ZEntity))))
  assoc40
)

(defun 3Zmove (3ZEntity 3ZDist 3ZRad1 3ZRad2 3ZDirection / i 3ZSteps k 3ZCAccuracy)

  ;(princ (strcat "\nStep accuracy: " (rtos 3ZSteps) "\n"))
  
  (setq 3ZCAccuracy 1)
  (setq 3ZSteps 1.00)
  (while (> 3ZDist (+ 3ZRad1 3ZRad2 3ZSteps))
  
    (if (< (- 3ZDist (+ 3ZRad1 3ZRad2)) 10.0)
	  (setq 3ZCAccuracy 1)
	)
	(if (< (- 3ZDist (+ 3ZRad1 3ZRad2)) 1.0)
	  (setq 3ZCAccuracy 2)
	)
	(if (< (- 3ZDist (+ 3ZRad1 3ZRad2)) 0.1)
	  (setq 3ZCAccuracy 3)
	)
	(if (< (- 3ZDist (+ 3ZRad1 3ZRad2)) 0.01)
	  (setq 3ZCAccuracy 4)
	)
	(if (< (- 3ZDist (+ 3ZRad1 3ZRad2)) 0.001)
	  (setq 3ZCAccuracy 5)
	)
	(if (< (- 3ZDist (+ 3ZRad1 3ZRad2)) 0.0001)
	  (setq 3ZCAccuracy 7)
	)
  
    (setq i 0)
    (setq 3ZSteps 1.00)
    (while (< i 3ZCAccuracy)
      (setq 3ZSteps (/ 3ZSteps 10))
	  (setq i (+ i 1))
    )
	
    (setq k 0.0)
  
    (if (= 3ZDirection 8)
      (setq k (strcat "0.0" "," (rtos 3ZSteps) "," "0.0"))                                                      ;N
    )
    (if (= 3ZDirection 9)
      (setq k (strcat (rtos 3ZSteps) "," (rtos 3ZSteps) "," "0.0"))                                             ;NE
    )
    (if (= 3ZDirection 6)
      (setq k (strcat (rtos 3ZSteps) "," "0.0" "," "0.0"))                                                      ;E
    )
    (if (= 3ZDirection 3)
      (setq k (strcat (rtos 3ZSteps) "," (rtos (- 0 3ZSteps)) "," "0.0"))                                       ;SE
    )
    (if (= 3ZDirection 2)
      (setq k (strcat "0.0" "," (rtos (- 0 3ZSteps)) "," "0.0"))                                                ;S
    )
    (if (= 3ZDirection 1)
      (setq k (strcat (rtos (- 0 3ZSteps)) "," (rtos (- 0 3ZSteps)) "," "0.0"))                                 ;SW
    )
    (if (= 3ZDirection 4)
      (setq k (strcat (rtos (- 0 3ZSteps)) "," "0.0" "," "0.0"))                                                ;W
    )
    (if (= 3ZDirection 7)
      (setq k (strcat (rtos (- 0 3ZSteps)) "," (rtos 3ZSteps) "," "0.0"))                                       ;NW
    )
    (if (and (/= 3ZDirection 1) (/= 3ZDirection 2) (/= 3ZDirection 3) (/= 3ZDirection 4) (/= 3ZDirection 6) (/= 3ZDirection 7) (/= 3ZDirection 8) (/= 3ZDirection 9))
	  (3Zwoa)
    )
  
    ;(princ (strcat "\nk: " k "\n"))
  
    (command "_.MOVE" 3ZEntity "" "D" k)
	(setq 3ZDist (3ZCalcDist (car 3ZCirc1Name) (car 3ZCirc2Name)))
  )
)

(defun 3ZCalcDist (3ZCirc1 3ZCirc2 / X1 Y1 X2 Y2 3ZD)                                                         ;Routine to calculate the distance between the curves
  (setq 3ZD 0)
    
  (setq X1 (nth 1 (assoc 10 (entget 3ZCirc1))))
  (setq Y1 (nth 2 (assoc 10 (entget 3ZCirc1))))
  (setq X2 (nth 1 (assoc 10 (entget 3ZCirc2))))
  (setq Y2 (nth 2 (assoc 10 (entget 3ZCirc2))))
    
  (setq 3ZD (sqrt (+ (expt (- X2 X1) 2) (expt (- Y2 Y1) 2))))
  
  3ZD
)

(defun 3ZCE (osnapOld /)
  (princ "\nCurves have already intersected.\n")
  (setvar "OSMODE" osnapOld)
  (command "_.UNDO" "END")
  (setvar "CMDECHO" 1)
  (princ "Exiting.\n")
  (exit)
)

(defun 3Zwoa ()
  (princ "\nwoa woa woa, that's not supposed to happen! how did you do that?!\n")
  (exit)
)

(prompt "\nEnter PZC to collide Circles/Arcs.\n")