;;;Program by Zak Manley, 2018
;;;Comissioned by Simpson Shaw Serveyors Ltd.

(defun c:3ZCHAIN (/ polyName vScale)

(princ "\n")

(setq vdebug 0)
(setq osnapOld (getvar "OSMODE"))                                                                        ;get OSnap

(setq echoOld (getvar "cmdecho"))                                                                        ;turn off echo
(setvar "cmdecho" 0)

;(command "_.UNDO" "BEGIN")
(princ "\n")

(if (not(tblsearch "layer" "plot"))                                                                      ;check for plot layer
  (and
    (command "_.layer" "New" "plot" "")
	(princ "\nPlot layer didn't exist, created\n")
  )
)

(command "_.style" "Standard" "Arial" "" "" "" "" "")                                                    ;set Standard text style to Arial
(if(= vdebug 1)(and(princ "\n{Set Standard text style to Arial}\n")))

(setq vScale 1)

  ;;Get Polyline from user input.
  (setq polyName (entsel "Select Polyline: "))
  (while (NULL polyName)
	(princ "No Object\n")
	(setq polyName (entsel "Select Polyline: "))
  )
  (while (/= (cdr(assoc 0 (entget(car polyName)))) "LWPOLYLINE")
	(princ "Object Polyline\n")
	(setq polyName (entsel "Select Polyline: "))
  )
  (princ "Selected\n")
  
)


(defun getthenumberofvertex (polyName / nVert)
  (setq nVert (assoc 90 (entget polyName)))
  nVert
)

(defun getthecoordsofvertex (polyName nVert / )
  (setq listthing '())
  (setq i 14)
  (while (< i (+ (* 4 nVert) 14))
    (setq listthing (consappend (nth i (entget polyName)) listthing))
    (setq i (+ i 4))
  )
  listthing
)

(defun getanglelist ()
  (setq thing1 0)
  (setq thing1 

(defun getvDist (polyName nVert / newEnt newitem1 distXYList i string1 distvList)                 ;routine to get the distance between each vertex
(if(= vdebug 1)(and(princ "\n{Entered getvDist}\n")))
  (setq distXYList '())
  (setq	newEnt (entnext polyName))
  (setq i 0)
  (while (< i nVert)
    (setq newitem1 '())
    (setq newitem1 (consappend (nth 1 (assoc 10 (entget newEnt))) newitem1))
    (setq newitem1 (consappend (nth 2 (assoc 10 (entget newEnt))) newitem1))
    (setq distXYList (consappend newitem1 distXYList))
    (setq newEnt (entnext newEnt))
    (setq i (+ i 1))
  )
  (setq distvList (calcvDist distXYList nVert))
(if(= vdebug 1)(and(princ "\n{Exited getvDist}\n")))
  distvList
)


(defun calcvDist (distXYList nVert / X1 Y1 X2 Y2 i string1 dist1 distancesList1)                         ;routine to calculate the distance between all vertexes
(if(= vdebug 1)(and(princ "\n{Entered calcvDist}\n")))
  (setq dist1 0)
  (setq distancesList1 '())
  (setq distancesList1 (consappend 0.0 distancesList1))
  (setq i 0)
  (while (< i (- nVert 1))
    (setq X1 (car (nth i distXYList)))
    (setq Y1 (cadr (nth i distXYList)))
    (setq X2 (car (nth (+ i 1) distXYList)))
    (setq Y2 (cadr (nth (+ i 1) distXYList)))
    
    (setq dist1 (sqrt (+ (expt (- X2 X1) 2) (expt (- Y2 Y1) 2))))
    
    (setq distancesList1 (consappend dist1 distancesList1))
    (setq i (+ i 1))
  )
(if(= vdebug 1)(and(princ "\n{Exited calcvDist}\n")))
  distancesList1
)


(defun insvchmarks ()
  (command "_insert" "N:/Staff Directories/Zak/Blocks/AutoPlot/plotVertical_N.dwg" (nth i XYlist) vScale vScale "0" n1)
)