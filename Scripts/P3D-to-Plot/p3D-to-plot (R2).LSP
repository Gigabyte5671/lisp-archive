;;;Plot a Longsectional graph from a single, or pair, of 3D polylines.
;;;Program by Zak Manley, 2018
;;;Comissioned by Simpson Shaw Serveyors Ltd.
;;;Revision 2.0.0

;;;;;Global Variables Key;;;;;
;|
surfacePolyName      ;The entity name of the surface polyline
invertPolyName       ;The entity name of the invert polyline
surfacevZ            ;List of Z coordinates from the surface polyline
surfacevXY           ;List of XY coordinates from the surface polyline
invertvZ             ;List of Z coordinates from the invert polyline
invertvXY            ;List of XY coordinates from the invert polyline
nVert                ;The number of vertices in the generic polyline
nVertS               ;The number of vertices in the surface polyline
nVertI               ;The number of vertices in the invert polyline
placeV               ;The coordinate of the selected placement of the plot
placevX              ;The X coordinate of the selected placement of the plot
placevY              ;The Y coordinate of the selected placement of the plot
Voffset              ;The constant vertical offset for drawing the plot line above the title/data block
serviceColour        ;The colour of the plot line (indicates service)
MHON                 ;The input string for drawing manholes ON/OFF
gradevList           ;The list of gradients between points
echoOld              ;The stored "cmdecho" value for restoring once drawing has finished
osnapOld             ;The stored "osnap" value for restoring once drawing has finished
|;

;====================================================================================================================================================================================;
;====================================================================================MAIN PROGRAM====================================================================================;
;====================================================================================================================================================================================;

(defun c:3tp2 (/ vdebug osnapOld echoOld surfacePolyName invertPolyName surfacevZ surfacevXY invertvZ invertvXY nVertS nVertI placeV placevX placevY Voffset vOffsetMode serviceColour MHON gradevList chainvListS chainvListI vDwgunits)

(princ "\n")

(setq vdebug 0)
(setq osnapOld (getvar "OSMODE"))                                                                        ;get OSnap

(setq echoOld (getvar "cmdecho"))                                                                        ;turn off echo
(setvar "cmdecho" 0)

(command "_.UNDO" "BEGIN")
(princ "\n")

(if (not(tblsearch "layer" "plot"))                                                                      ;check for plot layer
  (and
    (command "_.layer" "New" "plot" "")
	(if(= vdebug 1)(and(princ "\n{Plot layer didn't exist, created}\n")))
  )
)

(command "_.style" "Standard" "Arial" "" "" "" "" "")                                                    ;set Standard text style to Arial
(if(= vdebug 1)(and(princ "\n{Set Standard text style to Arial}\n")))

(setq vDwgunits 0)
(while (and (/= vDwgunits 1) (/= vDwgunits 2))
  (setq vDwgunits (getstring "Drawing Units? [Millimetres(1)/Metres(2)]: "))
  (setq vDwgunits (atoi vDwgunits))
  (if (and (/= vDwgunits 1) (/= vDwgunits 2))
    (princ "\nInvalid scale, please re-enter.\n")
  )
)

;;Get Service Colour from user input.
  (setq serviceColour (getstring "Service Colour [1-7]: "))
  (if (= serviceColour "-0")
    (and
	  (setq vdebug 1)
	  (princ "\nVerbose Logging On.\n")
	  (setq serviceColour (getstring "Service Colour [1-7]: "))
	)
  )

;;Get Surface 3D Polyline from user input.
  (setq surfacePolyName (entsel "Select Surface 3D Polyline: "))
  (while (NULL surfacePolyName)
	(princ "No Object\n")
	(setq surfacePolyName (entsel "Select Surface 3D Polyline: "))
  )
  (while (/= (cdr(assoc 0 (entget(car surfacePolyName)))) "POLYLINE")
	(princ "Object must be a 3D Polyline\n")
	(setq surfacePolyName (entsel "Select Surface 3D Polyline: "))
  )
  (princ "Selected Surface\n")
  
;;Get Invert 3D Polyline from user input.
  (if (or (= serviceColour "1") (= serviceColour "5"))
    (and
      (and
        (setq invertPolyName (entsel "Select Invert 3D Polyline: "))
	    (while (NULL invertPolyName)
	      (princ "No Object\n")
       	  (setq surfacePolyName (entsel "Select Invert 3D Polyline: "))
        )
	    (while (/= (cdr(assoc 0 (entget(car invertPolyName)))) "POLYLINE")
	      (princ "Object must be a 3D Polyline\n")
	      (setq invertPolyName (entsel "Select Invert 3D Polyline: "))
        )
	  )
      (princ "Selected Invert")
    )
	(princ "\nNo Invert")
  )

;;Get input on option to draw Manholes from user input.
  ;(if (or (= serviceColour "1") (= serviceColour "5"))
  ;  (setq MHON (getstring "\nDraw Manholes? [Yes/No]: "))
	(setq MHON 0)
  ;)
  
;;Get location of plot from user input.
  (setq placeV '())
  (setq placeV (getpoint "Select where to place the plot: "))
  (setq placevX (car placeV))
  (setq placevY (cadr placeV))
  (princ "Selected\n")
  
(setvar "OSMODE" 0)                                                                                      ;turn off OSnap
(if(= vdebug 1)(and(princ "\n{Set OSMODE to 0}\n")))
  
  (setq nVertS (getVertexCount (car surfacePolyName)))                                                   ;get the number of vertexes in the surface polyline
  (if(= vdebug 1)(and(princ "\n{Got nVertS}\n")))
  (if (or (= serviceColour "1") (= serviceColour "5"))(and
    (setq nVertI (getVertexCount (car invertPolyName)))                                                  ;get the number of vertexes in the invert polyline
	(if(= vdebug 1)(and(princ "\n{Got nVertI}\n"))))
  )
  
  (setq nVflag 0)
  (if (or (= serviceColour "1") (= serviceColour "5"))
    (if (= nVertI (- (* nVertS 2) 2))
	  (setq nVflag 1)
	  (and
	    (princ "\nMissing Vertices on either Surface or Invert polyline.\n")
		(setvar "OSMODE" osnapOld)
        (setvar "cmdecho" echoOld)                                                                       ;restore echo
		(command "_.UNDO" "END")
		(princ "Exiting.\n")
		(exit)
	  )
	)
  )
  
  (setq Voffset (getVoffset (car surfacePolyName) nVertS))                                               ;calculate the vertical offset with-which to draw the plot above the Title Block
  (if(= vdebug 1)(and(princ "\n{Got Voffset}\n")))	
  
  (setq surfacevZ (getvZ (car surfacePolyName) nVertS Voffset))                                          ;get the Z coordinate of each vertex
  (if(= vdebug 1)(and(princ "\n{Got surfacevZ}\n")))
  (if (or (= serviceColour "1") (= serviceColour "5"))(and
    (setq invertvZ (getvZ (car invertPolyName) nVertI Voffset))                                          ;get the Z coordinate of each vertex of invert line
	(if(= vdebug 1)(and(princ "\n{Got invertvZ}\n"))))
  )
  (setq chainvListS (getvDist (car surfacePolyName) nVertS))                                             ;get the relative chainage of each vertex
  (if(= vdebug 1)(and(princ "\n{Got chainvListS}\n")))
  (if (or (= serviceColour "1") (= serviceColour "5"))(and
    (setq chainvListI (getvDist (car invertPolyName) nVertI))                                            ;get the relative chainage of each vertex
    (if(= vdebug 1)(and(princ "\n{Got chainvListI}\n"))))
  )
  
  (setq vOffsetMode 0)
  (setq somethingvVar (chkvLargest surfacevZ nVertS))
  (princ "\nsomethingvVar: ")3
  (princ somethingvVar)
  (princ "\n")
  
  (if (and (> Voffset 0) (< Voffset 40))
    (setq vOffsetMode 1)
	(if (and (> Voffset 41) (< Voffset 60))
      (setq vOffsetMode 2)
	  (if (and (> Voffset 61) (< Voffset 80))
        (setq vOffsetMode 3)
		(setq vOffsetMode 0)
	  )
	)
  )
  (if(= vdebug 1)(and(princ "\n{Set vOffsetMode}\n")))
  
  (setq surfacevXY (makevXY surfacevZ nVertS placevX placevY chainvListS))                               ;create list of surface x,y coordinates
  (if(= vdebug 1)(and(princ "\n{Got surfacevXY}\n")))
  
  (if (or (= serviceColour "1") (= serviceColour "5"))
    (and
      (setq invertvXY (makevXY invertvZ nVertI placevX placevY chainvListI))                             ;create list of invert x,y coordinates
      (if(= vdebug 1)(and(princ "\n{Got invertvXY}\n")))
	)
  )
  
  (if (or (= serviceColour "1") (= serviceColour "5"))                                                   ;set up gradient param
    (and (setq gvXY invertvXY) (setq gvN nVertI))
    (and (setq gvXY surfacevXY) (setq gvN nVertS))
  )
  (if(= vdebug 1)(and(princ "\n{Set Gradient Param}\n")))
  (setq gradevList (getvGrade gvXY gvN))                                                                 ;get the gradient between each vertex pair (vertex & vertex+1)
  (if(= vdebug 1)(and(princ "\n{Got gradevList}\n")))
  
  
  (if (= vdebug 1)
    (and
  
      (princ "\nChainvListS: ")
      (setq i 0)
      (while (< i nVertS)
       (print (nth i chainvListS))
       (setq i (+ i 1))
      )

      (princ "\nNumber of Vertexes: ")(princ nVertS)
      (princ "\nSurface Z Points: ");(princ surfacevZ)
      (setq i 0)
      (while (< i nVertS)
       (print (- (nth i surfacevZ) Voffset))
       (setq i (+ i 1))
      )
  
      (if (or (= serviceColour "1") (= serviceColour "5"))
        (and
          (princ "\nInvert Z Points: ");(princ surfacevZ)
          (setq i 0)
          (while (< i nVertI)
           (print (- (nth i invertvZ) Voffset))
           (setq i (+ i 1))
          )
    	)
      )
  
      (princ "\nSurface XY Points: \n")
      (setq i 0)
      (while (< i nVertS)
       (princ (- (car (nth i surfacevXY)) placevX))
       (princ ",")
       (princ (- (cadr (nth i surfacevXY)) placevY))
       (princ "\n")
       (setq i (+ i 1))
      )
  
      (princ "\nGradients: ")(princ gradevList)
      (princ "\n")
    )
  )
  
  (drawvPlot surfacevXY invertvXY nVertS nVertI placevX placevY Voffset vOffsetMode surfacevZ invertvZ serviceColour MHON gradevList chainvListS chainvListI vDwgunits) ;draw plot
  (if(= vdebug 1)(and(princ "\n{Completed All Draws}\n")))
  
  (command "_.UNDO" "END")
  (princ "\n")
  
  ;(refreshCurves)
  (if(= vdebug 1)(and(princ "\n{Refreshed All Objects}\n")))
  
  ;(if(= vdebug 1)(and(princ "\n{Completed ")(princ (getvar "OSMODE"))(princ " Consappends}\n")))
(setvar "OSMODE" osnapOld)
(setvar "cmdecho" echoOld)                                                                               ;restore echo
  
  (princ "\nDone, chur.")
  (if(= vdebug 1)(textscr))
  (princ)
)

;====================================================================================================================================================================================;
;===================================================================================DIALOG ROUTINES==================================================================================;
;====================================================================================================================================================================================;

(defun toggleNum()
  (if(= (get_tile "RD") "1")
    (mode_tile "IP" 0)
    (mode_tile "IP" 1)
  )
  (if(or (= (get_tile "SW") "1")(= (get_tile "SS") "1"))
    (mode_tile "MH" 0)
    (mode_tile "MH" 1)
  )
  (if(= (get_tile "WR") "1")
    (mode_tile "ST" 0)
    (mode_tile "ST" 1)
  )
)

;====================================================================================================================================================================================;
;====================================================================================GET ROUTINES====================================================================================;
;====================================================================================================================================================================================;

(defun getVertexCount (vEnt / nEnt n assoc0)                                                             ;routine to get the number of vertexes in the polyline
(if(= vdebug 1)(and(princ "\n{Entered getVertexCount}\n")))
  (setq n 0)
  (setq assoc0 (cdr (assoc 0 (entget vEnt))))
  (setq	nEnt (entnext vEnt))
  (while (/= assoc0 "SEQEND")
    (setq n (+ n 1))
    (setq nEnt (entnext nEnt))
    (setq assoc0 (cdr (assoc 0 (entget nEnt))))
  )
(if(= vdebug 1)(and(princ "\n{Exited getVertexCount}\n")))
  n
)

(defun getVoffset (surfacePolyName nVert / newEnt V vOff1)
(if(= vdebug 1)(and(princ "\n{Entered getVoffset}\n")))
  (setq vOff1 0)
  (setq i 0)
  (setq	newEnt (entnext surfacePolyName))
  (while (< i nVert)                  ;get average Z
    (setq V (nth 3 (assoc 10 (entget newEnt))))
    (setq vOff1 (+ vOff1 V))
    (setq newEnt (entnext newEnt))
    (setq i (+ i 1))
  )
  (setq vOff1 (fix (/ vOff1 nVert)))  ;got average Z
  (setq vOff1 (* vOff1 2))
(if(= vdebug 1)(and(princ "\n{Exited getVoffset}\n")))
  vOff1
)
  
(defun getvZ (vEnt nVert Voffset / i Z Zlist newEnt)                                                     ;routine to get the Z coordinate of each vertex
(if(= vdebug 1)(and(princ "\n{Entered getvZ}\n")))
  (setq Zlist '())
  (setq i 0)
  (setq	newEnt (entnext vEnt))
  (while (< i nVert)
    (setq Z (nth 3 (assoc 10 (entget newEnt))))
    (setq Z (+ Z Voffset))
    (setq Zlist (consappend Z Zlist))
    (setq newEnt (entnext newEnt))
    (setq i (+ i 1))
  )
  Zlist
)

(defun getvGrade (invertvXY nVert / i vX1 vY1 vX2 vY2 vM vP vGrade)                                      ;routine to get and calculate the gradients between each vertex
(if(= vdebug 1)(and(princ "\n{Entered getvGrade}\n")))
  (setq vGrade '())
  (setq vM 0)
  (setq vP 0)
  (setq i 0)
  (while (< i (- nVert 1))
    (setq vX1 (car (nth i invertvXY)))
    (setq vY1 (cadr (nth i invertvXY)))
    (setq vX2 (car (nth (+ i 1) invertvXY)))
    (setq vY2 (cadr (nth (+ i 1) invertvXY)))
    (setq vM (/ (- vY2 vY1) (- vX2 vX1)))
	(setq vP (* vM 100))
    (setq vGrade (consappend vP vGrade))
    (setq i (+ i 1))
  )
(if(= vdebug 1)(and(princ "\n{Exited getvGrade}\n")))
  vGrade
)

(defun getvDist (surfacePolyName nVert / newEnt newitem1 distXYList i string1 distvList)                 ;routine to get the distance between each vertex
(if(= vdebug 1)(and(princ "\n{Entered getvDist}\n")))
  (setq distXYList '())
  (setq	newEnt (entnext surfacePolyName))
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

;====================================================================================================================================================================================;
;================================================================================CALCULATION ROUTINES================================================================================;
;====================================================================================================================================================================================;

(defun consappend (itemt listt / vconscount)                                                             ;routine to append a list with a variable
  (setq listt (reverse listt))
  (setq listt (cons itemt listt))
  (setq listt (reverse listt))
)

(defun makevList (vint nVert / 0list i)                                                                  ;routine to make a list of nVertS length out of an integer
(if(= vdebug 1)(and(princ "\n{Entered makevList}\n")))
  (setq 0list '())
  (setq i 0)
  (while (< i nVert)
    (setq 0list (consappend vint 0list))
    (setq i (+ i 1))
  )
(if(= vdebug 1)(and(princ "\n{Exited makevList}\n")))
  0list
)

(defun makevXY (0list nVert placevX placevY chainvList / i tV XV XYlist runningvChain)                   ;routine to convert surfacevZ (list of Z coords) to list of X,Y coords
(if(= vdebug 1)(and(princ "\n{Entered makevXY}\n")))
  (setq XYlist '())
  (setq tV '())
  (setq XV 0)
  (setq runningvChain 0)
  (setq i 0)
  (while (< i nVert)
    (setq runningvChain (+ runningvChain (nth i chainvList)))
    (setq XV 0)
    (setq XV (+ runningvChain placevX))
    (setq tV '())
    (setq tV (consappend XV tV))
    (setq tV (consappend (+ (nth i 0list) placevY) tV))
    (setq XYlist (consappend tV XYlist))
    (setq i (+ i 1))
  )
(if(= vdebug 1)(and(princ "\n{Exited makevXY}\n")))
  XYlist
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

(defun chkvLargest (surfacevZ nVertS / i vLargest)
  (setq i 0)
  (setq vLargest 0)
  (while (< i nVertS)
    (if (> (nth i surfacevZ) vLargest)
      (setq vLargest (nth i surfacevZ))
	  (setq avv i)
	)
	(setq i (+ i 1))
  )
  vLargest
)	

;====================================================================================================================================================================================;
;========================================================================================DRAW========================================================================================;
;====================================================================================================================================================================================;

(defun drawvPlot (surfacevXY invertvXY nVertS nVertI placevX placevY Voffset vOffsetMode surfacevZ invertvZ serviceColour MHON gradevList chainvListS chainvListI vDwgunits / newivXY)
(if(= vdebug 1)(and(princ "\n{Entered drawvPlot}\n")))
  
  (insvPline surfacevXY serviceColour)                                                                   ;draw surface polyline
  (princ "\nDrawn Surface.")
  
  (if (or (= serviceColour "1") (= serviceColour "5"))
    (and
      (setq newivXY (insvInvertPline invertvXY surfacevXY nVertI))                                       ;draw invert polyline
      (princ "\nDrawn Invert.")
	)
	(setq newivXY 0)
  )
  
  (if (or (eq MHON "y") (eq MHON "Y"))                                                                   ;draw manholes
    (and
      (foreach Pt surfacevXY (insvMH Pt vDwgunits))
	  (princ "\nDrawn Manholes.")
	)
  )
  (if (= serviceColour "7")                                                                              ;draw IPs
    (and
	  (foreach Pt surfacevXY (insvIP Pt vDwgunits))
	  (princ "\nDrawn IPs.")
	)
  )
  
  (insvVerticals nVertS placevX placevY invertvZ surfacevZ Voffset vOffsetMode chainvListS serviceColour vDwgunits);draw verticals and horizontals
  (princ "\nDrawn Descenders.")
  (if (or (= serviceColour "1") (= serviceColour "5"))
    (and
      (insvInvertVerticals newivXY nVertI placevY Voffset vOffsetMode chainvListI vDwgunits)             ;draw verticals and horizontals
      (princ "\nDrawn Invert Descenders.")
	)
  )
  
  (insvTitle placevX placevY Voffset serviceColour vDwgunits)                                            ;draw title block
  (princ "\nDrawn Title Block.")
  
  (insvGradelines nVertS placevX placevY chainvListS vDwgunits)                                          ;draw gradient lines
  (princ "\nDrawn Grade Lines.")
  
  (princ)
)

(defun insvPline (surfacevXY serviceColour)
(if(= vdebug 1)(and(princ "\n{Entered insvPline}\n")))
  (command "_pline")
  (foreach Pt surfacevXY (command Pt))
  (command "")
  (command "chprop" "_last" "" "c" serviceColour "")
  (command "chprop" "_last" "" "la" "plot" "")
)
(defun insvInvertPline (invertvXY surfacevXY nVertI / ivXY)
(if(= vdebug 1)(and(princ "\n{Entered insvInvertPline}\n")))

  (setq ivXY (cccq invertvXY surfacevXY nVertI))
  (if(= vdebug 1)(and(princ "\n{Exited cccq}\n")))

  (command "_pline")
  (foreach Pt ivXY (command Pt))
  (command "")
  (command "chprop" "_last" "" "c" "253" "")
  (command "chprop" "_last" "" "lt" "DASHED" "")
  (command "chprop" "_last" "" "s" "5" "")
  (command "chprop" "_last" "" "la" "plot" "")
  (princ)
  ivXY
)
(defun cccq (invertvXY surfacevXY nVertI / vX1 vY1 i j k varquad newivXYlist Pt1 Pt2)
(if(= vdebug 1)(and(princ "\n{Entered cccq}\n")))
  (setq newivXYlist '())
  (setq i 0)
  (setq j 0.5)
  (while (< i nVertI)
    (setq varquad '())
    (setq k (rem i 2))
	(if (= k 0)
	  (setq LvR 0.5)
	  (setq LvR (- 0 0.5))
	)
    (setq vX1 (+ (car (nth (fix j) surfacevXY)) LvR))
	(setq vY1 (- (cadr (nth i invertvXY)) 4))
	(setq varquad (consappend vX1 varquad))
	(setq varquad (consappend vY1 varquad))
	(setq newivXYlist (consappend varquad newivXYlist))
	
	  (setq Pt1 '())
	  (setq Pt1 (consappend (- (car (nth (fix j) surfacevXY)) 0.5) Pt1))
	  (setq Pt1 (consappend (cadr (nth (fix j) surfacevXY)) Pt1))
	  (setq Pt2 '())
	  (setq Pt2 (consappend (+ (car (nth (fix j) surfacevXY)) 0.5) Pt2))
	  (setq Pt2 (consappend (cadr (nth i newivXYlist)) Pt2))
	  
	  (command "_rectang" Pt1 Pt2)
      (command "chprop" "_last" "" "c" "5" "")
      (command "chprop" "_last" "" "la" "plot" "")
      (princ)
	
    (setq i (+ i 1))
    (setq j (+ j 0.5))
  )
  newivXYlist
)

(defun insvMH (Pt vDwgunits /)
(if(= vdebug 1)(and(princ "\n{Entered insvMH}\n")))
  (if (= vDwgunits 1)
    (command "_insert" "N:/Staff Directories/Zak/Blocks/AutoPlot/plotManhole.dwg" Pt "2" "2" "")
  )
  (if (= vDwgunits 2)
    (command "_insert" "N:/Staff Directories/Zak/Blocks/AutoPlot/plotManhole.dwg" Pt "2000" "2000" "")
  )
  (princ)
)
(defun insvIP (Pt vDwgunits /)
(if(= vdebug 1)(and(princ "\n{Entered insvIP}\n")))
  (if (= vDwgunits 1)
    (command "_insert" "N:/Staff Directories/Zak/Blocks/AutoPlot/plotIP.dwg" Pt "1" "1" "")
  )
  (if (= vDwgunits 2)
    (command "_insert" "N:/Staff Directories/Zak/Blocks/AutoPlot/plotIP.dwg" Pt "1000" "1000" "")
  )
  (princ)
)
(defun insvVerticals (nVertS placevX placevY invertvZ surfacevZ Voffset vOffsetMode chainvListS serviceColour vDwgunits / 0list vint XYlist i n1 n2 n3 n4 nv5 n5 n6 vhzn vScale)
(if(= vdebug 1)(and(princ "\n{Entered insvVerticals}\n")))
  (setq vint 0)
  (setq 0list 0)
  (setq 0list (makevList vint nVertS))
  (setq XYlist '())
  (setq XYlist (makevXY 0list nVertS placevX placevY chainvListS))

  (setq nv5 '())
  (setq nv5 (consappend " " nv5))
  (setq i 0)
  (while (< i (- nVertS 1))
    (setq nv5 (consappend (strcat (rtos (nth i gradevList) 2 2) "%   ") nv5))
    (setq i (+ i 1))
  )
  
  (setvar "ATTREQ" 1)
  (setq i 0)
  (while (< i nVertS)
    (setq n2 (rtos (- (nth i surfacevZ) Voffset) 2 2))
    (setq n3 (strcat (rtos (nth i chainvListS) 2 2) "m"))
    (if (= serviceColour "1")
	  (and
        (setq n4 (strcat "  SSMH: " (rtos (+ i 1) 2 0)))
		(if (> (+ i 2) nVertS)
		  (and (setq n1 (rtos (- (nth i invertvZ) Voffset) 2 2))(setq n6 ""))
	      (if (= i 0)
		    (and (setq n1 "")(setq n6 (rtos (- (nth i invertvZ) Voffset) 2 2)))
		    (and (setq n1 (rtos (- (nth i invertvZ) Voffset) 2 2))(setq n6 (rtos (- (nth (+ i 1) invertvZ) Voffset) 2 2)))
		  )
		)
	  )
	  (and
	    (setq n1 "-")
	    (setq n4 "")
	    (if (or (= serviceColour "2") (= serviceColour "3") (= serviceColour "6") (= serviceColour "7"))
          (setq n4 "  VTP")
		)
	    (if (= serviceColour "4")
          (setq n4 (strcat "  STRUCTURE: " (rtos (+ i 1) 2 0)))
		)
	    (if (= serviceColour "5")
		  (and
            (setq n4 (strcat "  SWMH: " (rtos (+ i 1) 2 0)))
	        (setq n1 (rtos (- (nth i invertvZ) Voffset) 2 2))
	        (setq n6 (rtos (- (nth (+ i 1) invertvZ) Voffset) 2 2))
		  )
			  
		)
	  )
    )
    (setq n5 (nth i nv5))
	
	(if (= vDwgunits 1)
	  (setq vScale 1)
	  (if (= vDwgunits 2)
	    (setq vScale 1000)
		(setq vScale 1)
	  )
	)
	(if (= vOffsetMode 0)
      (command "_insert" "N:/Staff Directories/Zak/Blocks/AutoPlot/plotVertical_N.dwg" (nth i XYlist) vScale vScale "0" n1 n2 n3 n4 n5 n6)
    )
	(if (= vOffsetMode 1)
      (command "_insert" "N:/Staff Directories/Zak/Blocks/AutoPlot/plotVertical_135.dwg" (nth i XYlist) vScale vScale "0" n1 n2 n3 n4 n5 n6)
    )
	(if (= vOffsetMode 2)
      (command "_insert" "N:/Staff Directories/Zak/Blocks/AutoPlot/plotVertical_165.dwg" (nth i XYlist) vScale vScale "0" n1 n2 n3 n4 n5 n6)
    )
	(if (= vOffsetMode 3)
      (command "_insert" "N:/Staff Directories/Zak/Blocks/AutoPlot/plotVertical_185.dwg" (nth i XYlist) vScale vScale "0" n1 n2 n3 n4 n5 n6)
    )
    (setq i (+ i 1))
  )
  (setq i 0)
  (while (< i 4)
    (setq vhzn (* i 16))
    (insvHorizontal vhzn nVertS placevX placevY chainvListS)
    (setq i (+ i 1))
  )
)
(defun insvInvertVerticals (invertvXY nVertI placevY Voffset vOffsetMode chainvListI vDwgunits / XYlist i)
(if(= vdebug 1)(and(princ "\n{Entered insvInvertVerticals}\n")))
  (setq i 0)
  (setq LvR 0)
  (while (< i nVertI)
    (setq k (rem i 2))
	(if (= k 0)
	  (setq LvR 1)
	  (setq LvR (- 0 1))
	)  
  
    (setq XYlist '())
	(setq Pt '())
	(setq Pt (consappend (car (nth i invertvXY)) Pt))
	(setq Pt (consappend (cadr (nth i invertvXY)) Pt))
	(setq XYlist (consappend Pt XYlist))
	(setq Pt '())
	(setq Pt (consappend (car (nth i invertvXY)) Pt))
	(setq Pt (consappend (+ placevY 73) Pt))
	(setq XYlist (consappend Pt XYlist))
	(setq Pt '())
	(setq Pt (consappend (car (nth i invertvXY)) Pt))
	(setq Pt (consappend (+ placevY 73) Pt))
	(setq XYlist (consappend Pt XYlist))
	(setq Pt '())
	(setq Pt (consappend (+ (car (nth i invertvXY)) LvR) Pt))
	(setq Pt (consappend (+ placevY 70) Pt))
	(setq XYlist (consappend Pt XYlist))
	(setq Pt '())
	(setq Pt (consappend (+ (car (nth i invertvXY)) LvR) Pt))
	(setq Pt (consappend (+ placevY 70) Pt))
	(setq XYlist (consappend Pt XYlist))
	(setq Pt '())
	(setq Pt (consappend (+ (car (nth i invertvXY)) LvR) Pt))
	(setq Pt (consappend (+ placevY 62) Pt))
	(setq XYlist (consappend Pt XYlist))
    (command "_pline")
    (foreach Pt XYlist (command Pt))
    (command "")
    (command "chprop" "_last" "" "c" "252" "")
    (command "chprop" "_last" "" "la" "plot" "")
    (princ)
	
	(setq XYlist '())
	(setq Pt '())
	(setq Pt (consappend (+ (car (nth i invertvXY)) LvR) Pt))
	(setq Pt (consappend (+ placevY 55) Pt))
	(setq XYlist (consappend Pt XYlist))
	(setq Pt '())
	(setq Pt (consappend (+ (car (nth i invertvXY)) LvR) Pt))
	(setq Pt (consappend (+ placevY 46) Pt))
	(setq XYlist (consappend Pt XYlist))
    (command "_pline")
    (foreach Pt XYlist (command Pt))
    (command "")
    (command "chprop" "_last" "" "c" "252" "")
    (command "chprop" "_last" "" "la" "plot" "")
    (princ)
	
	;|(setq Pt '())
	(setq Pt (consappend (car (nth i invertvXY)) Pt))
	(setq Pt (consappend (cadr (nth i invertvXY)) Pt))
	(if (= vDwgunits 1)
      (command "_insert" "N:/Staff Directories/Zak/Blocks/AutoPlot/plotIP2.dwg" Pt "1" "1" "")
    )
    (if (= vDwgunits 2)
      (command "_insert" "N:/Staff Directories/Zak/Blocks/AutoPlot/plotIP2.dwg" Pt "1000" "1000" "")
    )|;
	
	(setq Pt '())
	(setq Pt (consappend (+ (car (nth i invertvXY)) LvR) Pt))
	(setq Pt (consappend (+ placevY 46) Pt))
	(if (= vDwgunits 1)
      (command "_insert" "N:/Staff Directories/Zak/Blocks/AutoPlot/plotArrowheadD.dwg" Pt "1" "1" "")
    )
    (if (= vDwgunits 2)
      (command "_insert" "N:/Staff Directories/Zak/Blocks/AutoPlot/plotArrowheadD.dwg" Pt "1000" "1000" "")
    )
	
    (setq i (+ i 1))
  )
)
(defun insvHorizontal (vhzn nVertS placevX placevY chainvListS / 0list vint XYlist)
(if(= vdebug 1)(and(princ "\n{Entered insvHorizontal}\n")))
  (setq 0list 0)
  (setq 0list (makevList vhzn nVertS))
  (setq XYlist '())
  (setq XYlist (makevXY 0list nVertS placevX placevY chainvListS))
  (command "_pline")
  (foreach Pt XYlist (command Pt))
  (command "")
  (command "chprop" "_last" "" "c" "7" "")
  (command "chprop" "_last" "" "la" "plot" "")
  (princ)
)
(defun insvTitle (placevX placevY Voffset serviceColour vDwgunits / Pt n1 n2 n3)
(if(= vdebug 1)(and(princ "\n{Entered insvTitle}\n")))
  (setq Pt '())
  (setq Pt (consappend placevX Pt))
  (setq Pt (consappend placevY Pt))
  (setvar "ATTREQ" 1)
  (setq n1 (- 0 (- Voffset 48)));datum
  (setq n2 " Invert Level")
  (setq n3 " Finished Level")
  (if (= serviceColour "4");water
    (setq n2 " -"
	      n3 " Structure Level")
  )
  (if (= serviceColour "7");roading
    (and
      (setq n2 " -")
	  (setq n3 " Surface Level")
	)
  )
  (if (= vDwgunits 1)
    (command "_insert" "N:/Staff Directories/Zak/Blocks/AutoPlot/plotTitle.dwg" Pt "1" "1" "0" n1 n2 n3)
  )
  (if (= vDwgunits 2)
    (command "_insert" "N:/Staff Directories/Zak/Blocks/AutoPlot/plotTitle.dwg" Pt "1000" "1000" "0" n1 n2 n3)
  )
  (princ)
)
(defun insvGradelines (nVertS placevX placevY chainvListS vDwgunits / 0list vint XYlist i Pt)
(if(= vdebug 1)(and(princ "\n{Entered insvGradelines}\n")))
  (setq vint 0)
  (setq 0list (makevList vint nVertS))
  (setq XYlist '())
  (setq XYlist (makevXY 0list nVertS placevX placevY chainvListS))
  (setq i 1)
  (while (< i nVertS)
    (if (= vDwgunits 1)
      (command "_insert" "N:/Staff Directories/Zak/Blocks/AutoPlot/plotArrowheadR.dwg" (nth i XYlist) "1" "1" "")
    )
    (if (= vDwgunits 2)
      (command "_insert" "N:/Staff Directories/Zak/Blocks/AutoPlot/plotArrowheadR.dwg" (nth i XYlist) "1000" "1000" "")
    )
    (princ)
    (setq i (+ i 1))
  )
  (setq i 0)
  (while (< i (- nVertS 1))
    (if (= vDwgunits 1)
      (command "_insert" "N:/Staff Directories/Zak/Blocks/AutoPlot/plotArrowheadL.dwg" (nth i XYlist) "1" "1" "")
    )
    (if (= vDwgunits 2)
      (command "_insert" "N:/Staff Directories/Zak/Blocks/AutoPlot/plotArrowheadL.dwg" (nth i XYlist) "1000" "1000" "")
    )
    (princ)
    (setq i (+ i 1))
  )
  (setq vint 58)
  (setq 0list (makevList vint nVertS))
  (setq XYlist '())
  (setq XYlist (makevXY 0list nVertS placevX placevY chainvListS))
    (command "_pline")
    (foreach Pt XYlist (command Pt))
    (command "")
    (command "chprop" "_last" "" "c" "4" "")
    (command "chprop" "_last" "" "la" "plot" "")
    (princ)
)

(defun refreshCurves()
  (command "MOVE" (ssget "A") "" "D" "")
)

(prompt "\nEnter 3tp2 to plot section.\n")