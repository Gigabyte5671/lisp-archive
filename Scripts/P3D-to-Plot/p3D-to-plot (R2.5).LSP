;;;Plot a Longsectional graph from a single, or pair, of 3D polylines.
;;;Program by Zak Manley, 2018
;;;Comissioned by Simpson Shaw Serveyors Ltd.
;;;Revision 2.5.0

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

(defun c:3tp25 (/ vdebug osnapOld echoOld surfacePolyName invertPolyName surfacevZ surfacevXY invertvZ invertvXY nVertS nVertI placeV placevX placevY Voffset vOffsetMode serviceColour MHON gradevList chainvListS chainvListI vDwgunits)

(princ "\n")

(setq vdebug 0)
(setq osnapOld (getvar "OSMODE"))                                                                        ;get OSnap

(setq echoOld (getvar "cmdecho"))                                                                        ;turn off echo
(setvar "cmdecho" 0)

(command "_.UNDO" "BEGIN")
(princ "\n")

(if (not(tblsearch "layer" "plot"))                                                                      ;check for plot layer
  (progn
    (command "_.layer" "New" "plot" "")
	(princ "\nPlot layer didn't exist, created\n")
  )
)

(command "_.style" "Standard" "Arial" "" "" "" "" "")                                                    ;set Standard text style to Arial
(if(= vdebug 1)(progn(princ "\n{Set Standard text style to Arial}")))


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
    (progn
	  (setq vdebug 1)
	  (princ "Verbose Logging On.\n")
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
  (setq invertPolyName (entsel "Select Invert 3D Polyline: "))
  (while (NULL invertPolyName)
	(princ "No Object\n")
    (setq surfacePolyName (entsel "Select Invert 3D Polyline: "))
  )
  (while (/= (cdr(assoc 0 (entget(car invertPolyName)))) "POLYLINE")
	(princ "Object must be a 3D Polyline\n")
	(setq invertPolyName (entsel "Select Invert 3D Polyline: "))
  )
  (princ "Selected Invert\n")

;;Get input on option to draw Manholes from user input.
  ;(setq MHON (getstring "\nDraw Manholes? [Yes/No]: "))
  (setq MHON 0)
  
;;Get location of plot from user input.
  (setq placeV '())
  (setq placeV (getpoint "Select where to place the plot: "))
  (setq placevX (car placeV))
  (setq placevY (cadr placeV))
  (princ "Selected\n")
  
  ;(setq vDwgunits 0)
  ;(setq vDwgunits (getvDwgScale placeV))                                                                 ;get the scale of the drawing
  
(setvar "OSMODE" 0)                                                                                      ;turn off OSnap
(if(= vdebug 1)(progn(princ "\n{Set OSMODE to 0}")))
  
  (setq nVertS (getVertexCount (car surfacePolyName)))                                                   ;get the number of vertexes in the surface polyline
  (if(= vdebug 1)(progn(princ "\n{Got nVertS}")))
  (setq nVertI (getVertexCount (car invertPolyName)))                                                    ;get the number of vertexes in the invert polyline
  (if(= vdebug 1)(progn(princ "\n{Got nVertI}")))
  
  (setq vdebugSuffix '())
  (setq vdebugSuffix (consappend "st" vdebugSuffix))
  (setq vdebugSuffix (consappend "nd" vdebugSuffix))
  (setq vdebugSuffix (consappend "rd" vdebugSuffix))
  (setq i 0)
  (while (< i (+ nVertS 5))
    (setq vdebugSuffix (consappend "th" vdebugSuffix))
    (setq i (+ i 1))
  )
  ;(if(= vdebug 1)(progn(princ "\n{Debug Suffix: ")(print vdebugSuffix)(princ "}")))
  
  (setq nVflag 0)
  (if (= nVertI (- (* nVertS 2) 2))
	(setq nVflag 1)
	(3ZEXIT osnapOld echoOld 1 2)
  )
  
  (setq Voffset (getVoffset (car surfacePolyName) nVertS))                                               ;calculate the vertical offset with-which to draw the plot above the Title Block
  (if(= vdebug 1)(progn(princ "\n{Got Voffset}")))	
  
  (setq surfacevZ (getvZ (car surfacePolyName) nVertS Voffset))                                          ;get the Z coordinate of each vertex
  (if(= vdebug 1)(progn(princ "\n{Got surfacevZ}")))
  (setq invertvZ (getvZ (car invertPolyName) nVertI Voffset))                                            ;get the Z coordinate of each vertex of invert line
  (if(= vdebug 1)(progn(princ "\n{Got invertvZ}")))
  
  (setq chainvListS (getvDist (car surfacePolyName) nVertS))                                             ;get the relative chainage of each vertex
  (if(= vdebug 1)(progn(princ "\n{Got chainvListS}")))
  (setq chainvListI (getvDist (car invertPolyName) nVertI))                                              ;get the relative chainage of each vertex
  (if(= vdebug 1)(progn(princ "\n{Got chainvListI}")))
  
  
  (setq vOffsetMode 0)
  (setq somethingvVar (chkvLargest surfacevZ nVertS))
  (princ "\nsomethingvVar: ")
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
  (if(= vdebug 1)(progn(princ "\n{Set vOffsetMode}")))
  
  (setq surfacevXY (makevXY surfacevZ nVertS placevX placevY chainvListS))                               ;create list of surface x,y coordinates
  (if(= vdebug 1)(progn(princ "\n{Got surfacevXY}")))
  
  
  (setq invertvXY (makevXY invertvZ nVertI placevX placevY chainvListI))                                 ;create list of invert x,y coordinates
  (if(= vdebug 1)(progn(princ "\n{Got invertvXY}")))
   
  (progn (setq gvXY invertvXY) (setq gvN nVertI))                                                        ;set up gradient param
  ;(progn (setq gvXY surfacevXY) (setq gvN nVertS))
  
  (if(= vdebug 1)(progn(princ "\n{Set Gradient Param}")))
  (setq gradevList (getvGrade gvXY gvN))                                                                 ;get the gradient between each vertex pair (vertex & vertex+1)
  (if(= vdebug 1)(progn(princ "\n{Got gradevList}")))
  
  
  (if (= vdebug 1)
    (progn
  
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
	  
      (princ "\nInvert Z Points: ");(princ surfacevZ)
      (setq i 0)
      (while (< i nVertI)
        (print (- (nth i invertvZ) Voffset))
        (setq i (+ i 1))
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
	  
	  (princ "\nInvert XY Points: \n")
      (setq i 0)
      (while (< i nVertI)
       (princ (- (car (nth i invertvXY)) placevX))
       (princ ",")
       (princ (- (cadr (nth i invertvXY)) placevY))
       (princ "\n")
       (setq i (+ i 1))
      )
  
      (princ "\nGradients: ")(princ gradevList)
      (princ "\n")
    )
  )
  
  (drawvPlot surfacevXY invertvXY nVertS nVertI placevX placevY Voffset vOffsetMode surfacevZ invertvZ serviceColour MHON gradevList chainvListS chainvListI vDwgunits) ;draw plot
  (if(= vdebug 1)(progn(princ "\n{Completed All Draws}")))
  
  (refreshCurves)
  (if(= vdebug 1)(progn(princ "\n{Refreshed All Objects}")))
  
  (3ZEXIT osnapOld echoOld 1 0)
  (princ)
)

(defun 3ZEXIT (osnapOld echoOld undoOpt 3Zmsgnum / 3Zmsg)
  (princ "\n")
  (setq 3Zmsg 0)
  (if (= undoOpt 1)
    (command "_.UNDO" "END")
  )
  (setvar "OSMODE" osnapOld)   ;restore osnap setting
  (setvar "cmdecho" echoOld)   ;restore echo setting
  
  (if (= 3Zmsgnum 0);completed successfully
    (setq 3Zmsg "Done, chur.")
  )
  (if (= 3Zmsgnum 1);bad drawing scale
    (setq 3Zmsg "The drawing uses an odd (or imperial) scale, please use a drawing with a metric scale.")
  )
  (if (= 3Zmsgnum 2);missing points
    (setq 3Zmsg "Missing Vertices on Polyline.")
  )
  (if (= 3Zmsgnum 3)
    (setq 3Zmsg "Broke 3.")
  )
  
  (if (= 3Zmsg 0)
    (setq 3Zmsg "bork bork, all is bork")
  )
  
  (princ 3Zmsg)
  
(if(= vdebug 1)(progn(princ "\n{Macro exited with code: ")(princ 3Zmsgnum)(princ "}")))
  (princ "\n")
  (princ)
(if(= vdebug 1)(textscr))
  (exit)
)
;====================================================================================================================================================================================;
;===================================================================================DIALOG ROUTINES==================================================================================;
;====================================================================================================================================================================================;
;|
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
|;
;====================================================================================================================================================================================;
;====================================================================================GET ROUTINES====================================================================================;
;====================================================================================================================================================================================;
(defun getvDwgScale (placeV / measuredvLength figuredvUnits i)
(if(= vdebug 1)(progn(princ "\n{Entered getvDwgScale}")))

  (setq measuredvLength 0.000)
  (setq figuredvUnits 0)
  (setq i 0)

  (command "_.insert" "N:/Staff Directories/Zak/Blocks/AutoPlot/plotDwgScale.dwg" placeV "1" "1" "0")
(if(= vdebug 1)(progn(princ "\n{Inserted scale block}")))
  (command "_.EXPLODE" "_last")
(if(= vdebug 1)(progn(princ "\n{Exploded scale block}")))
  
  (setq measuredvLength (- (nth 1 (nth 18 (entget(entlast))))(nth 1 (nth 14 (entget(entlast))))))
  (princ "\nML: ")(princ measuredvLength)(princ "\n")
  
  (command "_.ERASE" "_last" "")
  
  (setq measuredvLength (* measuredvLength 1000))
  
  (princ "\nML2: ")(princ measuredvLength)
  
  (if (= measuredvLength 1000)
    (setq figuredvUnits 1)
	(setq i (+ i 1))
  )
  (if (= measuredvLength 1)
    (setq figuredvUnits 2)
	(setq i (+ i 1))
  )
  (print i)(princ "\nFU: ")(princ figuredvUnits)(princ "\n")
  (if (= i 2)
    (3ZEXIT osnapOld echoOld 1 1)
  )
  
  figuredvUnits
)


(defun getVertexCount (vEnt / nEnt n assoc0)                                                             ;routine to get the number of vertexes in the polyline
(if(= vdebug 1)(progn(princ "\n{Entered getVertexCount}")))
  (setq n 0)
  (setq assoc0 (cdr (assoc 0 (entget vEnt))))
  (setq	nEnt (entnext vEnt))
  (while (/= assoc0 "SEQEND")
    (setq n (+ n 1))
    (setq nEnt (entnext nEnt))
    (setq assoc0 (cdr (assoc 0 (entget nEnt))))
  )
(if(= vdebug 1)(progn(princ "\n{Exited getVertexCount}")))
  n
)

(defun getVoffset (surfacePolyName nVert / newEnt V vOff1)
(if(= vdebug 1)(progn(princ "\n{Entered getVoffset}")))
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
(if(= vdebug 1)(progn(princ "\n{Exited getVoffset}")))
  vOff1
)
  
(defun getvZ (vEnt nVert Voffset / i Z Zlist newEnt)                                                     ;routine to get the Z coordinate of each vertex
(if(= vdebug 1)(progn(princ "\n{Entered getvZ}")))
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
(if(= vdebug 1)(progn(princ "\n{Entered getvGrade}")))
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
(if(= vdebug 1)(progn(princ "\n{Exited getvGrade}")))
  vGrade
)

(defun getvDist (surfacePolyName nVert / newEnt newitem1 distXYList i string1 distvList)                 ;routine to get the distance between each vertex
(if(= vdebug 1)(progn(princ "\n{Entered getvDist}")))
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
(if(= vdebug 1)(progn(princ "\n{Exited getvDist}")))
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
(if(= vdebug 1)(progn(princ "\n{Entered makevList}")))
  (setq 0list '())
  (setq i 0)
  (while (< i nVert)
    (setq 0list (consappend vint 0list))
    (setq i (+ i 1))
  )
(if(= vdebug 1)(progn(princ "\n{Exited makevList}")))
  0list
)

(defun makevXY (0list nVert placevX placevY chainvList / i tV XV XYlist runningvChain)                   ;routine to convert surfacevZ (list of Z coords) to list of X,Y coords
(if(= vdebug 1)(progn(princ "\n{Entered makevXY}")))
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
(if(= vdebug 1)(progn(princ "\n{Exited makevXY}")))
  XYlist
)

(defun calcvDist (distXYList nVert / X1 Y1 X2 Y2 i string1 dist1 distancesList1)                         ;routine to calculate the distance between all vertexes
(if(= vdebug 1)(progn(princ "\n{Entered calcvDist}")))
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
(if(= vdebug 1)(progn(princ "\n{Exited calcvDist}")))
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
(if(= vdebug 1)(progn(princ "\n{Entered drawvPlot}")))
  
  (insvPline surfacevXY serviceColour)                                                                   ;draw surface polyline
  (princ "\nDrawn Surface.")
  
  (setq newivXY 0)
  (setq newivXY (insvInvertPline invertvXY surfacevXY nVertI))                                           ;draw invert polyline
  (princ "\nDrawn Invert.")
  
  (if (or (eq MHON "y") (eq MHON "Y"))                                                                   ;draw manholes
    (progn
      (foreach Pt surfacevXY (insvMH Pt vDwgunits))
	  (princ "\nDrawn Manholes.")
	)
  )
  
  (insvVerticals nVertS nVertI placevX placevY invertvZ surfacevZ Voffset vOffsetMode chainvListS gradevList serviceColour vDwgunits);draw verticals and horizontals
  (princ "\nDrawn Descenders.")
  (insvInvertVerticals newivXY nVertI placevY Voffset vOffsetMode chainvListI vDwgunits)                 ;draw verticals and horizontals
  (princ "\nDrawn Invert Descenders.")
  
  (insvTitle placevX placevY Voffset vDwgunits)                                                          ;draw title block
  (princ "\nDrawn Title Block.")
  
  (insvGradelines nVertS placevX placevY chainvListS vDwgunits)                                          ;draw gradient lines
  (princ "\nDrawn Grade Lines.")
  
  (insvchainText nVertS placevX placevY chainvListS)                                                     ;draw independent chainage text
  (princ "\nDrawn Chainage Text.")
  
  (princ)
)

(defun insvPline (surfacevXY serviceColour)
(if(= vdebug 1)(progn(princ "\n{Entered insvPline}")))
  (command "_pline")
  (foreach Pt surfacevXY (command Pt))
  (command "")
  (command "chprop" "_last" "" "c" serviceColour "")
  (command "chprop" "_last" "" "la" "plot" "")
)
(defun insvInvertPline (invertvXY surfacevXY nVertI / ivXY)
(if(= vdebug 1)(progn(princ "\n{Entered insvInvertPline}")))

  (setq ivXY (cccq invertvXY surfacevXY nVertI))
  (if(= vdebug 1)(progn(princ "\n{Exited cccq}")))

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
(if(= vdebug 1)(progn(princ "\n{Entered cccq}")))
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
(if(= vdebug 1)(progn(princ "\n{Entered insvMH}")))
  (if (= vDwgunits 1)
    (command "_insert" "N:/Staff Directories/Zak/Blocks/AutoPlot/plotManhole.dwg" Pt "2" "2" "")
  )
  (if (= vDwgunits 2)
    (command "_insert" "N:/Staff Directories/Zak/Blocks/AutoPlot/plotManhole.dwg" Pt "2000" "2000" "")
  )
  (princ)
)
(defun insvIP (Pt vDwgunits /)
(if(= vdebug 1)(progn(princ "\n{Entered insvIP}")))
  (if (= vDwgunits 1)
    (command "_insert" "N:/Staff Directories/Zak/Blocks/AutoPlot/plotIP.dwg" Pt "1" "1" "")
  )
  (if (= vDwgunits 2)
    (command "_insert" "N:/Staff Directories/Zak/Blocks/AutoPlot/plotIP.dwg" Pt "1000" "1000" "")
  )
  (princ)
)
(defun insvVerticals (nVertS nVertI placevX placevY invertvZ surfacevZ Voffset vOffsetMode chainvListS gradevList serviceColour vDwgunits / 0list vint XYlist i n1 n2 nv3 n3 n4 nv5 n5 n6 vhzn vScale ninvertvZ)
(if(= vdebug 1)(progn(princ "\n{Entered insvVerticals}")))
  (setq vint 0)
  (setq 0list 0)
  (setq 0list (makevList vint nVertS))
  (setq XYlist '())
  (setq XYlist (makevXY 0list nVertS placevX placevY chainvListS))

  (setq nv5 '())
  (setq nv5 (consappend " " nv5))
  
  (setq ninvertvZ '())
  (setq ninvertvZ (consappend 0 ninvertvZ))
  (setq i 0)
  (while (< i nVertI)
    (setq ninvertvZ (consappend (nth i invertvZ) ninvertvZ))
	(setq i (+ i 1))
  )
  
  (setq i 0)
  (while (< i (- nVertS 1))
    (if (< (nth (+ i 1) chainvListS) 12)
      (setq nv5 (consappend (strcat (rtos (nth (+ i i) gradevList) 2 2) "%") nv5))
      (if (< (nth (+ i 1) chainvListS) 15)
        (setq nv5 (consappend (strcat (rtos (nth (+ i i) gradevList) 2 2) "% ") nv5))
	    (setq nv5 (consappend (strcat (rtos (nth (+ i i) gradevList) 2 2) "%   ") nv5))
	  )
	)
    (setq i (+ i 1))
  )
  
  (setvar "ATTREQ" 1)
  (setq nv3 0)
  (setq i 0)
  (while (< i nVertS)
    (setq n2 (rtos (- (nth i surfacevZ) Voffset) 2 2))
	
    (setq nv3 (+ nv3 (nth i chainvListS)))
	(setq n3 (strcat (rtos nv3 2 2) "m"))
	
    (if (= serviceColour "1")
	  (progn
        (setq n4 (strcat "  SSMH: " (rtos (+ i 1) 2 0)))
		(if (> (+ i 2) nVertS)
		  (progn (setq n1 (rtos (- (nth i ninvertvZ) Voffset) 2 2))(setq n6 ""))
	      (if (= i 0)
		    (progn (setq n1 "")(setq n6 (rtos (- (nth (+ i 1) ninvertvZ) Voffset) 2 2)))
		    (progn (setq n1 (rtos (- (nth (+ i i) ninvertvZ) Voffset) 2 2))(setq n6 (rtos (- (nth (+ (+ i i) 1) ninvertvZ) Voffset) 2 2)))
		  )
		)
	  )
	  (progn
	    (setq n1 "-")
	    (setq n4 "")
	    (if (or (= serviceColour "2") (= serviceColour "3") (= serviceColour "4") (= serviceColour "6") (= serviceColour "7"))
          (setq n4 (strcat "  STRUCTURE: " (rtos (+ i 1) 2 0)))
		)
	    (if (= serviceColour "5")
		  (progn
            (setq n4 (strcat "  SWMH: " (rtos (+ i 1) 2 0)))
		    (if (> (+ i 2) nVertS)
		      (progn (setq n1 (rtos (- (nth i ninvertvZ) Voffset) 2 2))(setq n6 ""))
	          (if (= i 0)
		        (progn (setq n1 "")(setq n6 (rtos (- (nth (+ i 1) ninvertvZ) Voffset) 2 2)))
		        (progn (setq n1 (rtos (- (nth (+ i i) ninvertvZ) Voffset) 2 2))(setq n6 (rtos (- (nth (+ (+ i i) 1) ninvertvZ) Voffset) 2 2)))
		      )
		    )
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
	(command "chprop" "_last" "" "la" "plot" "")
	
	(if(= vdebug 1)(progn(princ "\n{Drawn ")(princ (+ i 1))(princ (nth i vdebugSuffix))(princ " vertical}")))
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
(if(= vdebug 1)(progn(princ "\n{Entered insvInvertVerticals}")))
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
    )
	(command "chprop" "_last" "" "la" "plot" "")
	|;
	
	(setq Pt '())
	(setq Pt (consappend (+ (car (nth i invertvXY)) LvR) Pt))
	(setq Pt (consappend (+ placevY 46) Pt))
	(if (= vDwgunits 1)
      (command "_insert" "N:/Staff Directories/Zak/Blocks/AutoPlot/plotArrowheadD.dwg" Pt "1" "1" "")
    )
    (if (= vDwgunits 2)
      (command "_insert" "N:/Staff Directories/Zak/Blocks/AutoPlot/plotArrowheadD.dwg" Pt "1000" "1000" "")
    )
	(command "chprop" "_last" "" "la" "plot" "")
	
    (setq i (+ i 1))
  )
)
(defun insvHorizontal (vhzn nVertS placevX placevY chainvListS / 0list vint XYlist)
(if(= vdebug 1)(progn(princ "\n{Entered insvHorizontal}")))
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
(defun insvTitle (placevX placevY Voffset vDwgunits / Pt n1 n2 n3)
(if(= vdebug 1)(progn(princ "\n{Entered insvTitle}")))
  (setq Pt '())
  (setq Pt (consappend placevX Pt))
  (setq Pt (consappend placevY Pt))
  (setvar "ATTREQ" 1)
  (setq n1 (- 0 (- Voffset 48)));datum
  (setq n2 " Invert Level")
  (setq n3 " Finished Level")
  (if (= vDwgunits 1)
    (command "_insert" "N:/Staff Directories/Zak/Blocks/AutoPlot/plotTitle.dwg" Pt "1" "1" "0" n1 n2 n3)
  )
  (if (= vDwgunits 2)
    (command "_insert" "N:/Staff Directories/Zak/Blocks/AutoPlot/plotTitle.dwg" Pt "1000" "1000" "0" n1 n2 n3)
  )
  (command "chprop" "_last" "" "la" "plot" "")
  
  (princ)
)
(defun insvGradelines (nVertS placevX placevY chainvListS vDwgunits / 0list vint XYlist i Pt)
(if(= vdebug 1)(progn(princ "\n{Entered insvGradelines}")))
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
	(command "chprop" "_last" "" "la" "plot" "")
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
	(command "chprop" "_last" "" "la" "plot" "")
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
(defun insvchainText (nVertS placevX placevY chainvListS / vint 0list vxy1 vxy2 n1 i chainvL cvL)
(if(= vdebug 1)(progn(princ "\n{Entered insvchainText}")))

  (setq chainvL '())
  (setq cvL 0)
  (setq i 0)
  (while (< i nVertS)
    (setq cvL (+ cvL (nth i chainvListS)))
	(setq chainvL (consappend cvL chainvL))
	(setq i (+ i 1))
  )

  (setq vxy2 '())
  (setq i 0)
  (while (< i (- nVertS 1))
    (setq vxy1 '())
    (setq vxy1 (consappend (- (+ (/ (- (nth (+ i 1) chainvL) (nth i chainvL)) 2) (nth i chainvL) placevX) 1.5) vxy1))
    (setq vxy1 (consappend (+ 2 placevY) vxy1))
    (setq vxy2 (consappend vxy1 vxy2))
	(setq i (+ i 1))
  )
  
  ;|
  (setq vint 4)
  (setq 0list 0)
  (setq 0list (makevList vint nVertS))
  (setq vxy1 '())
  (setq vxy1 (makevXY 0list nVertS placevX placevY chainvListS))
  
  (setq vint 0)
  (setq 0list 0)
  (setq 0list (makevList vint (- nVertS 1)))
  (setq vxy2 '())
  (setq chainvL '())
  (setq i 1)
  (while (< i nVertS)
    (setq chainvL (consappend (nth i chainvListS) chainvL))
	(setq i (+ i 1))
  )
  (setq vxy2 (makevXY 0list (- nVertS 1) placevX placevY chainvL))
  |;
  
  (setq n1 0)
  (setq i 0)
  (while (< i (- nVertS 1))
    (setq n1 (rtos (nth (+ i 1) chainvListS) 2 2))
    ;(command "_.MTEXT" (nth i vxy1) "H" "2.5" "J" "MC" (nth i vxy2) n1 "")
	(if (> (atoi n1) 12)
	  (progn
	    (if (= vDwgunits 1)
          (command "_insert" "N:/Staff Directories/Zak/Blocks/AutoPlot/plotCHText.dwg" (nth i vxy2) "1" "1" "" n1)
        )
        (if (= vDwgunits 2)
          (command "_insert" "N:/Staff Directories/Zak/Blocks/AutoPlot/plotCHText.dwg" (nth i vxy2) "1000" "1000" "" n1)
        )
	    (command "chprop" "_last" "" "la" "plot" "")
	  )
	)
	(setq i (+ i 1))
  )
)

(defun refreshCurves()
  (command "MOVE" (ssget "A") "" "D" "")
)

(prompt "\nEnter 3tp25 to plot section.\n")