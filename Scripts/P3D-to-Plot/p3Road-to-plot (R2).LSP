;;;Plot a Longsectional graph from a single 3D polyline.
;;;Program by Zak Manley, 2018
;;;Comissioned by Simpson Shaw Serveyors Ltd.
;;;Revision 2.0.0

;;;;;Global Variables Key;;;;;
;|
surfacePolyName      ;The entity name of the surface polyline
surfacevZ            ;List of Z coordinates from the surface polyline
surfacevXY           ;List of XY coordinates from the surface polyline
nVert                ;The number of vertices in the generic polyline
nVertS               ;The number of vertices in the surface polyline
placeV               ;The coordinate of the selected placement of the plot
placevX              ;The X coordinate of the selected placement of the plot
placevY              ;The Y coordinate of the selected placement of the plot
Voffset              ;The constant vertical offset for drawing the plot line above the title/data block
serviceColour        ;The colour of the plot line (indicates service)
gradevList           ;The list of gradients between points
echoOld              ;The stored "cmdecho" value for restoring once drawing has finished
osnapOld             ;The stored "osnap" value for restoring once drawing has finished
|;

;====================================================================================================================================================================================;
;====================================================================================MAIN PROGRAM====================================================================================;
;====================================================================================================================================================================================;

(defun c:3rp2 (/ vdebug osnapOld echoOld surfacePolyName surfacevZ surfacevXY nVertS placeV placevX placevY Voffset vOffsetMode serviceColour gradevList chainvListS vDwgunits)

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
;|
;;Get Service Colour from user input.
  (setq serviceColour (getstring "Service Colour [1-7]: "))
  (if (= serviceColour "-0")
    (and
	  (setq vdebug 1)
	  (princ "\nVerbose Logging On.\n")
	  (setq serviceColour (getstring "Service Colour [1-7]: "))
	)
  )
|;
(setq serviceColour "7")

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
  
;;Get location of plot from user input.
  (setq placeV '())
  (setq placeV (getpoint "Select where to place the plot: "))
  (setq placevX (car placeV))
  (setq placevY (cadr placeV))
  (princ "Selected\n")
  
(setvar "OSMODE" 0)                                                                                      ;turn off OSnap
(if(= vdebug 1)(and(princ "\n{Set OSMODE to 0}\n")))
  
  (setq nVertS (RgetVertexCount (car surfacePolyName)))                                                   ;get the number of vertexes in the surface polyline
  (if(= vdebug 1)(and(princ "\n{Got nVertS}\n")))
  
  (setq Voffset (RgetVoffset (car surfacePolyName) nVertS))                                               ;calculate the vertical offset with-which to draw the plot above the Title Block
  (if(= vdebug 1)(and(princ "\n{Got Voffset}\n")))	
  
  (setq surfacevZ (RgetvZ (car surfacePolyName) nVertS Voffset))                                          ;get the Z coordinate of each vertex
  (if(= vdebug 1)(and(princ "\n{Got surfacevZ}\n")))
  
  (setq chainvListS (RgetvDist (car surfacePolyName) nVertS))                                             ;get the relative chainage of each vertex
  (if(= vdebug 1)(and(princ "\n{Got chainvListS}\n")))
  
  (setq vOffsetMode 0)
  (setq somethingvVar (RchkvLargest surfacevZ nVertS))
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
  (if(= vdebug 1)(and(princ "\n{Set vOffsetMode}\n")))
  
  (setq surfacevXY (RmakevXY surfacevZ nVertS placevX placevY chainvListS))                               ;create list of surface x,y coordinates
  (if(= vdebug 1)(and(princ "\n{Got surfacevXY}\n")))
  
  (setq gvXY surfacevXY)
  (setq gvN nVertS)
  (if(= vdebug 1)(and(princ "\n{Set Gradient Param}\n")))
  
  (setq gradevList (RgetvGrade gvXY gvN))                                                                 ;get the gradient between each vertex pair (vertex & vertex+1)
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
  
  (RdrawvPlot surfacevXY nVertS placevX placevY Voffset vOffsetMode surfacevZ serviceColour gradevList chainvListS vDwgunits) ;draw plot
  (if(= vdebug 1)(and(princ "\n{Completed All Draws}\n")))
  
  (command "_.UNDO" "END")
  (princ "\n")
  
  (RrefreshCurves)
  (if(= vdebug 1)(and(princ "\n{Refreshed All Objects}\n")))
  
  ;(if(= vdebug 1)(and(princ "\n{Completed ")(princ (getvar "OSMODE"))(princ " Consappends}\n")))
(setvar "OSMODE" osnapOld)
(setvar "cmdecho" echoOld)                                                                               ;restore echo
  
  (princ "\nDone, chur.")
  (if(= vdebug 1)(textscr))
  (princ)
)

;====================================================================================================================================================================================;
;====================================================================================GET ROUTINES====================================================================================;
;====================================================================================================================================================================================;

(defun RgetVertexCount (vEnt / nEnt n assoc0)                                                             ;routine to get the number of vertexes in the polyline
(if(= vdebug 1)(and(princ "\n{Entered RgetVertexCount}\n")))
  (setq n 0)
  (setq assoc0 (cdr (assoc 0 (entget vEnt))))
  (setq	nEnt (entnext vEnt))
  (while (/= assoc0 "SEQEND")
    (setq n (+ n 1))
    (setq nEnt (entnext nEnt))
    (setq assoc0 (cdr (assoc 0 (entget nEnt))))
  )
(if(= vdebug 1)(and(princ "\n{Exited RgetVertexCount}\n")))
  n
)

(defun RgetVoffset (surfacePolyName nVert / newEnt V vOff1)
(if(= vdebug 1)(and(princ "\n{Entered RgetVoffset}\n")))
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
(if(= vdebug 1)(and(princ "\n{Exited RgetVoffset}\n")))
  vOff1
)
  
(defun RgetvZ (vEnt nVert Voffset / i Z Zlist newEnt)                                                     ;routine to get the Z coordinate of each vertex
(if(= vdebug 1)(and(princ "\n{Entered RgetvZ}\n")))
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

(defun RgetvGrade (ivXY nVert / i vX1 vY1 vX2 vY2 vM vP vGrade)                                      ;routine to get and calculate the gradients between each vertex
(if(= vdebug 1)(and(princ "\n{Entered RgetvGrade}\n")))
  (setq vGrade '())
  (setq vM 0)
  (setq vP 0)
  (setq i 0)
  (while (< i (- nVert 1))
    (setq vX1 (car (nth i ivXY)))
    (setq vY1 (cadr (nth i ivXY)))
    (setq vX2 (car (nth (+ i 1) ivXY)))
    (setq vY2 (cadr (nth (+ i 1) ivXY)))
    (setq vM (/ (- vY2 vY1) (- vX2 vX1)))
	(setq vP (* vM 100))
    (setq vGrade (consappend vP vGrade))
    (setq i (+ i 1))
  )
(if(= vdebug 1)(and(princ "\n{Exited RgetvGrade}\n")))
  vGrade
)

(defun RgetvDist (surfacePolyName nVert / newEnt newitem1 distXYList i string1 distvList)                 ;routine to get the distance between each vertex
(if(= vdebug 1)(and(princ "\n{Entered RgetvDist}\n")))
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
  (setq distvList (RcalcvDist distXYList nVert))
(if(= vdebug 1)(and(princ "\n{Exited RgetvDist}\n")))
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

(defun RmakevList (vint nVert / 0list i)                                                                  ;routine to make a list of nVertS length out of an integer
(if(= vdebug 1)(and(princ "\n{Entered RmakevList}\n")))
  (setq 0list '())
  (setq i 0)
  (while (< i nVert)
    (setq 0list (consappend vint 0list))
    (setq i (+ i 1))
  )
(if(= vdebug 1)(and(princ "\n{Exited RmakevList}\n")))
  0list
)

(defun RmakevXY (0list nVert placevX placevY chainvList / i tV XV XYlist runningvChain)                   ;routine to convert surfacevZ (list of Z coords) to list of X,Y coords
(if(= vdebug 1)(and(princ "\n{Entered RmakevXY}\n")))
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
(if(= vdebug 1)(and(princ "\n{Exited RmakevXY}\n")))
  XYlist
)

(defun RcalcvDist (distXYList nVert / X1 Y1 X2 Y2 i string1 dist1 distancesList1)                         ;routine to calculate the distance between all vertexes
(if(= vdebug 1)(and(princ "\n{Entered RcalcvDist}\n")))
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
(if(= vdebug 1)(and(princ "\n{Exited RcalcvDist}\n")))
  distancesList1
)

(defun RchkvLargest (surfacevZ nVertS / i vLargest)                                                       ;routine to find the largest value in a list of nVertS length
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

(defun RdrawvPlot (surfacevXY nVertS placevX placevY Voffset vOffsetMode surfacevZ serviceColour gradevList chainvListS vDwgunits / newivXY)
(if(= vdebug 1)(and(princ "\n{Entered RdrawvPlot}\n")))
  
  (RinsvPline surfacevXY serviceColour)                                                                   ;draw surface polyline
  (princ "\nDrawn Surface.")
  
  (foreach Pt surfacevXY (RinsvIP Pt vDwgunits))
  
  (RinsvVerticals nVertS placevX placevY surfacevZ Voffset vOffsetMode chainvListS serviceColour vDwgunits gradevList);draw verticals and horizontals
  (princ "\nDrawn Descenders.")
  
  (RinsvTitle placevX placevY Voffset serviceColour vDwgunits)                                            ;draw title block
  (princ "\nDrawn Title Block.")
  
  (RinsvGradelines nVertS placevX placevY chainvListS vDwgunits)                                          ;draw gradient lines
  (princ "\nDrawn Grade Lines.")
  
  (insvchainText nVertS placevX placevY chainvListS)                                                      ;draw independent chainage text
  (princ "\nDrawn Chainage Text.")
  
  (princ)
)

(defun RinsvPline (surfacevXY serviceColour)
(if(= vdebug 1)(and(princ "\n{Entered RinsvPline}\n")))
  (command "_pline")
  (foreach Pt surfacevXY (command Pt))
  (command "")
  (command "chprop" "_last" "" "c" serviceColour "")
  (command "chprop" "_last" "" "la" "plot" "")
)
(defun RinsvIP (Pt vDwgunits /)
(if(= vdebug 1)(and(princ "\n{Entered RinsvIP}\n")))
  (if (= vDwgunits 1)
    (command "_insert" "N:/Staff Directories/Zak/Blocks/AutoPlot/plotIP.dwg" Pt "1" "1" "")
  )
  (if (= vDwgunits 2)
    (command "_insert" "N:/Staff Directories/Zak/Blocks/AutoPlot/plotIP.dwg" Pt "1000" "1000" "")
  )
  (princ)
)
(defun RinsvVerticals (nVertS placevX placevY surfacevZ Voffset vOffsetMode chainvListS serviceColour vDwgunits gradevList / 0list vint XYlist i n1 n2 n3 n4 nv5 n5 n6 vhzn vScale)
(if(= vdebug 1)(and(princ "\n{Entered RinsvVerticals}\n")))
  (setq vint 0)
  (setq 0list 0)
  (setq 0list (RmakevList vint nVertS))
  (setq XYlist '())
  (setq XYlist (RmakevXY 0list nVertS placevX placevY chainvListS))

  (setq nv5 '())
  (setq nv5 (consappend " " nv5))
  (setq i 0)
  (while (< i (- nVertS 1))
    (setq nv5 (consappend (strcat (rtos (nth i gradevList) 2 2) "%") nv5))
    (setq i (+ i 1))
  )
  
  (setvar "ATTREQ" 1)
  (setq i 0)
  (while (< i nVertS)
    (setq n2 (rtos (- (nth i surfacevZ) Voffset) 2 2))
    (setq n3 (strcat (rtos (nth i chainvListS) 2 2) "m"))
	(setq n1 " ")
	(setq n4 " ")
    (setq n5 (nth i nv5))
	(setq n6 " ")
	
	(if (= vDwgunits 1)
	  (setq vScale 1)
	  (if (= vDwgunits 2)
	    (setq vScale 1000)
		(setq vScale 1)
	  )
	)
	(if (= vOffsetMode 0)
      (command "_insert" "N:/Staff Directories/Zak/Blocks/AutoPlot/plotVerticalR_N.dwg" (nth i XYlist) vScale vScale "0" n1 n2 n3 n4 n5 n6)
    )
	(if (= vOffsetMode 1)
      (command "_insert" "N:/Staff Directories/Zak/Blocks/AutoPlot/plotVerticalR_135.dwg" (nth i XYlist) vScale vScale "0" n1 n2 n3 n4 n5 n6)
    )
	(if (= vOffsetMode 2)
      (command "_insert" "N:/Staff Directories/Zak/Blocks/AutoPlot/plotVerticalR_165.dwg" (nth i XYlist) vScale vScale "0" n1 n2 n3 n4 n5 n6)
    )
	(if (= vOffsetMode 3)
      (command "_insert" "N:/Staff Directories/Zak/Blocks/AutoPlot/plotVerticalR_185.dwg" (nth i XYlist) vScale vScale "0" n1 n2 n3 n4 n5 n6)
    )
	(command "chprop" "_last" "" "la" "plot" "")
    (setq i (+ i 1))
  )
  (setq i 0)
  (while (< i 3)
    (setq vhzn (* i 16))
    (RinsvHorizontal vhzn nVertS placevX placevY chainvListS)
    (setq i (+ i 1))
  )
)
(defun RinsvHorizontal (vhzn nVertS placevX placevY chainvListS / 0list vint XYlist)
(if(= vdebug 1)(and(princ "\n{Entered RinsvHorizontal}\n")))
  (setq 0list 0)
  (setq 0list (RmakevList vhzn nVertS))
  (setq XYlist '())
  (setq XYlist (RmakevXY 0list nVertS placevX placevY chainvListS))
  (command "_pline")
  (foreach Pt XYlist (command Pt))
  (command "")
  (command "chprop" "_last" "" "c" "7" "")
  (command "chprop" "_last" "" "la" "plot" "")
  (princ)
)
(defun RinsvTitle (placevX placevY Voffset serviceColour vDwgunits / Pt n1 n2)
(if(= vdebug 1)(and(princ "\n{Entered RinsvTitle}\n")))
  (setq Pt '())
  (setq Pt (consappend placevX Pt))
  (setq Pt (consappend placevY Pt))
  (setvar "ATTREQ" 1)
  (setq n1 (- 0 (- Voffset 48)));datum
  (setq n2 " Finished Level")
  (if (= vDwgunits 1)
    (command "_insert" "N:/Staff Directories/Zak/Blocks/AutoPlot/plotTitle2.dwg" Pt "1" "1" "0" n1 n2)
  )
  (if (= vDwgunits 2)
    (command "_insert" "N:/Staff Directories/Zak/Blocks/AutoPlot/plotTitle2.dwg" Pt "1000" "1000" "0" n1 n2)
  )
  (command "chprop" "_last" "" "la" "plot" "")
  (princ)
)
(defun RinsvGradelines (nVertS placevX placevY chainvListS vDwgunits / 0list vint XYlist i Pt)
(if(= vdebug 1)(and(princ "\n{Entered RinsvGradelines}\n")))
  (setq vint (- 0 16))
  (setq 0list (RmakevList vint nVertS))
  (setq XYlist '())
  (setq XYlist (RmakevXY 0list nVertS placevX placevY chainvListS))
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
  (setq vint (- 58 16))
  (setq 0list (RmakevList vint nVertS))
  (setq XYlist '())
  (setq XYlist (RmakevXY 0list nVertS placevX placevY chainvListS))
    (command "_pline")
    (foreach Pt XYlist (command Pt))
    (command "")
    (command "chprop" "_last" "" "c" "4" "")
    (command "chprop" "_last" "" "la" "plot" "")
    (princ)
)
(defun insvchainText (nVertS placevX placevY chainvListS / vint 0list vxy1 vxy2 n1 i chainvL)
(if(= vdebug 1)(and(princ "\n{Entered insvchainText}\n")))
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
  
  (setq n1 0)
  (setq i 0)
  (while (< i (- nVertS 1))
    (setq n1 (rtos (nth (+ i 1) chainvListS) 2 2))
    (command "_.MTEXT" (nth i vxy1) "H" "2.5" "J" "MC" (nth i vxy2) n1 "")
	
	(setq i (+ i 1))
  )
)

(defun RrefreshCurves()
  (command "MOVE" (ssget "A") "" "D" "")
)

(prompt "\nEnter 3tp2 to plot section.\n")