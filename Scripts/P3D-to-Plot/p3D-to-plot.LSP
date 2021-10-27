;;;Plot a Longsectional graph from a single, or pair, of 3D polylines.
;;;Program by Zak Manley, 2018
;;;Comissioned by Simpson Shaw Serveyors Ltd.
;;;Revision 1.0.0

;;;;;Global Variables Key;;;;;
;|
surfacePolyName      ;The entity name of the surface polyline
invertPolyName       ;The entity name of the invert polyline
surfacevZ            ;List of Z coordinates from the surface polyline
surfacevXY           ;List of XY coordinates from the surface polyline
invertvZ             ;List of Z coordinates from the invert polyline
invertvXY            ;List of XY coordinates from the invert polyline
nVert                ;The number of vertices in the surface polyline
placeV               ;The coordinate of the selected placement of the plot
placevX              ;The X coordinate of the selected placement of the plot
placevY              ;The Y coordinate of the selected placement of the plot
Voffset              ;The constant vertical offset for drawing the plot line above the title/data block
serviceColour        ;The colour of the plot line (indicates service)
MHON                 ;The input string for drawing manholes ON/OFF
gradevList           ;The list of gradients between points
echoOld              ;The stored "cmdecho" value for restoring once drawing has finished
|;

;====================================================================================================================================================================================;
;====================================================================================MAIN PROGRAM====================================================================================;
;====================================================================================================================================================================================;

(defun c:3tp (/ osnapOld surfacePolyName invertPolyName surfacevZ surfacevXY invertvZ invertvXY nVert placeV placevX placevY Voffset serviceColour MHON gradevList chainvList vDwgunits)

(setq osnapOld (getvar "OSMODE"))

(setq vDwgunits 0)
(while (and (/= vDwgunits 1) (/= vDwgunits 2))
  (setq vDwgunits (getstring "Drawing Units? [Millimetres(1)/Metres(2)]: "))
  (setq vDwgunits (atoi vDwgunits))
  (if (and (/= vDwgunits 1) (/= vDwgunits 2))
    (princ "\nInvalid scale, please re-enter.\n")
  )
)

;|
  ;;;--- Put up the dialog box
  (setq dcl_id (load_dialog "N:/Staff Directories/Zak/LISP Files/p3D.dcl"))
 
  ;;;--- See if it is already loaded
  (if (not (new_dialog "p3D" dcl_id))
    (progn
      (alert "The p3D.DCL file could not be found!")
      (exit)
    )
  )
  ;;;--- Disable the number of sides edit box
  (mode_tile "IP" 0)
  (mode_tile "MH" 1)
  (mode_tile "ST" 1)

  ;;;--- If an action event occurs, do this function
  (action_tile "RD" "(toggleNum)")
  (action_tile "SW" "(toggleNum)")
  (action_tile "SS" "(toggleNum)")
  (action_tile "WR" "(toggleNum)")
  (action_tile "cancel" "(setq ddiag 1)(done_dialog)")
  (action_tile "accept" "(setq ddiag 2)(saveVars)(done_dialog)") 

  ;;;--- Display the dialog box
  (start_dialog)
|;

;;Get Service Colour from user input.
  ;(setq serviceColour 1)
  (setq serviceColour (getstring "Service Colour [1-7]: "))

;;Get Surface 3D Polyline from user input.
  (setq surfacePolyName (entsel "Select Surface 3D Polyline: "))
  (princ "Selected Surface\n")

;;Get Invert 3D Polyline from user input.
  (if (or (= serviceColour "1") (= serviceColour "5"))
    (and
      (setq invertPolyName (entsel "Select Invert 3D Polyline: "))
      (princ "Selected Invert\n")
    )
	(princ "\nNo Invert\n")
  )

;;Get input on option to draw Manholes from user input.
  (if (or (= serviceColour "1") (= serviceColour "5"))
    (setq MHON (getstring "Draw Manholes? [Yes/No]: "))
	(setq MHON 0)
  )
  
;;Get location of plot from user input.
  (setq placeV '())
  (setq placeV (getpoint "Select where to place the plot: "))
  (setq placevX (car placeV))
  (setq placevY (cadr placeV))
  (princ "Selected\n")
  
(setvar "OSMODE" 0)
  
  (setq Voffset 90)
  
  (setq nVert (getVertexCount (car surfacePolyName)))                                                   ;get the number of vertexes in the polyline
  ;(setq nVert 1)
  (setq surfacevZ (getvZ (car surfacePolyName) nVert Voffset))                                          ;get the Z coordinate of each vertex
  (if (or (= serviceColour "1") (= serviceColour "5"))
    (setq invertvZ (getvZ (car invertPolyName) nVert Voffset))                                            ;get the Z coordinate of each vertex of invert line
  )
  (setq chainvList (getvDist (car surfacePolyName) nVert))                                              ;get the relative chainage of each vertex

  (princ "\nChainvList: ")
  (setq i 0)
  (while (< i nVert)
   (print (nth i chainvList))
   (setq i (+ i 1))
  )

  (princ "\nNumber of Vertexes: ")(princ nVert)
  (princ "\nSurface Z Points: ");(princ surfacevZ)
  (setq i 0)
  (while (< i nVert)
   (print (- (nth i surfacevZ) Voffset))
   (setq i (+ i 1))
  )
  
  (if (or (= serviceColour "1") (= serviceColour "5"))
    (and
      (princ "\nInvert Z Points: ");(princ surfacevZ)
      (setq i 0)
      (while (< i nVert)
       (print (- (nth i invertvZ) Voffset))
       (setq i (+ i 1))
      )
	)
  )
  
  (setq surfacevXY (makevXY surfacevZ nVert placevX placevY chainvList))                                ;create list of x,y coordinates
  (princ "\nSurface XY Points: \n")
  (setq i 0)
  (while (< i nVert)
   (princ (- (car (nth i surfacevXY)) placevX))
   (princ ",")
   (princ (- (cadr (nth i surfacevXY)) placevY))
   (princ "\n")
   (setq i (+ i 1))
  )
  
  (if (or (= serviceColour "1") (= serviceColour "5"))
    (and
      (setq invertvXY (makevXY invertvZ nVert placevX placevY chainvList))                                  ;create list of x,y coordinates from invert line
      (princ "\nInvert XY Points: ")(princ invertvXY)
	)
  )

  (if (or (= serviceColour "1") (= serviceColour "5"))
    (setq gvXY invertvXY)
	(setq gvXY surfacevXY)
  )  
  (setq gradevList (getvGrade gvXY nVert))                                                                ;get the gradient between each vertex pair (vertex & vertex+1)
  (princ "\nGradients: ")(princ gradevList)
  (princ "\n")
  
  (drawvPlot surfacevXY invertvXY nVert placevX placevY Voffset surfacevZ invertvZ serviceColour MHON gradevList chainvList vDwgunits) ;draw plot
  
(setvar "OSMODE" osnapOld)
  
  (princ "\nDone, chur.")
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

(defun getVertexCount (vEnt / nEnt n assoc0)                                                            ;routine to get the number of vertexes in the polyline
  (setq n 0)
  (setq assoc0 (cdr (assoc 0 (entget vEnt))))
  (setq	nEnt (entnext vEnt))
  (while (/= assoc0 "SEQEND")
    (setq n (+ n 1))
    (setq nEnt (entnext nEnt))
    (setq assoc0 (cdr (assoc 0 (entget nEnt))))
  )
  n
)
  
(defun getvZ (vEnt nVert Voffset / i Z Zlist newEnt)                                                    ;routine to get the Z coordinate of each vertex
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

(defun getvGrade (invertvXY nVert / i vX1 vY1 vX2 vY2 vM vP vGrade)                                     ;routine to get and calculate the gradients between each vertex
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
  vGrade
)

(defun getvDist (surfacePolyName nVert / newEnt newitem1 distXYList i string1 distvList)                ;routine to get the distance between each vertex
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
  distvList
)

;====================================================================================================================================================================================;
;================================================================================CALCULATION ROUTINES================================================================================;
;====================================================================================================================================================================================;

(defun consappend (itemt listt)                                                                         ;routine to append a list with a variable
  (setq listt (reverse listt))
  (setq listt (cons itemt listt))
  (setq listt (reverse listt))
)

(defun vround (n / vunrounded vrounded)                                                                 ;routine to round integers to 2dp (doesn't work)
  (setq vrounded 0.00)
  (setq vrounded (/ (fix (* n 100)) 100.00))
  vrounded
)

(defun makevList (vint nVert / 0list i)                                                                 ;routine to make a list of nVert length out of an integer
  (setq 0list '())
  (setq i 0)
  (while (< i nVert)
    (setq 0list (consappend vint 0list))
    (setq i (+ i 1))
  )
  0list
)

(defun makevXY (0list nVert placevX placevY chainvList / i tV XV XYlist runningvChain)                  ;routine to convert surfacevZ (list of Z coords) to list of X,Y coords
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
  XYlist
)

(defun calcvDist (distXYList nVert / X1 Y1 X2 Y2 i string1 dist1 distancesList1)                        ;routine to calculate the distance between all vertexes
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
  distancesList1
)

;====================================================================================================================================================================================;
;========================================================================================DRAW========================================================================================;
;====================================================================================================================================================================================;

(defun drawvPlot (surfacevXY invertvXY nVert placevX placevY Voffset surfacevZ invertvZ serviceColour MHON gradevList chainvList vDwgunits) ;routine to draw pline plot

  (setq echoOld (getvar "cmdecho"))                                                                     ;turn off echo
  (setvar "cmdecho" 0)
  
  (insvPline surfacevXY serviceColour)                                                                  ;draw surface polyline
  (princ "\nDrawn Surface.")
  
  (if (or (= serviceColour "1") (= serviceColour "5"))
    (and
      (insvInvertPline invertvXY)                                                                       ;draw invert polyline
      (princ "\nDrawn Invert.")
	)
  )
  
  (if (or (eq MHON "y") (eq MHON "Y"))                                                                  ;draw manholes
    (and
      (foreach Pt surfacevXY (insvMH Pt vDwgunits))
	  (princ "\nDrawn Manholes.")
	)
	(princ "\nDrawn No Manholes.")
  )
  (if (= serviceColour "7")                                                                               ;draw IPs
    (and
	  (foreach Pt surfacevXY (insvIP Pt vDwgunits))
	  (princ "\nDrawn IPs.")
	)
	(princ "\nDrawn No IPs.")
  )
  
  (insvVerticals nVert placevX placevY invertvZ surfacevZ Voffset chainvList serviceColour vDwgunits)    ;draw verticals and horizontals
  (princ "\nDrawn Verticals.")
  
  (insvTitle placevX placevY Voffset serviceColour vDwgunits)                                                     ;draw title block
  (princ "\nDrawn Title Block.")
  
  (insvGradelines nVert placevX placevY chainvList vDwgunits)                                                       ;draw gradient lines
  (princ "\nDrawn Grade Lines.")
  
  ;(insvGradetext placevX placevY nVert gradevList chainvList)                                             ;draw gradient text
  ;(princ "\nDrawn Gradient Text.")

  (setvar "cmdecho" echoOld)   ;restore echo
  
  (princ)
)

(defun insvPline (surfacevXY serviceColour)
  (command "_pline")
  (foreach Pt surfacevXY (command Pt))
  (command "")
  (command "chprop")(command "_last")(command "")(command "c")(command serviceColour)(command "")
  (command "chprop")(command "_last")(command "")(command "la")(command "plot")(command "")
)
(defun insvInvertPline (invertvXY)
  (command "_pline")
  (foreach Pti invertvXY (command Pti))
  (command "")
  (command "chprop")(command "_last")(command "")(command "c")(command "253")(command "")
  (command "chprop")(command "_last")(command "")(command "lt")(command "DASHED")(command "")
  (command "chprop")(command "_last")(command "")(command "s")(command "5")(command "")
  (command "chprop")(command "_last")(command "")(command "la")(command "plot")(command "")
)

(defun insvMH (Pt vDwgunits /)
  (if (= vDwgunits 1)
    (command "_insert" "N:/Staff Directories/Zak/Blocks/AutoPlot/plotManhole.dwg" Pt "2" "2" "")
  )
  (if (= vDwgunits 2)
    (command "_insert" "N:/Staff Directories/Zak/Blocks/AutoPlot/plotManhole.dwg" Pt "2000" "2000" "")
  )
  ;(command "_insert" "C:/Users/Zak/Desktop/Blocks/plotManhole.dwg" Pt "2000" "2000" "")
  (princ)
)
(defun insvIP (Pt vDwgunits /)
  (if (= vDwgunits 1)
    (command "_insert" "N:/Staff Directories/Zak/Blocks/AutoPlot/plotIP.dwg" Pt "1" "1" "")
  )
  (if (= vDwgunits 2)
    (command "_insert" "N:/Staff Directories/Zak/Blocks/AutoPlot/plotIP.dwg" Pt "1000" "1000" "")
  )
  ;(command "_insert" "C:/Users/Zak/Desktop/Blocks/plotManhole.dwg" Pt "2000" "2000" "")
  (princ)
)
(defun insvVerticals (nVert placevX placevY invertvZ surfacevZ Voffset chainvList serviceColour vDwgunits / 0list vint XYlist i n1 n2 n3 n4 nv5 n5 vhzn)
  (setq vint 0)
  (setq 0list 0)
  (setq 0list (makevList vint nVert))
  (setq XYlist '())
  (setq XYlist (makevXY 0list nVert placevX placevY chainvList))

  (setq nv5 '())
  (setq nv5 (consappend " " nv5))
  (setq i 0)
  (while (< i (- nVert 1))
    (setq nv5 (consappend (strcat (rtos (nth i gradevList) 2 2) "%  ") nv5))
    (setq i (+ i 1))
  )
  
  (setvar "ATTREQ" 1)
  (setq i 0)
  (while (< i nVert)
    (setq n2 (rtos (- (nth i surfacevZ) Voffset) 2 2))
	(setq ni3 0)
	(setq ni3 (+ ni3 (nth i chainvList)))
    (setq n3 (strcat (rtos (ni3) 2 2) "m"))
    (if (= serviceColour "1")
	  (and
        (setq n4 (strcat "  SSMH: __" (rtos (+ i 1) 2 0)))
	    (setq n1 (rtos (- (nth i invertvZ) Voffset) 2 2))
	  )
	  (and
	  (setq n1 "-")
	    (if (or (= serviceColour "2") (= serviceColour "3") (= serviceColour "6") (= serviceColour "7"))
          (setq n4 "  IP")
	      (if (= serviceColour "4")
            (setq n4 (strcat "  STRUCTURE _" (rtos (+ i 1) 2 0)))
	    	(if (= serviceColour "5")
			  (and
                (setq n4 (strcat "  SWMH: __" (rtos (+ i 1) 2 0)))
	            (setq n1 (rtos (- (nth i invertvZ) Voffset) 2 2))
			  )
			  (setq n4 "")
			)
	      )
		)
      )
    )
    (setq n5 (nth i nv5))
	(if (= vDwgunits 1)
      (command "_insert" "N:/Staff Directories/Zak/Blocks/AutoPlot/plotVertical.dwg" (nth i XYlist) "1" "1" "0" n1 n2 n3 n4 n5)
    )
    (if (= vDwgunits 2)
      (command "_insert" "N:/Staff Directories/Zak/Blocks/AutoPlot/plotVertical.dwg" (nth i XYlist) "1000" "1000" "0" n1 n2 n3 n4 n5)
    )
    (setq i (+ i 1))
  )
  (setq i 0)
  (while (< i 4)
    (setq vhzn (* i 16))
    (insvHorizontal vhzn nVert placevX placevY chainvList)
    (setq i (+ i 1))
  )
)
(defun insvHorizontal (vhzn nVert placevX placevY chainvList / 0list vint XYlist)
  (setq 0list 0)
  (setq 0list (makevList vhzn nVert))
  (setq XYlist '())
  (setq XYlist (makevXY 0list nVert placevX placevY chainvList))
  (command "_pline")
  (foreach Pt XYlist (command Pt))
  (command "")
  (command "chprop")(command "_last")(command "")(command "c")(command "7")(command "")
  (command "chprop")(command "_last")(command "")(command "la")(command "plot")(command "")
  (princ)
)
(defun insvTitle (placevX placevY Voffset serviceColour vDwgunits / Pt n1 n2 n3)
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
    (setq n2 " -"
	      n3 " Surface Level")
  )
  (if (= vDwgunits 1)
    (command "_insert" "N:/Staff Directories/Zak/Blocks/AutoPlot/plotTitle.dwg" Pt "1" "1" "0" n1 n2 n3)
  )
  (if (= vDwgunits 2)
    (command "_insert" "N:/Staff Directories/Zak/Blocks/AutoPlot/plotTitle.dwg" Pt "1000" "1000" "0" n1 n2 n3)
  )
  ;(command "_insert" "C:/Users/Zak/Desktop/Blocks/plotTitle.dwg" Pt "1000" "1000" "0" n1 n2 n3)
  (princ)
)
(defun insvGradelines (nVert placevX placevY chainvList vDwgunits / 0list vint XYlist i Pt)
  (setq vint 0)
  (setq 0list (makevList vint nVert))
  (setq XYlist '())
  (setq XYlist (makevXY 0list nVert placevX placevY chainvList))
  (setq i 1)
  (while (< i nVert)
    (if (= vDwgunits 1)
      (command "_insert" "N:/Staff Directories/Zak/Blocks/AutoPlot/plotArrowheadR.dwg" (nth i XYlist) "1" "1" "")
    )
    (if (= vDwgunits 2)
      (command "_insert" "N:/Staff Directories/Zak/Blocks/AutoPlot/plotArrowheadR.dwg" (nth i XYlist) "1000" "1000" "")
    )
    ;(command "_insert" "C:/Users/Zak/Desktop/Blocks/plotGrade.dwg" Pt "1000" "1000" "")
    (princ)
    (setq i (+ i 1))
  )
  (setq i 0)
  (while (< i (- nVert 1))
    (if (= vDwgunits 1)
      (command "_insert" "N:/Staff Directories/Zak/Blocks/AutoPlot/plotArrowheadL.dwg" (nth i XYlist) "1" "1" "")
    )
    (if (= vDwgunits 2)
      (command "_insert" "N:/Staff Directories/Zak/Blocks/AutoPlot/plotArrowheadL.dwg" (nth i XYlist) "1000" "1000" "")
    )
    ;(command "_insert" "C:/Users/Zak/Desktop/Blocks/plotGrade.dwg" Pt "1000" "1000" "")
    (princ)
    (setq i (+ i 1))
  )
  (setq vint 58)
  (setq 0list (makevList vint nVert))
  (setq XYlist '())
  (setq XYlist (makevXY 0list nVert placevX placevY chainvList))
    (command "_pline")
    (foreach Pt XYlist (command Pt))
    (command "")
    (command "chprop")(command "_last")(command "")(command "c")(command "4")(command "")
    (command "chprop")(command "_last")(command "")(command "la")(command "plot")(command "")
    (princ)
)
(defun insvGradetext (placevX placevY nVert gradevList chainvList / PtL PtR i n)
  (setq i 0)
  (while (< i (- nVert 1))

    (setq PtL '())
    (setq PtL (consappend (+ (+ placevX (* (nth i chainvList) i)) 20) PtL))
    (setq PtL (consappend (+ placevY 65) PtL))
    (setq PtR '())
    (setq PtR (consappend (+ (+ placevX (* (nth i chainvList) i)) 25) PtR))
    (setq PtR (consappend (+ placevY 60) PtR))
  
    (setq n (strcat (rtos (nth i gradevList) 2 2) "%"))
  
    (command
    "_.MTEXT"
    PtL 
    "_Justify" "MC"		
    "_Height" 2.5
    "_Rotation" 0
	PtR
    n
    ""				    ; end of text
    )
    (command "chprop")(command "_last")(command "")(command "la")(command "plot")(command "")
	(setq i (+ i 1))
  )
)

(prompt "\nEnter 3tp to plot section.\n")