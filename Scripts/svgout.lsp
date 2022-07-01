(defun c:svgout (/ 3Zentset 3Zent nVert SVGscale LargestPt SmallestPt sorfill scap si i t)
(princ "\n")

;|
  (setq 3Zentset (ssget))
  (setq si (sslength 3Zentset))
  
  (setq i 0)
  (while (< i si)
    (setq 3Zent (ssname 3Zentset i))
    (setq i (+ i 1))
  )
|;

  (setq 3Zent (entsel "\nSelect polyline: "))
  (while (NULL 3Zent)
	(princ "No object selected\n")
	(setq 3Zent (entsel "Select polyline: "))
  )
  (while (/= (cdr(assoc 0 (entget(car 3Zent)))) "LWPOLYLINE")
	(princ "Object must be a polyline\n")
	(setq 3Zent (entsel "Select polyline: "))
  )
  (princ "Selected polyline\n")
  
  (setq t "")
  (setq scap "")
  (setq nVert (cdr(assoc 90 (entget(car 3Zent)))))
  (if (= nVert 2)
    (progn
      (setq t (getstring "Selected polyline is only a single stroke, would you like to add a linecap? "))
	  (if (or (= t "y") (= t "yes") (= t "1") (= t "c"))
        (setq scap " stroke-linecap=\"round\"")
	    (setq scap "")
	  )
	)
  )
  
  (setq LargestPt (getSVGlargest 3Zent))
  (setq SmallestPt (getSVGsmallest 3Zent))
  
  (setq SVGscale 100)
  
  (setq sorfill 0)
  (setq sorfill (getstring "\nStroke or Fill [S/F]: "))
  (while (or (/= sorfill "s") (/= sorfill "f"))
    (princ "Invalid input\n")
    (setq sorfill (getstring "Stroke or Fill [S/F]: "))
  )
  
  (setq SVGverts (getSVGverts 3Zent))
  
  (TXOUT SVGverts SVGscale LargestPt SmallestPt sorfill scap)
  
  (princ "\nDone.")
)

(defun getSVGlargest (3Zent nVert / i XL1 YL1 XYL1)
  (setq ci (3ZgetPreamble 3Zent))
  (setq XL1 (nth 1 (nth ci (entget(car 3Zent)))))
  (setq YL1 (nth 2 (nth ci (entget(car 3Zent)))))
  
  (setq i 0)
  (while (< i (* nVert 4))
    (if (> (nth 1 (nth (+ ci i) (entget(car 3Zent)))) XL1)
      (setq XL1 (nth 1 (nth (+ ci i) (entget(car 3Zent)))))
	)
	(if (> (nth 2 (nth (+ ci i) (entget(car 3Zent)))) YL1)
	  (setq YL1 (nth 2 (nth (+ ci i) (entget(car 3Zent)))))
	)
    (setq i (+ i 4))
  )
  (setq XYL1 '())
  (setq XYL1 (consappend XL1 XYL1))
  (setq XYL1 (consappend YL1 XYL1))
  XYL1
)

(defun getSVGsmallest (3Zent nVert / i XS1 YS1 XYS1)
  (setq ci (3ZgetPreamble 3Zent))
  (setq XS1 (nth 1 (nth ci (entget(car 3Zent)))))
  (setq YS1 (nth 2 (nth ci (entget(car 3Zent)))))
  
  (setq i 0)
  (while (< i (* nVert 4))
    (if (< (nth 1 (nth (+ ci i) (entget(car 3Zent)))) XS1)
      (setq XS1 (nth 1 (nth (+ ci i) (entget(car 3Zent)))))
	)
	(if (< (nth 2 (nth (+ ci i) (entget(car 3Zent)))) YS1)
	  (setq YS1 (nth 2 (nth (+ ci i) (entget(car 3Zent)))))
	)
    (setq i (+ i 4))
  )
  (setq XYS1 '())
  (setq XYS1 (consappend XS1 XYS1))
  (setq XYS1 (consappend YS1 XYS1))
  XYS1
)

(defun getSVGverts (3Zent nVert / ci i SVGverts)
  (setq ci (3ZgetPreamble 3Zent))
  (setq i 0)
  (setq SVGverts (strcat "M" (rtos(nth 1 (nth (+ ci i) (entget(car 3Zent)))) 2 2) " " (rtos(nth 2 (nth (+ ci i) (entget(car 3Zent)))) 2 2) " "))
  (setq i 4)
  (while (< i (* nVert 4))
    (setq SVGverts (strcat SVGverts "L" (rtos(nth 1 (nth (+ ci i) (entget(car 3Zent)))) 2 2) " "))
	(setq SVGverts (strcat SVGverts (rtos(nth 2 (nth (+ ci i) (entget(car 3Zent)))) 2 2) " "))
    (setq i (+ i 4))
  )
  (setq SVGverts (strcat SVGverts "Z"))
  SVGverts
)

(defun TXOUT (SVGverts SVGscale LargestPt SmallestPt sorfill scap / elist en fn fname i ss txtline1 txtline2 txtline3 txtline4 sorfilltxt)
  (setvar "cmdecho" 0)
  (princ "\nSVG file saves to directory of current drawing\n")
  ;(if (setq ss (ssget (list (cons 0 "TEXT"))))
    ;(progn 
	  (setq fname (getstring "\nEnter file name: "))
      (if (= fname "")
        (setq fname (substr (getvar "dwgname") 1 (- (strlen (getvar "dwgname")) 4)))
	  )
      (setq fn (open (strcat (getvar "dwgprefix") fname ".svg") "w"))
      (setq i -1)
	  
	  (if (= sorfill "s")
	    (setq sorfilltxt " fill=\"none\" stroke-width=\"2\"")
		(setq sorfilltxt "")
	  )
	  
	  (setq txtline1 (strcat "<svg xmlns=\"" "http://www.w3.org/2000/svg\"" 
	                         " width=\"" 
							 (rtos SVGscale 2 2) 
							 "\" height=\"" 
							 (rtos SVGscale 2 2) 
							 "\" viewBox=\"" 
							 (rtos (- (nth 0 SmallestPt) 1) 2 0) 
							 " "
							 (rtos (- (nth 1 SmallestPt) 1) 2 0) 
							 " "
							 (rtos (+ (nth 0 LargestPt) 1) 2 0) 
							 " "
							 (rtos (+ (nth 1 LargestPt) 1) 2 0) 
							 "\">")
	  )
	  (setq txtline3 (strcat "<path" sorfilltxt scap " d=\"" SVGverts "\"/>"))
	  (setq txtline4 "</svg>")
	  
	  (write-line txtline1 fn)
	  ;(write-line txtline2 fn)
	  (write-line txtline3 fn)
	  (write-line txtline4 fn)
	  
	  ;|
      (repeat (sslength ss)
        (setq i (1+ i))
        (setq en (ssname ss i)) 
		(setq elist (entget en))
        (setq txt (cdr (assoc 1 elist)))
        (write-line txt fn)
	  )
	  |;
      (close fn)
	;)
  ;)
  (princ (strcat "\n* SVG file " (getvar "dwgprefix") fname " has been created *"))
  (setvar "cmdecho" 1)
  (setq fn (strcat (getvar "dwgprefix") fname ".svg"))
  ;(startapp (strcat "Notepad " (chr 34) fn (chr 34)))
  (startapp (strcat "Chrome " (chr 34) fn (chr 34)))
  (princ)
)
