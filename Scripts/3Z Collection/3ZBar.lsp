;;;Program by Zak Manley, 2018
;;;Comissioned by Simpson Shaw Serveyors Ltd.

(defun c:3Zbar (/ osnapOld echoOld 3ZPolyName ci newbscale oldbs)
(setq osnapOld (getvar "OSMODE"))                                                                        ;get OSnap
(setq echoOld (getvar "cmdecho"))                                                                        ;turn off echo
(setvar "cmdecho" 0)
(command "_.UNDO" "BEGIN")
(princ "\n")
  
  (setq newbscale (atof (getstring "Enter viewport scale factor: ")))
  
(setvar "OSMODE" 0)
  
  ;(command "ERASE" (THselectb) "")
  
  (if (ssget "x" (list(cons 0 "INSERT")(cons 2 "BarScale")))
    (progn
	  (setq oldbs (ssget "x" (list(cons 0 "INSERT")(cons 2 "BarScale"))))
      (command "ERASE" oldbs "")
	)
  )
  
  ;|(setq bscaleexist (THselectb))
  (if (= bscaleexist 0)
    (command "ERASE" "")
  )|;
  ;|(if (or (= bscaleexist 4) (= bscaleexist 1))
	(progn
      (princ "\nSelection Error. Exiting...\n")
      (setvar "OSMODE" osnapOld)
      (command "_.UNDO" "END")
	  (setvar "cmdecho" echoOld)
      (princ "\n")
      (princ)
      (exit)
	)
  )|;
  
  (insvBar newbscale)
  
(setvar "OSMODE" osnapOld)
(command "_.UNDO" "END")
(setvar "cmdecho" echoOld)
(princ "\n")
  (princ "\nDone.")
  (princ)  
)

(defun insvBar (newbscale / Pt n1 n2 n3 n4 n5)
  (setq Pt '())
  (setq Pt (consappend 343 Pt))
  (setq Pt (consappend 3 Pt))
  (setvar "ATTREQ" 1)
  (setq n1 (strcat (rtos (/ 100 newbscale) 2 2) " "))
  (setq n2 (strcat (rtos (* (/ (/ 100 newbscale) 4) 3) 2 2) " "))
  (setq n3 (strcat (rtos (/ (/ 100 newbscale) 2) 2 2) " "))
  (setq n4 (strcat (rtos (/ (/ 100 newbscale) 4) 2 2) " "))
  (setq n5 "0 ")
  
  (command "_insert" "N:/Staff Directories/Zak/Blocks/3Z/BarScale.dwg" Pt "1" "0" n1 n2 n3 n4 n5)
  (command "chprop" "_last" "" "la" "GE-Plan Text" "")
  
  (princ)
)

(defun THselectb (/ blkname found objs ss i blk bname boutstate)
  (vl-load-com)
  (setq boutstate 4)
  ;;; Tharwat 03. March. 2012 ;;;
  (if (and (setq blkname "BarScale")
           (setq found (tblsearch "BLOCK" blkname))
           (setq objs (ssadd)
                 ss   (ssget "_x" '((0 . "INSERT")))
           )
      )
    (progn
	  (setq boutstate 0)
      (repeat
        (setq i (sslength ss))
         (setq bname (vla-get-effectivename
                      (vlax-ename->vla-object
                        (setq blk (ssname ss (setq i (1- i))))
                      )
                    )
         )
         (if (eq (strcase blkname) (strcase bname))
           (ssadd blk objs)
         )
      )
      (if objs
        (sssetfirst nil objs)
      )
    )
    (cond ((not blkname)
           (setq boutstate 1) ;Missed name of block ***
          )
          ((not found)
           (setq boutstate 2) ;Block not found in drawing !!!
          )
          (t
           (setq boutstate 3) ;Couldn't find any block !!!
          )
    )

  )
  boutstate
)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  