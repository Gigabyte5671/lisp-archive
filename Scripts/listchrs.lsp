;;;Lists all special characters available in AutoCAD 2008
;;;Program by Zak Manley, 2018

(defun c:listchrs (/ i)
  (princ "\nStart")
  (setq i 1)
  (while (< i 397)
    (princ (strcat "\n" (rtos i) " => " (chr i)))
    (setq i (+ i 1))
  )
  (princ "\nEnd.")
  (print)
)