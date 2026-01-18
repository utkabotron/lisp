;;; Инструменты для работы с точками
;;; Команды: PTT, PTTT, TXT2PT

;;; PTT - Установка стиля точек (квадрат+круг+крест)
(defun c:PTT ( )

  ;; Set point display mode: square + circle + diagonal cross
  (setvar "PDMODE" 99)

  ;; Set point size in absolute units
  (setvar "PDSIZE" 0.3)

  (princ "\nPOINT style set: square + circle + X, size 0.3 (absolute).")
  (princ)
)

;;; PTTT - Установка стиля точек с выбором размера
(defun c:PTTT ( / acad doc psize )

  ;; Get active document
  (setq acad (vlax-get-acad-object))
  (setq doc (vla-get-ActiveDocument acad))

  ;; Ask user for point size
  (setq psize
        (getreal
          "\nEnter point size (absolute units, e.g. 0.3): "
        )
  )

  ;; Exit if user cancels or enters invalid value
  (if (or (null psize) (<= psize 0.0))
    (progn
      (princ "\nInvalid point size. Command canceled.")
      (princ)
      (exit)
    )
  )

  ;; Set point display mode
  (setvar "PDMODE" 99)

  ;; Set point size in absolute units
  (setvar "PDSIZE" psize)

  ;; Regenerate drawing
  (vla-Regen doc acAllViewports)

  (princ
    (strcat
      "\nPOINT style set: PDMODE 99, size "
      (rtos psize 2 4)
      " (absolute)."
    )
  )

  (princ)
)

;;; TXT2PT - Преобразование текста с отметками в 3D точки
(defun c:TXT2PT (/ ss i ent ed ins txt z)
  ;; Convert text elevation labels to 3D points
  ;; Text value becomes Z coordinate, point placed at text insertion
  
  (prompt "\nSelect text objects with elevation values: ")
  (setq ss (ssget '((0 . "TEXT"))))
  
  (if ss
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i)
              ed  (entget ent)
              ins (cdr (assoc 10 ed))  ; insertion point
              txt (cdr (assoc 1 ed))   ; text content
              z   (atof txt))          ; convert to number
        
        ;; Create point at X,Y of text with Z from text value
        (entmake
          (list
            '(0 . "POINT")
            (cons 10 (list (car ins) (cadr ins) z))
          )
        )
        (setq i (1+ i))
      )
      (prompt (strcat "\nCreated " (itoa (sslength ss)) " points."))
    )
    (prompt "\nNo text selected.")
  )
  (princ)
)

(princ "\nPoint tools loaded: PTT, PTTT, TXT2PT")
(princ)
