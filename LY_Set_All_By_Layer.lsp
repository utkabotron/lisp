;;; Установка цвета "По слою" (BYLAYER) для всех объектов
;;; Обрабатывает все объекты включая вложенные блоки
;;; Команда: SetAllByLayer

; This function sets color BYLAYER (256) for all entities in the drawing (including inside all blocks)
(defun c:SetAllByLayer ( / )
  (princ "\nSetting all objects' color to BYLAYER...")
  (SetAllByLayer-AllEnts (ssget "X"))
  (SetAllByLayer-Blocks)
  (princ "\nDone.")
  (princ)
)

; Set color to BYLAYER for all entities in selection set
(defun SetAllByLayer-AllEnts (ss / i ent data enttype)
  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (setq data (entget ent))
        (setq enttype (cdr (assoc 0 data)))
        ; Always set color to BYLAYER (256)
        (if (assoc 62 data)
          (setq data (subst (cons 62 256) (assoc 62 data) data))
          (setq data (append data (list (cons 62 256))))
        )
        (entmod data)
        ; For blocks: process nested
        (if (and (= enttype "INSERT") (cdr (assoc 2 data)))
          (SetAllByLayer-Blocks-Recursive (cdr (assoc 2 data)))
        )
        (setq i (1+ i))
      )
    )
  )
)

; Process all block definitions (excluding anonymous/XREF blocks)
(defun SetAllByLayer-Blocks ( / blkn blkname )
  (setq blkn (tblnext "BLOCK" T))
  (while blkn
    (setq blkname (cdr (assoc 2 blkn)))
    (if (and blkname (not (wcmatch blkname "*|*")))
      (SetAllByLayer-Blocks-Recursive blkname)
    )
    (setq blkn (tblnext "BLOCK"))
  )
)

; Recursively set BYLAYER color for all entities inside block definition
(defun SetAllByLayer-Blocks-Recursive (blkname / entname data enttype )
  (if (and blkname (tblsearch "BLOCK" blkname))
    (progn
      (setq entname (tblobjname "BLOCK" blkname))
      (setq entname (entnext entname))
      (while entname
        (setq data (entget entname))
        (setq enttype (cdr (assoc 0 data)))
        ; Always set color to BYLAYER (256)
        (if (assoc 62 data)
          (setq data (subst (cons 62 256) (assoc 62 data) data))
          (setq data (append data (list (cons 62 256))))
        )
        (entmod data)
        ; If nested block, process recursively
        (if (and (= enttype "INSERT") (cdr (assoc 2 data)))
          (SetAllByLayer-Blocks-Recursive (cdr (assoc 2 data)))
        )
        (setq entname (entnext entname))
      )
    )
  )
)

(princ "\nType SetAllByLayer to set all colors to BYLAYER.")
