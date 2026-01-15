;;; Удаление дублирующихся блоков с одинаковыми координатами вставки
;;; Находит и удаляет блоки, расположенные в одной точке
;;; Команда: DelDupBlocks

(defun c:DelDupBlocks ()
  (setq blks (ssget "_X" '((0 . "INSERT"))))  ; Get all blocks
  (if blks
    (progn
      (setq seen '())  ; List to track insertion points
      (setq i 0)  ; Counter for deleted blocks
      (setq j 0)  ; Counter for processed blocks
      (while (< j (sslength blks))
        (setq ent (ssname blks j))  ; Get object from selection
        (setq blkData (entget ent))  ; Get block properties
        (setq pos (cdr (assoc 10 blkData)))  ; Extract insertion point
        (if pos
          (if (member pos seen)
            (progn
              (entdel ent)  ; Delete duplicate block
              (setq i (1+ i))
            )
            (setq seen (cons pos seen))  ; Add insertion point to seen list
          )
        )
        (setq j (1+ j))
      )
      (princ (strcat "\nDeleted duplicate blocks: " (itoa i)))
    )
    (princ "\nNo blocks found.")
  )
  (princ)
)