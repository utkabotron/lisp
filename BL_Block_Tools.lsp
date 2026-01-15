;;; Инструменты для работы с блоками
;;; Команды: RX90, RY90, PB3

;;; RX90 - Поворот выбранных блоков на 90 градусов вокруг оси X
(defun c:RX90 (/ ss i blkName insPt)
  (setq ss (ssget '((0 . "INSERT")))) ; Пользователь выбирает блоки
  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq blkName (ssname ss i))
        (setq insPt (cdr (assoc 10 (entget blkName)))) ; Получаем точку вставки
        (command "_.ROTATE3D" blkName "" "X" insPt "90") ; Поворачиваем вокруг X
        (command "_.MOVE" blkName "" insPt insPt) ; Перемещаем обратно в ту же точку
        (setq i (1+ i))
      )
      (princ "\nВыбранные блоки повернуты на 90 градусов вокруг X без смещения точки вставки.")
    )
    (princ "\nВы не выбрали ни одного блока.")
  )
  (princ)
)

;;; RY90 - Поворот выбранных блоков на 90 градусов вокруг оси Y
(defun c:RY90 (/ ss i blkName insPt)
  (setq ss (ssget '((0 . "INSERT")))) ; Пользователь выбирает блоки
  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq blkName (ssname ss i))
        (setq insPt (cdr (assoc 10 (entget blkName)))) ; Получаем точку вставки
        (command "_.ROTATE3D" blkName "" "Y" insPt "90") ; Поворачиваем вокруг Y
        (command "_.MOVE" blkName "" insPt insPt) ; Перемещаем обратно в ту же точку
        (setq i (1+ i))
      )
      (princ "\nВыбранные блоки повернуты на 90 градусов вокруг Y без смещения точки вставки.")
    )
    (princ "\nВы не выбрали ни одного блока.")
  )
  (princ)
)

;;; PB3 - Перемещение блоков на Z-координату из 3D полилинии
(defun c:PB3 (/ pl blks blk p3d z)
  (vl-load-com)
  (setq pl (car (entsel "\nSelect 3D polyline: ")))  ;; Select 3D polyline
  (setq blks (ssget '((0 . "INSERT"))))  ;; Select all blocks
  
  (if (and pl blks)
    (progn
      (setq pl (vlax-ename->vla-object pl))
      (vlax-for blk (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object)))
        (if (member (vlax-vla-object->ename blk) (mapcar 'cadr (ssnamex blks)))
          (progn
            (setq p3d (vlax-curve-getClosestPointTo pl (vlax-get blk 'InsertionPoint)))
            (setq z (caddr p3d))  ;; Get Z-coordinate from 3D polyline
            (vla-put-InsertionPoint blk (vlax-3d-point (list (car p3d) (cadr p3d) z)))
          )
        )
      )
    )
    (princ "\nYou did not select 3D polyline or blocks!")
  )
  (princ)
)

(princ "\nBlock tools loaded: RX90, RY90, PB3")
(princ)
