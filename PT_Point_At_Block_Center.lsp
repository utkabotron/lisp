;;; Point at Block Center - Создание точки в центре блока
;;; Команда: PTBC

;;; PTBC - Создание точки в центре выбранного блока
(defun c:PTBC ( / blk vBlk insPt)
  (princ "\nSelect block: ")
  (setq blk (car (entsel)))

  (if (null blk)
    (progn
      (princ "\nNo block selected!")
      (exit)
    )
  )

  ;; Проверка что это блок
  (if (/= (cdr (assoc 0 (entget blk))) "INSERT")
    (progn
      (princ "\nSelected object is not a block!")
      (exit)
    )
  )

  ;; Получение точки вставки блока (центр)
  (setq vBlk (vlax-ename->vla-object blk))
  (setq insPt (vlax-get vBlk 'InsertionPoint))

  ;; Преобразование из variant в список (универсальный способ)
  (if (= (type insPt) 'VARIANT)
    (setq insPt (vlax-safearray->list (vlax-variant-value insPt)))
  )
  ;; Если это уже список, оставляем как есть

  ;; Создание точки в центре блока
  (entmake (list
    '(0 . "POINT")
    (cons 10 insPt)
  ))

  (princ (strcat "\nPoint created at: "
                 (rtos (car insPt) 2 2) ", "
                 (rtos (cadr insPt) 2 2) ", "
                 (rtos (caddr insPt) 2 2)))
  (princ)
)

;;; PTBCM - Создание точек в центре множества блоков
(defun c:PTBCM ( / ss n i eBlk vBlk insPt count)
  (princ "\nSelect blocks: ")
  (setq ss (ssget '((0 . "INSERT"))))

  (if (null ss)
    (progn
      (princ "\nNo blocks selected!")
      (exit)
    )
  )

  (setq n (sslength ss))
  (setq i 0)
  (setq count 0)

  (princ (strcat "\nProcessing " (itoa n) " blocks..."))

  (repeat n
    (setq eBlk (ssname ss i))
    (setq vBlk (vlax-ename->vla-object eBlk))
    (setq insPt (vlax-get vBlk 'InsertionPoint))

    ;; Преобразование из variant в список (универсальный способ)
    (if (= (type insPt) 'VARIANT)
      (setq insPt (vlax-safearray->list (vlax-variant-value insPt)))
    )
    ;; Если это уже список, оставляем как есть

    ;; Создание точки
    (entmake (list
      '(0 . "POINT")
      (cons 10 insPt)
    ))

    (setq count (1+ count))
    (setq i (1+ i))
  )

  (princ (strcat "\n" (itoa count) " points created."))
  (princ)
)

(princ "\nPoint at Block Center loaded: PTBC (single), PTBCM (multiple)")
(princ)
