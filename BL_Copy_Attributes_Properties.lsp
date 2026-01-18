;;; Копирование свойств атрибутов и масштаба блока
;;; Команда: COPYATTPROP

(defun c:COPYATTPROP (/ source-block target-blocks source-atts source-insert source-angle source-scale)
  (vl-load-com)
  
  ;; Функция для вычисления расстояния между двумя точками (2D)
  (defun dist2d (p1 p2)
    (sqrt (+ (expt (- (car p2) (car p1)) 2)
             (expt (- (cadr p2) (cadr p1)) 2)))
  )
  
  ;; Функция для вычисления угла между двумя точками
  (defun calc-angle (p1 p2)
    (atan (- (cadr p2) (cadr p1))
          (- (car p2) (car p1)))
  )
  
  ;; Функция для копирования свойств атрибутов
  (defun copy-attribute-properties (source-att target-att source-insert source-angle target-insert target-angle / 
                                     att-dist att-angle-offset new-angle new-insert)
    
    ;; Вычисляем новую позицию атрибута
    (setq att-dist (dist2d source-insert 
                           (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint source-att)))))
    (setq att-angle-offset (- (calc-angle source-insert 
                                          (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint source-att))))
                              source-angle))
    (setq new-angle (+ att-angle-offset target-angle))
    
    ;; Новая позиция атрибута
    (setq new-insert (list (+ (car target-insert) (* att-dist (cos new-angle)))
                           (+ (cadr target-insert) (* att-dist (sin new-angle)))
                           (caddr target-insert)))
    
    ;; Сначала устанавливаем позицию и угол поворота
    (vla-put-Rotation target-att (+ (- (vla-get-Rotation source-att) source-angle) target-angle))
    (vla-Move target-att 
              (vla-get-InsertionPoint target-att)
              (vlax-3d-point new-insert))
    
    ;; Затем копируем остальные текстовые свойства
    (vla-put-Height target-att (vla-get-Height source-att))
    (vla-put-ScaleFactor target-att (vla-get-ScaleFactor source-att))
    (vla-put-ObliqueAngle target-att (vla-get-ObliqueAngle source-att))
    (vla-put-Alignment target-att (vla-get-Alignment source-att))
    (vla-put-Layer target-att (vla-get-Layer source-att))
    (vla-put-StyleName target-att (vla-get-StyleName source-att))
    (vla-put-Invisible target-att (vla-get-Invisible source-att))
    (vla-put-Color target-att (vla-get-Color source-att))
  )
  
  ;; Выбор исходного блока (образца)
  (princ "\nSelect source block (pattern): ")
  (setq source-block (car (entsel)))
  
  (if (not source-block)
    (progn
      (princ "\nNo block selected!")
      (exit)
    )
  )
  
  ;; Проверка что это блок
  (if (/= (cdr (assoc 0 (entget source-block))) "INSERT")
    (progn
      (princ "\nSelected object is not a block!")
      (exit)
    )
  )
  
  ;; Получаем VLA объект исходного блока
  (setq source-block (vlax-ename->vla-object source-block))
  
  ;; Проверка наличия атрибутов
  (if (/= (vla-get-HasAttributes source-block) :vlax-true)
    (progn
      (princ "\nSource block has no attributes!")
      (exit)
    )
  )
  
  ;; Получаем свойства исходного блока
  (setq source-atts (vlax-variant-value (vla-GetAttributes source-block)))
  (setq source-insert (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint source-block))))
  (setq source-angle (vla-get-Rotation source-block))
  (setq source-scale (vla-get-XScaleFactor source-block))  ; Получаем масштаб
  
  ;; Выбор целевых блоков
  (princ "\nSelect target blocks: ")
  (setq target-blocks (ssget '((0 . "INSERT"))))
  
  (if (not target-blocks)
    (progn
      (princ "\nNo target blocks selected!")
      (exit)
    )
  )
  
  ;; Обрабатываем каждый целевой блок
  (setq i 0)
  (setq count 0)
  (repeat (sslength target-blocks)
    (setq target-ent (ssname target-blocks i))
    (setq target-block (vlax-ename->vla-object target-ent))
    
    ;; Проверяем наличие атрибутов
    (if (= (vla-get-HasAttributes target-block) :vlax-true)
      (progn
        ;; Получаем свойства целевого блока
        (setq target-atts (vlax-variant-value (vla-GetAttributes target-block)))
        (setq target-insert (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint target-block))))
        (setq target-angle (vla-get-Rotation target-block))
        
        ;; Копируем масштаб блока
        (vla-put-XScaleFactor target-block source-scale)
        (vla-put-YScaleFactor target-block source-scale)
        (vla-put-ZScaleFactor target-block source-scale)
        
        ;; Копируем свойства каждого атрибута
        (setq att-count (min (vlax-safearray-get-u-bound source-atts 1)
                             (vlax-safearray-get-u-bound target-atts 1)))
        
        (setq j 0)
        (while (<= j att-count)
          (setq source-att (vlax-safearray-get-element source-atts j))
          (setq target-att (vlax-safearray-get-element target-atts j))
          
          (copy-attribute-properties source-att target-att 
                                    source-insert source-angle 
                                    target-insert target-angle)
          
          (setq j (1+ j))
        )
        
        (setq count (1+ count))
      )
    )
    
    (setq i (1+ i))
  )
  
  (princ (strcat "\nProcessed " (itoa count) " blocks."))
  (vla-Regen (vla-get-ActiveDocument (vlax-get-acad-object)) acAllViewports)
  (princ)
)

(princ "\nCopy Attribute Properties tool loaded. Type COPYATTPROP to start.")
(princ)
