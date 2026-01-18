;;; Управление видимостью атрибутов блоков через диалоговое окно
;;; Команда: ATTVIS

(defun c:ATTVIS (/ dcl_id blocks attr_list attr_names selected_blocks)
  (vl-load-com)
  
  ;; Функция для сбора всех уникальных имен атрибутов из выбранных блоков
  (defun collect-attributes (ss / i ent blk attr-list tag)
    (setq attr-list '())
    (setq i 0)
    (repeat (sslength ss)
      (setq ent (ssname ss i))
      (setq blk (vlax-ename->vla-object ent))
      
      (if (= (vla-get-HasAttributes blk) :vlax-true)
        (foreach att (vlax-invoke blk 'GetAttributes)
          (setq tag (vla-get-TagString att))
          (if (not (member tag attr-list))
            (setq attr-list (append attr-list (list tag)))
          )
        )
      )
      (setq i (1+ i))
    )
    attr-list
  )
  
  ;; Функция для установки видимости атрибутов
  (defun set-attribute-visibility (ss attr-names visible / i ent blk)
    (setq i 0)
    (repeat (sslength ss)
      (setq ent (ssname ss i))
      (setq blk (vlax-ename->vla-object ent))
      
      (if (= (vla-get-HasAttributes blk) :vlax-true)
        (foreach att (vlax-invoke blk 'GetAttributes)
          (if (member (vla-get-TagString att) attr-names)
            (vla-put-Invisible att (if visible :vlax-false :vlax-true))
          )
        )
      )
      (setq i (1+ i))
    )
    (vla-Regen (vla-get-ActiveDocument (vlax-get-acad-object)) acAllViewports)
  )
  
  ;; Выбор блоков
  (princ "\nSelect blocks: ")
  (setq selected_blocks (ssget '((0 . "INSERT"))))
  
  (if (not selected_blocks)
    (progn
      (princ "\nNo blocks selected!")
      (princ)
      (exit)
    )
  )
  
  ;; Собираем список атрибутов
  (setq attr_list (collect-attributes selected_blocks))
  
  (if (null attr_list)
    (progn
      (alert "Selected blocks have no attributes!")
      (princ)
      (exit)
    )
  )
  
  ;; Загружаем DCL файл
  (setq dcl_id (load_dialog "AT_Attribute_Visibility.dcl"))
  
  (if (not (new_dialog "attribute_visibility" dcl_id))
    (progn
      (alert "Cannot load dialog!")
      (princ)
      (exit)
    )
  )
  
  ;; Заполняем список атрибутов
  (start_list "attr_list")
  (mapcar 'add_list attr_list)
  (end_list)
  
  ;; Обработчик кнопки "Visible"
  (action_tile "btn_visible"
    "(progn
       (setq sel_indices (get_tile \"attr_list\"))
       (if (and sel_indices (/= sel_indices \"\"))
         (progn
           (setq selected_attrs (get-selected-items attr_list sel_indices))
           (if selected_attrs
             (progn
               (set-attribute-visibility selected_blocks selected_attrs T)
               (princ (strcat \"\\nMade visible: \" (join-strings selected_attrs \", \")))
             )
           )
         )
         (alert \"Please select attributes from the list!\")
       )
     )"
  )
  
  ;; Обработчик кнопки "Invisible"
  (action_tile "btn_invisible"
    "(progn
       (setq sel_indices (get_tile \"attr_list\"))
       (if (and sel_indices (/= sel_indices \"\"))
         (progn
           (setq selected_attrs (get-selected-items attr_list sel_indices))
           (if selected_attrs
             (progn
               (set-attribute-visibility selected_blocks selected_attrs nil)
               (princ (strcat \"\\nMade invisible: \" (join-strings selected_attrs \", \")))
             )
           )
         )
         (alert \"Please select attributes from the list!\")
       )
     )"
  )
  
  ;; Обработчик кнопки "Exit"
  (action_tile "btn_exit" "(done_dialog)")
  (action_tile "cancel" "(done_dialog)")
  
  ;; Показываем диалог
  (start_dialog)
  (unload_dialog dcl_id)
  
  (princ)
)

;; Вспомогательная функция для получения выбранных элементов из списка
(defun get-selected-items (lst indices-str / indices result idx)
  (if (and indices-str (/= indices-str ""))
    (progn
      (setq indices (read (strcat "(" indices-str ")")))
      (setq result '())
      (foreach idx indices
        (if (and (>= idx 0) (< idx (length lst)))
          (setq result (append result (list (nth idx lst))))
        )
      )
      result
    )
    nil
  )
)

;; Вспомогательная функция для объединения строк
(defun join-strings (lst sep / result)
  (if lst
    (progn
      (setq result (car lst))
      (foreach s (cdr lst)
        (setq result (strcat result sep s))
      )
      result
    )
    ""
  )
)

(princ "\nAttribute Visibility tool loaded. Type ATTVIS to start.")
(princ)
