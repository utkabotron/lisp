;;; Изменение шрифта атрибутов блоков на Arial Unicode MS
;;; Команда: CHANGEATTRFONT

(defun c:CHANGEATTRFONT ( / ss i blk atts att count new-font)
  (vl-load-com)

  ;; Запрос имени шрифта
  (initget 1)
  (setq new-font (getstring T "\nEnter font name (Arial, Times New Roman, ISOCPEUR) <Arial>: "))
  (if (= new-font "") (setq new-font "Arial"))

  (princ "\nSelect blocks with attributes: ")
  (setq ss (ssget '((0 . "INSERT"))))

  (if (not ss)
    (progn
      (princ "\nNo blocks selected!")
      (princ)
      (exit)
    )
  )

  (setq count 0)
  (setq i 0)

  (repeat (sslength ss)
    (setq blk (vlax-ename->vla-object (ssname ss i)))

    (if (= (vla-get-HasAttributes blk) :vlax-true)
      (progn
        (setq atts (vlax-variant-value (vla-GetAttributes blk)))
        (setq j 0)

        (while (<= j (vlax-safearray-get-u-bound atts 1))
          (setq att (vlax-safearray-get-element atts j))

          ;; Получаем текущий стиль
          (setq old-style (vla-get-StyleName att))

          ;; Создаём новый стиль с нужным шрифтом
          (setq new-style-name (strcat "UNICODE_" new-font))
          (create-text-style new-style-name new-font)

          ;; Применяем стиль
          (vla-put-StyleName att new-style-name)

          (setq count (1+ count))
          (setq j (1+ j))
        )
      )
    )

    (setq i (1+ i))
  )

  (vla-Regen (vla-get-ActiveDocument (vlax-get-acad-object)) acAllViewports)
  (princ (strcat "\nUpdated " (itoa count) " attributes to font: " new-font))
  (princ)
)

;; Вспомогательная функция для создания стиля текста
(defun create-text-style (style-name font-name / acad doc styles new-style)
  (setq acad (vlax-get-acad-object))
  (setq doc (vla-get-ActiveDocument acad))
  (setq styles (vla-get-TextStyles doc))

  ;; Проверяем, существует ли стиль
  (if (not (tblsearch "STYLE" style-name))
    (progn
      (setq new-style (vla-Add styles style-name))
      (vla-put-FontFile new-style font-name)
      (vla-put-Height new-style 0.0)  ; Высота по атрибуту
      (princ (strcat "\nCreated text style: " style-name))
    )
  )
)

(princ "\nType CHANGEATTRFONT to change font of attributes to Unicode-compatible.")
(princ)
