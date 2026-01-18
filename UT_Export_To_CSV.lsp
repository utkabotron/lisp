;;; Экспорт данных объектов в CSV файл
;;; Команда: EXPORTCSV - с сохранением файла
;;; Команда: EXPORTEXCEL - автоматически открывает в Excel без сохранения

(defun c:EXPORTCSV (/ ss csv-file filepath obj-count)
  (export-to-csv-main nil)
)

(defun c:EXPORTEXCEL (/ ss csv-file filepath obj-count)
  (export-to-csv-main T)
)

(defun export-to-csv-main (auto-open / ss csv-file filepath obj-count temp-file random-num dwg-name base-name)
  (vl-load-com)

  ;; Запрос выбора объектов
  (princ "\nSelect objects to export: ")
  (setq ss (ssget))

  (if (not ss)
    (progn
      (princ "\nNo objects selected!")
      (princ)
    )
    (progn
      ;; Определяем путь к файлу
      (if auto-open
        ;; Создаем временный файл с именем текущего чертежа + случайное число
        (progn
          (setq random-num (rem (getvar "MILLISECS") 99999))
          (setq dwg-name (getvar "DWGNAME"))
          ;; Убираем расширение .dwg
          (if (vl-string-search ".dwg" (strcase dwg-name))
            (setq base-name (vl-string-subst "" ".dwg" (strcase dwg-name)))
            (setq base-name dwg-name)
          )
          (setq filepath (strcat (getenv "TEMP") "\\"
                                 base-name "_"
                                 (rtos random-num 2 0) ".csv"))
        )
        ;; Запрос имени файла
        (setq filepath (getfiled "Save CSV file" "c:/export" "csv" 1))
      )
      
      (if filepath
        (progn
          ;; Открываем файл для записи
          (setq csv-file (open filepath "w"))
          
          (if csv-file
            (progn
              ;; Обрабатываем каждый объект
              (setq obj-count 0)
              (setq i 0)
              (repeat (sslength ss)
                (setq ent (ssname ss i))
                (setq obj (vlax-ename->vla-object ent))
                (setq obj-type (vla-get-ObjectName obj))
                
                ;; Обрабатываем в зависимости от типа
                (cond
                  ((= obj-type "AcDbBlockReference")
                   (export-block obj csv-file)
                   (setq obj-count (1+ obj-count)))
                  
                  ((= obj-type "AcDbLine")
                   (export-line obj csv-file)
                   (setq obj-count (+ obj-count 2)))
                  
                  ((= obj-type "AcDbCircle")
                   (export-circle obj csv-file)
                   (setq obj-count (1+ obj-count)))
                  
                  ((= obj-type "AcDbArc")
                   (export-arc obj csv-file)
                   (setq obj-count (+ obj-count 4)))
                  
                  ((or (= obj-type "AcDbPolyline") 
                       (= obj-type "AcDb2dPolyline")
                       (= obj-type "AcDb3dPolyline"))
                   (setq obj-count (+ obj-count (export-polyline obj csv-file))))
                  
                  ((or (= obj-type "AcDbText") (= obj-type "AcDbMText"))
                   (export-text obj csv-file)
                   (setq obj-count (1+ obj-count)))
                  
                  ((= obj-type "AcDbPoint")
                   (export-point obj csv-file)
                   (setq obj-count (1+ obj-count)))
                  
                  ((or (= obj-type "AcDbAlignedDimension")
                       (= obj-type "AcDbRotatedDimension")
                       (= obj-type "AcDbLinearDimension"))
                   (export-dimension obj csv-file)
                   (setq obj-count (1+ obj-count)))
                )
                
                (setq i (1+ i))
              )
              
              ;; Закрываем файл
              (close csv-file)
              
              (princ (strcat "\nExported " (itoa obj-count) " rows to: " filepath))
              
              ;; Автоматически открываем в Excel если нужно
              (if auto-open
                (progn
                  (princ "\nOpening in Excel...")
                  ;; Используем системную команду для открытия файла
                  (startapp "cmd" (strcat "/c start \"\" \"" filepath "\""))
                )
                (princ "\nFile can be opened in Excel.")
              )
            )
            (princ "\nError: Cannot create file!")
          )
        )
        (princ "\nExport cancelled.")
      )
      (princ)
    )
  )
)

;; Вспомогательная функция для форматирования числа
(defun format-num (num)
  (if num
    (rtos num 2 4)
    ""
  )
)

;; Вспомогательная функция для экранирования CSV
(defun csv-escape (str)
  (if (and str (= (type str) 'STR))
    (if (or (vl-string-search "," str) (vl-string-search "\"" str))
      (strcat "\"" (vl-string-subst "\"\"" "\"" str) "\"")
      str
    )
    ""
  )
)

;; Экспорт блока
(defun export-block (obj csv-file / pt name atts att-vals handle layer csv-line att-str att-count j att)
  (setq pt (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint obj))))
  (setq name (vla-get-EffectiveName obj))
  (setq handle (vla-get-Handle obj))
  (setq layer (vla-get-Layer obj))
  
  ;; Начинаем строку CSV с имени блока и координат
  (setq csv-line (strcat 
    (csv-escape name) ","
    (format-num (car pt)) ","
    (format-num (cadr pt)) ","
    (format-num (caddr pt))
  ))
  
  ;; Добавляем атрибуты в порядке их следования
  (if (= (vla-get-HasAttributes obj) :vlax-true)
    (progn
      (setq atts (vlax-variant-value (vla-GetAttributes obj)))
      (setq att-count (vlax-safearray-get-u-bound atts 1))
      (setq j 0)
      (while (<= j att-count)
        (setq att (vlax-safearray-get-element atts j))
        (setq att-str (vla-get-TextString att))
        (setq csv-line (strcat csv-line "," (csv-escape att-str)))
        (setq j (1+ j))
      )
    )
  )
  
  (write-line csv-line csv-file)
)

;; Экспорт линии
(defun export-line (obj csv-file / pt1 pt2 len handle layer)
  (setq pt1 (vlax-safearray->list (vlax-variant-value (vla-get-StartPoint obj))))
  (setq pt2 (vlax-safearray->list (vlax-variant-value (vla-get-EndPoint obj))))
  (setq len (vla-get-Length obj))
  (setq handle (vla-get-Handle obj))
  (setq layer (vla-get-Layer obj))
  
  ;; Начальная точка
  (write-line (strcat "LINE," 
    (format-num (car pt1)) "," 
    (format-num (cadr pt1)) "," 
    (format-num (caddr pt1)) ","
    (format-num len) ",,,,"
    handle ","
    layer) csv-file)
  
  ;; Конечная точка
  (write-line (strcat "LINE," 
    (format-num (car pt2)) "," 
    (format-num (cadr pt2)) "," 
    (format-num (caddr pt2)) ","
    (format-num len) ",,,,"
    handle ","
    layer) csv-file)
)

;; Экспорт окружности
(defun export-circle (obj csv-file / pt rad handle layer)
  (setq pt (vlax-safearray->list (vlax-variant-value (vla-get-Center obj))))
  (setq rad (vla-get-Radius obj))
  (setq handle (vla-get-Handle obj))
  (setq layer (vla-get-Layer obj))
  
  (write-line (strcat "CIRCLE," 
    (format-num (car pt)) "," 
    (format-num (cadr pt)) "," 
    (format-num (caddr pt)) ","
    (format-num rad) ",,,,"
    handle ","
    layer) csv-file)
)

;; Экспорт дуги
(defun export-arc (obj csv-file / pt1 pt2 center rad handle layer)
  (setq pt1 (vlax-safearray->list (vlax-variant-value (vla-get-StartPoint obj))))
  (setq pt2 (vlax-safearray->list (vlax-variant-value (vla-get-EndPoint obj))))
  (setq center (vlax-safearray->list (vlax-variant-value (vla-get-Center obj))))
  (setq rad (vla-get-Radius obj))
  (setq handle (vla-get-Handle obj))
  (setq layer (vla-get-Layer obj))
  
  ;; Начало дуги
  (write-line (strcat "ArcStart," 
    (format-num (car pt1)) "," 
    (format-num (cadr pt1)) "," 
    (format-num (caddr pt1)) ",,,,"
    handle ","
    layer) csv-file)
  
  ;; Конец дуги
  (write-line (strcat "ArcEnd," 
    (format-num (car pt2)) "," 
    (format-num (cadr pt2)) "," 
    (format-num (caddr pt2)) ",,,,"
    handle ","
    layer) csv-file)
  
  ;; Центр дуги
  (write-line (strcat "ArcCenter," 
    (format-num (car center)) "," 
    (format-num (cadr center)) "," 
    (format-num (caddr center)) ",,,,"
    handle ","
    layer) csv-file)
  
  ;; Радиус
  (write-line (strcat "ArcRadius,,,," 
    (format-num rad) ",,,,"
    handle ","
    layer) csv-file)
)

;; Экспорт полилинии
(defun export-polyline (obj csv-file / coords len pt-count i x y z handle layer count)
  (setq coords (vlax-safearray->list (vlax-variant-value (vla-get-Coordinates obj))))
  (setq len (vla-get-Length obj))
  (setq handle (vla-get-Handle obj))
  (setq layer (vla-get-Layer obj))
  (setq count 0)
  
  ;; Определяем 2D или 3D полилиния
  (if (= (rem (length coords) 2) 0)
    ;; 2D полилиния
    (progn
      (setq i 0)
      (while (< i (length coords))
        (setq x (nth i coords))
        (setq y (nth (+ i 1) coords))
        (setq z 0.0)
        
        (write-line (strcat "PLINE," 
          (format-num x) "," 
          (format-num y) "," 
          (format-num z) ","
          (format-num len) ",,,,"
          handle ","
          layer) csv-file)
        
        (setq i (+ i 2))
        (setq count (1+ count))
      )
    )
    ;; 3D полилиния
    (progn
      (setq i 0)
      (while (< i (length coords))
        (setq x (nth i coords))
        (setq y (nth (+ i 1) coords))
        (setq z (nth (+ i 2) coords))
        
        (write-line (strcat "PLINE," 
          (format-num x) "," 
          (format-num y) "," 
          (format-num z) ","
          (format-num len) ",,,,"
          handle ","
          layer) csv-file)
        
        (setq i (+ i 3))
        (setq count (1+ count))
      )
    )
  )
  count
)

;; Экспорт текста
(defun export-text (obj csv-file / pt txt handle layer)
  (setq pt (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint obj))))
  (setq txt (vla-get-TextString obj))
  (setq handle (vla-get-Handle obj))
  (setq layer (vla-get-Layer obj))
  
  (write-line (strcat "TEXT," 
    (format-num (car pt)) "," 
    (format-num (cadr pt)) "," 
    (format-num (caddr pt)) ","
    (csv-escape txt) ",,,,"
    handle ","
    layer) csv-file)
)

;; Экспорт точки
(defun export-point (obj csv-file / pt handle layer)
  (setq pt (vlax-safearray->list (vlax-variant-value (vla-get-Coordinates obj))))
  (setq handle (vla-get-Handle obj))
  (setq layer (vla-get-Layer obj))
  
  (write-line (strcat "POINT," 
    (format-num (car pt)) "," 
    (format-num (cadr pt)) "," 
    (format-num (caddr pt)) ",,,,"
    handle ","
    layer) csv-file)
)

;; Экспорт размера
(defun export-dimension (obj csv-file / measurement handle layer)
  (setq measurement (vla-get-Measurement obj))
  (setq handle (vla-get-Handle obj))
  (setq layer (vla-get-Layer obj))
  
  (write-line (strcat "DIMENSION,,,," 
    (format-num measurement) ",,,,"
    handle ","
    layer) csv-file)
)

(princ "\nExport to CSV tool loaded.")
(princ "\n  EXPORTCSV - Export to CSV file (save dialog)")
(princ "\n  EXPORTEXCEL - Export and open in Excel automatically")
(princ)
