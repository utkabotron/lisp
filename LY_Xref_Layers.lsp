;;; Управление слоями внешних ссылок (XREF) в AutoCAD
;;; Изменяет цвет и прозрачность слоев связанных с внешними ссылками
;;; Команда: CHXA (обработка всех XREF слоев)

(vl-load-com)

(defun c:CHXA ( / layer_obj layer_coll layer_name processed_count xref_layers)
  (setq processed_count 0)
  (setq xref_layers '())
  
  (princ "\nScanning XREF layers...")
  
  ;; Access the layer collection
  (setq layer_coll (vla-get-layers (vla-get-activedocument (vlax-get-acad-object))))
  
  ;; First pass: collect all XREF layers and set color
  (vlax-for layer_obj layer_coll
    (setq layer_name (vl-catch-all-apply 'vla-get-name (list layer_obj)))
    
    (if (and layer_name 
             (not (vl-catch-all-error-p layer_name))
             (> (strlen layer_name) 0)
             (vl-string-search "|" layer_name))
      (progn
        (setq processed_count (1+ processed_count))
        (setq xref_layers (cons layer_name xref_layers))
        ;; Set color immediately via VLA
        (vl-catch-all-apply 'vla-put-color (list layer_obj 146))
      )
    )
  )
  
  ;; Second pass: set transparency for all layers in one command
  (if (> processed_count 0)
    (progn
      (princ (strcat "\nSetting transparency for " (itoa processed_count) " layers..."))
      (command "._-layer")
      (foreach layer_name xref_layers
        (command "_transparency" "50" layer_name)
      )
      (command "")
      
      (princ "\nRegenerating...")
      (command "REGENALL")
      (princ (strcat "\nDone! Processed " (itoa processed_count) " XREF layers."))
    )
    (princ "\nNo XREF layers found.")
  )
  (princ)
)

(princ)
