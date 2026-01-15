;;; Вставка блоков M1502 на место точек и вершин объектов с нумерацией и высотой
;;; Выбирает точки (POINT), линии, полилинии, окружности и вставляет блок M1502
;;; Для линий - на начальной и конечной точках
;;; Для полилиний - на всех вершинах
;;; Для окружностей - в центре и/или на 4 квадрантах (настраивается)
;;; Автоматически заполняет атрибуты: номер и высота из Z-координаты
;;; Команда: POP

;; Global variable to store last used number
(if (not *POP-LAST-NUM*) (setq *POP-LAST-NUM* 1))

;; Global variable to store circle mode (All/Center/Quadrants)
(if (not *POP-CIRCLE-MODE*) (setq *POP-CIRCLE-MODE* "A"))

(defun c:POP ( / ss i count ent entdata typ pt blkname startnum prefix suffix scale 
                z-coord num-str height-str current-num nextent nextdata cen rad input)
  (vl-load-com)
  
  ;; Default values
  (setq prefix "")
  (setq suffix "")
  (setq startnum *POP-LAST-NUM*)
  
  ;; Get user input with options - use getint with keyword options
  (initget "Prefix Suffix")  ; Allow null (Enter) for default value
  (setq input (getint (strcat "\nStarting number [Prefix/Suffix] <" (itoa *POP-LAST-NUM*) ">: ")))
  
  ;; If input is nil, user pressed Enter - use default
  (if (not input)
    (setq input *POP-LAST-NUM*)
  )
  
  ;; Process input (always continues unless Escape was pressed during nested prompts)
  (cond
    ;; User wants to set prefix
    ((= input "Prefix")
      (setq prefix (getstring T "\nEnter prefix: "))
      (if (not prefix) (setq prefix ""))
      (initget "Suffix")
      (setq input (getint (strcat "\nStarting number [Suffix] <" (itoa *POP-LAST-NUM*) ">: ")))
      (if (not input) (setq input *POP-LAST-NUM*))
      (if (= input "Suffix")
        (progn
          (setq suffix (getstring T "\nEnter suffix: "))
          (if (not suffix) (setq suffix ""))
          (initget 7)  ; 1=no null, 2=no zero, 4=no negative (for final number only)
          (setq startnum (getint (strcat "\nStarting number <" (itoa *POP-LAST-NUM*) ">: ")))
          (if (not startnum) (setq startnum *POP-LAST-NUM*))
        )
        (if (numberp input)
          (setq startnum input)
          (setq startnum *POP-LAST-NUM*)
        )
      )
    )
    ;; User wants to set suffix
    ((= input "Suffix")
      (setq suffix (getstring T "\nEnter suffix: "))
      (if (not suffix) (setq suffix ""))
      (initget "Prefix")
      (setq input (getint (strcat "\nStarting number [Prefix] <" (itoa *POP-LAST-NUM*) ">: ")))
      (if (not input) (setq input *POP-LAST-NUM*))
      (if (= input "Prefix")
        (progn
          (setq prefix (getstring T "\nEnter prefix: "))
          (if (not prefix) (setq prefix ""))
          (initget 7)  ; 1=no null, 2=no zero, 4=no negative (for final number only)
          (setq startnum (getint (strcat "\nStarting number <" (itoa *POP-LAST-NUM*) ">: ")))
          (if (not startnum) (setq startnum *POP-LAST-NUM*))
        )
        (if (numberp input)
          (setq startnum input)
          (setq startnum *POP-LAST-NUM*)
        )
      )
    )
    ;; User entered number directly or pressed Enter
    (T
      (setq startnum input)
    )
  )
      
      ;; Get block scale based on annotation scale
      (setq scale (get-block-scale))
  
  ;; Check if block M1502 exists, if not - try to load it
  (setq blkname (get-or-load-block "M1502"))
  (if (not blkname)
    (progn
      (princ "\nError: Block M1502 not found and could not be loaded!")
      (princ)
      (setq blkname nil)
    )
  )
  
  (if (not blkname)
    (progn
      (princ)
      (exit)
    )
  )
  
  ;; Create layer M1502 if it doesn't exist
  (create-layer "M1502")
  
  ;; Select objects
  (princ "\nSelect objects (POINT, LINE, POLYLINE, CIRCLE): ")
  (setq ss (ssget '((0 . "POINT,LINE,LWPOLYLINE,POLYLINE,CIRCLE"))))
  
  (if ss
    (progn
      (setq count (sslength ss))
      (setq i 0)
      (setq current-num startnum)
      (setq *CIRCLE-MODE-ASKED* nil)  ; Reset circle mode flag
      
      (princ (strcat "\nProcessing " (itoa count) " objects..."))
      
      (while (< i count)
        (setq ent (ssname ss i))
        (setq entdata (entget ent))
        (setq typ (cdr (assoc 0 entdata)))
        
        (cond
          ;; --- POINT ---
          ((= typ "POINT")
            (setq pt (cdr (assoc 10 entdata)))
            (process-point pt current-num prefix suffix blkname scale)
            (setq current-num (1+ current-num))
          )
          
          ;; --- LINE ---
          ((= typ "LINE")
            (setq current-num (process-line entdata current-num prefix suffix blkname scale))
          )
          
          ;; --- 2D POLYLINE ---
          ((= typ "LWPOLYLINE")
            (setq current-num (process-lwpolyline entdata current-num prefix suffix blkname scale))
          )
          
          ;; --- 3D POLYLINE ---
          ((= typ "POLYLINE")
            (setq current-num (process-polyline ent current-num prefix suffix blkname scale))
          )
          
          ;; --- CIRCLE ---
          ((= typ "CIRCLE")
            (setq current-num (process-circle-insblp entdata current-num prefix suffix blkname scale))
          )
        )
        
        (setq i (1+ i))
      )
      
      ;; Store last number for next run
      (setq *POP-LAST-NUM* current-num)
      
      (princ (strcat "\nDone! Last number: " prefix (itoa (1- current-num)) suffix))
    )
    (princ "\nNo objects selected.")
  )
  (princ)
)

;;; Process single point
(defun process-point (pt current-num prefix suffix blkname scale / num-str height-str z-coord)
  (setq z-coord (caddr pt))
  (if (not z-coord) (setq z-coord 0.0))  ; Default to 0 if Z is nil
  (setq num-str (strcat prefix (itoa current-num) suffix))
  (setq height-str (rtos z-coord 2 2))
  (insert-block-with-attribs pt blkname scale num-str height-str)
)

;;; Process line (2 points: start and end)
(defun process-line (entdata current-num prefix suffix blkname scale / pt1 pt2)
  (setq pt1 (cdr (assoc 10 entdata)))
  (setq pt2 (cdr (assoc 11 entdata)))
  
  (process-point pt1 current-num prefix suffix blkname scale)
  (process-point pt2 (1+ current-num) prefix suffix blkname scale)
  
  (+ current-num 2)
)

;;; Process 2D polyline (LWPOLYLINE)
(defun process-lwpolyline (entdata current-num prefix suffix blkname scale / pt)
  (foreach x entdata
    (if (= (car x) 10)
      (progn
        (setq pt (cdr x))
        (process-point pt current-num prefix suffix blkname scale)
        (setq current-num (1+ current-num))
      )
    )
  )
  current-num
)

;;; Process 3D polyline (POLYLINE)
(defun process-polyline (ent current-num prefix suffix blkname scale / nextent nextdata pt)
  (setq nextent (entnext ent))
  (while nextent
    (setq nextdata (entget nextent))
    (if (= (cdr (assoc 0 nextdata)) "VERTEX")
      (progn
        (setq pt (cdr (assoc 10 nextdata)))
        (process-point pt current-num prefix suffix blkname scale)
        (setq current-num (1+ current-num))
      )
    )
    (setq nextent (entnext nextent))
  )
  current-num
)

;;; Process circle with mode selection
(defun process-circle-insblp (entdata current-num prefix suffix blkname scale / cen rad mode)
  (setq cen (cdr (assoc 10 entdata)))
  (setq rad (cdr (assoc 40 entdata)))
  
  ;; Ask user for mode (only once per command run)
  (if (not *CIRCLE-MODE-ASKED*)
    (progn
      (initget "All Center Quadrants")
      (setq mode (getkword (strcat "\nCircle blocks [All/Center/Quadrants] <" *POP-CIRCLE-MODE* ">: ")))
      (if mode
        (setq *POP-CIRCLE-MODE* (substr mode 1 1))
      )
      (setq *CIRCLE-MODE-ASKED* T)
    )
  )
  
  ;; Insert blocks based on mode
  (cond
    ;; All (center + 4 quadrants)
    ((= *POP-CIRCLE-MODE* "A")
      (process-point cen current-num prefix (strcat "C" suffix) blkname scale)
      (setq current-num (1+ current-num))
      (process-point (list (+ (car cen) rad) (cadr cen) (caddr cen)) current-num prefix suffix blkname scale)
      (setq current-num (1+ current-num))
      (process-point (list (car cen) (+ (cadr cen) rad) (caddr cen)) current-num prefix suffix blkname scale)
      (setq current-num (1+ current-num))
      (process-point (list (- (car cen) rad) (cadr cen) (caddr cen)) current-num prefix suffix blkname scale)
      (setq current-num (1+ current-num))
      (process-point (list (car cen) (- (cadr cen) rad) (caddr cen)) current-num prefix suffix blkname scale)
      (setq current-num (1+ current-num))
    )
    ;; Center only
    ((= *POP-CIRCLE-MODE* "C")
      (process-point cen current-num prefix (strcat "C" suffix) blkname scale)
      (setq current-num (1+ current-num))
    )
    ;; Quadrants only
    ((= *POP-CIRCLE-MODE* "Q")
      (process-point (list (+ (car cen) rad) (cadr cen) (caddr cen)) current-num prefix suffix blkname scale)
      (setq current-num (1+ current-num))
      (process-point (list (car cen) (+ (cadr cen) rad) (caddr cen)) current-num prefix suffix blkname scale)
      (setq current-num (1+ current-num))
      (process-point (list (- (car cen) rad) (cadr cen) (caddr cen)) current-num prefix suffix blkname scale)
      (setq current-num (1+ current-num))
      (process-point (list (car cen) (- (cadr cen) rad) (caddr cen)) current-num prefix suffix blkname scale)
      (setq current-num (1+ current-num))
    )
  )
  
  current-num
)

;;; Helper function to get block scale based on annotation scale
(defun get-block-scale ( / cannoscale)
  (setq cannoscale (getvar "CANNOSCALE"))
  
  (cond
    ((equal cannoscale "1:2000" 1) 1.44)
    ((equal cannoscale "1:1000" 1) 0.72)
    ((equal cannoscale "1:500" 1) 0.36)
    ((equal cannoscale "1:250" 1) 0.18)
    ((equal cannoscale "1:100" 1) 0.08)
    ((equal cannoscale "1:50" 1) 0.04)
    ((equal cannoscale "1:25" 1) 0.02)
    (T 0.2)  ; Default scale
  )
)

;;; Helper function to check if block exists, if not - try to load it
(defun get-or-load-block (blk-name / blk-path)
  ;; First check if block already exists in drawing
  (if (tblsearch "BLOCK" blk-name)
    blk-name
    (progn
      ;; Block not found, try to load from standard path
      (setq blk-path "C:\\LDOCS\\BLOCKS\\M1502.dwg")
      
      (princ (strcat "\nBlock " blk-name " not found in drawing."))
      (princ (strcat "\nTrying to load from: " blk-path))
      
      (if (findfile blk-path)
        (progn
          ;; Insert block from file (invisible, will be deleted)
          (command "_.INSERT" blk-path '(0 0 0) 1 1 0)
          (command "_.ERASE" "L" "")
          
          ;; Check if block was loaded
          (if (tblsearch "BLOCK" blk-name)
            (progn
              (princ (strcat "\nBlock " blk-name " loaded successfully!"))
              blk-name
            )
            nil
          )
        )
        (progn
          ;; File not found, ask user for path
          (princ "\nBlock file not found at standard location.")
          (setq blk-path (getfiled "Select M1502.dwg file" "" "dwg" 8))
          
          (if blk-path
            (progn
              (command "_.INSERT" blk-path '(0 0 0) 1 1 0)
              (command "_.ERASE" "L" "")
              
              (if (tblsearch "BLOCK" blk-name)
                (progn
                  (princ (strcat "\nBlock " blk-name " loaded successfully!"))
                  blk-name
                )
                nil
              )
            )
            nil
          )
        )
      )
    )
  )
)

;;; Helper function to create layer if it doesn't exist
(defun create-layer (layer-name / acadObj doc layers newLayer)
  (setq acadObj (vlax-get-acad-object))
  (setq doc (vla-get-ActiveDocument acadObj))
  (setq layers (vla-get-Layers doc))
  
  (if (not (tblsearch "LAYER" layer-name))
    (progn
      (setq newLayer (vla-Add layers layer-name))
      (princ (strcat "\nCreated layer: " layer-name))
    )
  )
)

;;; Helper function to insert block with attributes
(defun insert-block-with-attribs (pt blkname scale num-str height-str / 
                                   acadObj doc mspace insertPnt blkObj attribs att-count i att pt3d)
  (setq acadObj (vlax-get-acad-object))
  (setq doc (vla-get-ActiveDocument acadObj))
  (setq mspace (vla-get-ModelSpace doc))
  
  ;; Ensure point has 3 coordinates
  (if (= (length pt) 2)
    (setq pt3d (list (car pt) (cadr pt) 0.0))
    (setq pt3d pt)
  )
  
  ;; Convert point to variant
  (setq insertPnt (vlax-3d-point pt3d))
  
  ;; Insert block
  (setq blkObj (vla-InsertBlock mspace insertPnt blkname scale scale scale 0.0))
  
  ;; Set layer
  (vla-put-Layer blkObj "M1502")
  
  ;; Get attributes
  (if (= (vla-get-HasAttributes blkObj) :vlax-true)
    (progn
      (setq attribs (vlax-variant-value (vla-GetAttributes blkObj)))
      (setq att-count (vlax-safearray-get-u-bound attribs 1))
      
      ;; Loop through attributes
      (setq i 0)
      (while (<= i att-count)
        (setq att (vlax-safearray-get-element attribs i))
        
        (cond
          ;; Attribute for height (usually index 1 or tag "HEIGHT")
          ((or (= i 1) 
               (= (strcase (vla-get-TagString att)) "HEIGHT")
               (= (strcase (vla-get-TagString att)) "H")
               (= (strcase (vla-get-TagString att)) "Z"))
            (if (/= (caddr pt) 0.0)
              (vla-put-TextString att height-str)
            )
          )
          
          ;; Attribute for number (usually index 2 or tag "NUMBER")
          ((or (= i 2)
               (= (strcase (vla-get-TagString att)) "NUMBER")
               (= (strcase (vla-get-TagString att)) "NUM")
               (= (strcase (vla-get-TagString att)) "N"))
            (vla-put-TextString att num-str)
          )
        )
        
        (setq i (1+ i))
      )
    )
  )
  
  (princ ".")  ; Progress indicator
)

(princ "\nType POP to insert M1502 blocks at point locations.")
(princ)
