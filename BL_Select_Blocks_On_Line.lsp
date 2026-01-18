;;; Выбор блоков расположенных на полилиниях с поддержкой дуг
;;; Позволяет накапливать выбор из нескольких полилиний
;;; Команды: SBP (выбрать), SBPC (очистить выбор)

;;; SelectBlocksOnPolyline - with ARC support and accumulative selection
;;; Global variable to accumulate selection

(setq *sbp-selection* nil)

(defun c:SBP (/ ent etype segments blk-list tol
               i ename bent ins-pt bx by found j seg count-new)
  
  ;; Tolerance for coordinate comparison
  (setq tol 0.1)
  
  ;; Initialize selection set if empty
  (if (or (not *sbp-selection*)
          (not (sslength *sbp-selection*)))
    (setq *sbp-selection* (ssadd))
  )
  
  ;; Prompt user to select polyline
  (princ "\nSelect POLYLINE (Enter to finish): ")
  (setq ent (car (entsel)))
  
  (if ent
    (progn
      (setq etype (cdr (assoc 0 (entget ent))))
      
      (cond
        ((= etype "LWPOLYLINE")
         (setq segments (get-lwpoly-segments ent)))
        
        ((= etype "LINE")
         (setq segments (list 
           (list "LINE"
                 (cdr (assoc 10 (entget ent)))
                 (cdr (assoc 11 (entget ent)))))))
        
        (T
         (setq segments nil)
         (princ (strcat "\nUnsupported: " etype)))
      )
      
      (if segments
        (progn
          ;; Get all blocks
          (setq blk-list (ssget "X" '((0 . "INSERT"))))
          
          (if blk-list
            (progn
              (setq count-new 0)
              
              ;; Check each block
              (setq i 0)
              (repeat (sslength blk-list)
                (setq ename (ssname blk-list i))
                (setq bent (entget ename))
                (setq ins-pt (cdr (assoc 10 bent)))
                (setq bx (car ins-pt))
                (setq by (cadr ins-pt))
                
                ;; Check against each segment
                (setq found nil)
                (setq j 0)
                (while (and (not found) (< j (length segments)))
                  (setq seg (nth j segments))
                  (if (point-on-segment bx by seg tol)
                    (setq found T)
                  )
                  (setq j (1+ j))
                )
                
                (if found
                  (progn
                    ;; Add to accumulated selection
                    (if (not (ssmemb ename *sbp-selection*))
                      (progn
                        (ssadd ename *sbp-selection*)
                        (setq count-new (1+ count-new))
                        (princ (strcat "\n  + " (cdr (assoc 2 bent))))
                      )
                    )
                  )
                )
                (setq i (1+ i))
              )
              
              ;; Show results
              (princ (strcat "\n\nAdded: " (itoa count-new) 
                            " | Total: " (itoa (sslength *sbp-selection*))))
              
              ;; Highlight all accumulated
              (sssetfirst nil *sbp-selection*)
            )
          )
        )
      )
    )
    ;; Enter pressed - finish
    (princ (strcat "\nFinished. Total selected: " 
                   (itoa (if *sbp-selection* (sslength *sbp-selection*) 0))))
  )
  (princ)
)


;;; Clear accumulated selection
(defun c:SBPC ()
  (setq *sbp-selection* (ssadd))
  (sssetfirst nil nil)
  (princ "\nSelection cleared.")
  (princ)
)


;;; Extract segments from LWPOLYLINE with bulge (arc) info
(defun get-lwpoly-segments (ent / data pts bulges closed 
                                  i p1 p2 bulge segments
                                  arc-data n)
  (setq data (entget ent))
  (setq pts '())
  (setq bulges '())
  
  (setq closed (= (logand (cdr (assoc 70 data)) 1) 1))
  
  (foreach item data
    (cond
      ((= (car item) 10)
       (setq pts (append pts (list (cdr item)))))
      ((= (car item) 42)
       (setq bulges (append bulges (list (cdr item)))))
    )
  )
  
  (while (< (length bulges) (length pts))
    (setq bulges (append bulges '(0.0)))
  )
  
  (setq segments '())
  (setq n (if closed (length pts) (1- (length pts))))
  
  (setq i 0)
  (repeat n
    (setq p1 (nth i pts))
    (setq p2 (nth (rem (1+ i) (length pts)) pts))
    (setq bulge (nth i bulges))
    
    (if (< (abs bulge) 0.0001)
      (setq segments (append segments 
                             (list (list "LINE" p1 p2))))
      (progn
        (setq arc-data (bulge-to-arc p1 p2 bulge))
        (setq segments (append segments (list arc-data)))
      )
    )
    (setq i (1+ i))
  )
  segments
)


;;; Convert bulge to arc parameters
(defun bulge-to-arc (p1 p2 bulge / dx dy chord half-chord
                                   sagitta radius dist
                                   mid-x mid-y perp-x perp-y
                                   center start-ang end-ang ccw)
  
  (setq dx (- (car p2) (car p1)))
  (setq dy (- (cadr p2) (cadr p1)))
  (setq chord (sqrt (+ (* dx dx) (* dy dy))))
  (setq half-chord (/ chord 2.0))
  
  (setq sagitta (* half-chord (abs bulge)))
  
  (setq radius (/ (+ (* half-chord half-chord) (* sagitta sagitta))
                  (* 2.0 sagitta)))
  
  (setq dist (- radius sagitta))
  
  (setq mid-x (/ (+ (car p1) (car p2)) 2.0))
  (setq mid-y (/ (+ (cadr p1) (cadr p2)) 2.0))
  
  (setq perp-x (/ (- dy) chord))
  (setq perp-y (/ dx chord))
  
  (if (> bulge 0)
    (progn
      (setq center (list (+ mid-x (* dist perp-x))
                         (+ mid-y (* dist perp-y))))
      (setq ccw T))
    (progn
      (setq center (list (- mid-x (* dist perp-x))
                         (- mid-y (* dist perp-y))))
      (setq ccw nil))
  )
  
  (setq start-ang (atan (- (cadr p1) (cadr center))
                        (- (car p1) (car center))))
  (setq end-ang (atan (- (cadr p2) (cadr center))
                      (- (car p2) (car center))))
  
  (list "ARC" p1 p2 center radius start-ang end-ang ccw)
)


;;; Check if point lies on segment (LINE or ARC)
(defun point-on-segment (px py seg tol / stype)
  (setq stype (car seg))
  (cond
    ((= stype "LINE")
     (point-on-line-seg px py (nth 1 seg) (nth 2 seg) tol))
    ((= stype "ARC")
     (point-on-arc-seg px py seg tol))
    (T nil)
  )
)


;;; Check if point lies on straight line segment
(defun point-on-line-seg (px py p1 p2 tol / 
                          x1 y1 x2 y2 dx dy len t-param
                          proj-x proj-y dist)
  
  (setq x1 (car p1) y1 (cadr p1))
  (setq x2 (car p2) y2 (cadr p2))
  (setq dx (- x2 x1))
  (setq dy (- y2 y1))
  (setq len (sqrt (+ (* dx dx) (* dy dy))))
  
  (if (< len 0.0001)
    (< (sqrt (+ (expt (- px x1) 2) (expt (- py y1) 2))) tol)
    (progn
      (setq t-param (/ (+ (* (- px x1) dx) (* (- py y1) dy))
                       (* len len)))
      (if (and (>= t-param -0.001) (<= t-param 1.001))
        (progn
          (setq t-param (max 0.0 (min 1.0 t-param)))
          (setq proj-x (+ x1 (* t-param dx)))
          (setq proj-y (+ y1 (* t-param dy)))
          (setq dist (sqrt (+ (expt (- px proj-x) 2)
                              (expt (- py proj-y) 2))))
          (< dist tol)
        )
        nil
      )
    )
  )
)


;;; Check if point lies on arc segment
(defun point-on-arc-seg (px py seg tol /
                         center radius start-ang end-ang ccw
                         dist-to-center pt-ang)
  
  (setq center (nth 3 seg))
  (setq radius (nth 4 seg))
  (setq start-ang (nth 5 seg))
  (setq end-ang (nth 6 seg))
  (setq ccw (nth 7 seg))
  
  (setq dist-to-center (sqrt (+ (expt (- px (car center)) 2)
                                (expt (- py (cadr center)) 2))))
  
  (if (> (abs (- dist-to-center radius)) tol)
    nil
    (progn
      (setq pt-ang (atan (- py (cadr center))
                         (- px (car center))))
      (angle-in-arc pt-ang start-ang end-ang ccw)
    )
  )
)


;;; Check if angle is within arc span
(defun angle-in-arc (ang start-ang end-ang ccw / 
                     a s e two-pi)
  
  (setq two-pi (* 2.0 pi))
  
  (setq a (rem (+ ang two-pi) two-pi))
  (setq s (rem (+ start-ang two-pi) two-pi))
  (setq e (rem (+ end-ang two-pi) two-pi))
  
  (if ccw
    (if (<= s e)
      (and (>= a (- s 0.001)) (<= a (+ e 0.001)))
      (or (>= a (- s 0.001)) (<= a (+ e 0.001)))
    )
    (if (>= s e)
      (and (<= a (+ s 0.001)) (>= a (- e 0.001)))
      (or (<= a (+ s 0.001)) (>= a (- e 0.001)))
    )
  )
)

(princ "\nLoaded: SBP (select blocks on polyline)")
(princ "\n        SBPC (clear selection)")
(princ)