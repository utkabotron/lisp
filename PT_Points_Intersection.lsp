;;; Поиск пересечений полилиний и создание точек в местах пересечений
;;; Обрабатывает 2D полилинии и создает точки в местах их пересечения
;;; Команда: PTSINT

;; AutoLISP: Points at intersections of 2D LWPOLYLINEs
;; Comments in English as requested.

(defun seg-intersect (p1 p2 q1 q2 tol / x1 y1 x2 y2 x3 y3 x4 y4 dx1 dy1 dx2 dy2 den t-param u-param)
  "Return intersection point of segments P1-P2 and Q1-Q2 or nil.
P1,P2,Q1,Q2 are (x y). tol is tolerance for denom zero."
  (setq x1 (car p1)   y1 (cadr p1)
        x2 (car p2)   y2 (cadr p2)
        x3 (car q1)   y3 (cadr q1)
        x4 (car q2)   y4 (cadr q2)
        dx1 (- x2 x1) dy1 (- y2 y1)
        dx2 (- x4 x3) dy2 (- y4 y3)
        den (- (* dx1 dy2) (* dy1 dx2))
  )
  (if (< (abs den) tol)
    nil
    (progn
      (setq t-param (/ (- (* (- x3 x1) dy2) (* (- y3 y1) dx2)) den))
      (setq u-param (/ (- (* (- x3 x1) dy1) (* (- y3 y1) dx1)) den))
      (if (and (>= t-param 0.0) (<= t-param 1.0) 
               (>= u-param 0.0) (<= u-param 1.0))
        (list (+ x1 (* t-param dx1)) (+ y1 (* t-param dy1)))
        nil
      )
    )
  )
)

(defun point-in-list-p (pt lst tol / found)
  "Check if point pt is within tol of any point in lst."
  (setq found nil)
  (foreach p lst
    (if (and (< (abs (- (car pt) (car p))) tol)
             (< (abs (- (cadr pt) (cadr p))) tol))
      (setq found T)
    )
  )
  found
)

(defun coords-to-verts (coords / res i coord-step)
  "Convert flat coordinates list to list of vertex points (x y).
Handles both 2D (step=2) and 3D (step=3) coordinate arrays."
  (setq res '())
  ;; Determine if coordinates are 2D or 3D based on list length
  ;; For 3 vertices: 6 values = 2D (x,y pairs), 9 values = 3D (x,y,z triplets)
  (if (= (rem (length coords) 3) 0)
    (setq coord-step 3)  ;; 3D coordinates (x, y, z)
    (setq coord-step 2)  ;; 2D coordinates (x, y)
  )
  
  (setq i 0)
  (while (< i (length coords))
    ;; Always extract only X and Y, skip Z if present
    (setq res (cons (list (nth i coords) (nth (1+ i) coords)) res))
    (setq i (+ i coord-step))
  )
  (reverse res)
)

(defun get-polyline-coords (obj / coords-raw coords)
  "Safely get coordinates from polyline object."
  (setq coords nil)
  (if (vlax-property-available-p obj 'Coordinates)
    (progn
      (setq coords-raw (vlax-get obj 'Coordinates))
      (if (= (type coords-raw) 'variant)
        (setq coords (vlax-safearray->list (vlax-variant-value coords-raw)))
        (if (= (type coords-raw) 'safearray)
          (setq coords (vlax-safearray->list coords-raw))
          (setq coords coords-raw)
        )
      )
    )
  )
  coords
)

(defun verts-to-segments (verts / segs k)
  "Convert vertex list to segment pairs."
  (setq segs '() k 0)
  (while (< k (1- (length verts)))
    (setq segs (cons (list (nth k verts) (nth (1+ k) verts)) segs))
    (setq k (1+ k))
  )
  (reverse segs)
)

(defun create-point (pt)
  "Create an AutoCAD POINT at pt (x y)."
  (entmake (list '(0 . "POINT") (cons 10 (list (car pt) (cadr pt) 0.0))))
)

(defun c:PTSINT ( / ss n i j obj1 coords1 verts1 segs1 segs2 s1 s2 p 
                    tol intersections polydata test-count)
  (setq tol 1e-6)
  
  ;; Select polylines
  (princ "\nSelect polylines: ")
  (setq ss (ssget (list (cons 0 "LWPOLYLINE,POLYLINE"))))
  
  (if (not ss)
    (progn 
      (princ "\nNo polylines selected.") 
      (princ)
    )
    (progn
      (setq n (sslength ss))
      (setq intersections '())
      (setq polydata '())
      (setq test-count 0)
      
      (princ (strcat "\nProcessing " (itoa n) " polylines..."))
      
      ;; First pass: extract all polyline data
      (setq i 0)
      (while (< i n)
        (setq obj1 (vlax-ename->vla-object (ssname ss i)))
        (if (vlax-property-available-p obj1 'Coordinates)
          (progn
            (setq coords1 (get-polyline-coords obj1))
            (if coords1
              (progn
                (setq verts1 (coords-to-verts coords1))
                (setq segs1 (verts-to-segments verts1))
                (if (> (length segs1) 0)
                  (setq polydata (cons segs1 polydata))
                )
              )
            )
          )
        )
        (setq i (1+ i))
      )
      
      (setq polydata (reverse polydata))
      (setq n (length polydata))
      
      (princ (strcat "\nExtracted " (itoa n) " valid polylines"))
      
      (if (< n 2)
        (progn
          (princ "\nNeed at least 2 valid polylines.")
          (princ)
        )
        (progn
          ;; Second pass: compare all pairs
          (setq i 0)
          (while (< i n)
            (setq segs1 (nth i polydata))
            (setq j (1+ i))
            (while (< j n)
              (setq segs2 (nth j polydata))
              
              ;; Test each segment pair
              (foreach s1 segs1
                (foreach s2 segs2
                  (setq test-count (1+ test-count))
                  (setq p (seg-intersect (car s1) (cadr s1) (car s2) (cadr s2) tol))
                  (if p
                    (if (not (point-in-list-p p intersections 1e-5))
                      (progn
                        (setq intersections (cons p intersections))
                        (create-point p)
                      )
                    )
                  )
                )
              )
              (setq j (1+ j))
            )
            (setq i (1+ i))
          )
          (princ (strcat "\nTested " (itoa test-count) " segment pairs"))
          (princ (strcat "\nCreated " (itoa (length intersections)) " intersection points."))
          (princ)
        )
      )
    )
  )
  (princ)
)

(princ "\nType PTSINT to find polyline intersections.")
(princ)