;;; Инструменты для расчета отклонений от линий и окружностей
;;; Команды: SetDeltaDAL, SetDeltaDAL_CIRCLE, SetDelta_Point

;;; SetDeltaDAL - Установка отклонений от линии в атрибут DELTA
(defun c:SetDeltaDAL ( / ssBlocks nBlocks eBlock vBlock insPt eLine pNearest2D dDist2D vAtt tagFound i valStr)

  ;; Returns a copy of point with Z=0 (for XY-plane calculations)
  (defun xy-only (pt) (list (car pt) (cadr pt) 0.0))

  ;; Always format number with exactly two decimals (e.g., 0 -> 0.00)
  (defun fmt2 (x / s p nd)
    ;; Convert number to string with 2 decimals
    (setq s (rtos x 2 2))
    ;; Ensure decimal point and 2 digits after it
    (if (null (vl-string-search "." s))
      (setq s (strcat s ".00"))
      (progn
        (setq p  (vl-string-search "." s))          ;; position of dot
        (setq nd (- (strlen s) p 1))                ;; digits after dot
        (cond
          ((= nd 0) (setq s (strcat s "00")))
          ((= nd 1) (setq s (strcat s "0")))
        )
      )
    )
    s
  )

  (princ "\nSelect blocks: ")
  (setq ssBlocks (ssget '((0 . "INSERT"))))
  (if (null ssBlocks) (progn (princ "\nNo blocks selected!") (exit)))

  (princ "\nSelect line, polyline, arc, spline, or circle: ")
  (setq eLine (car (entsel)))
  (if (null eLine) (progn (princ "\nNo line selected!") (exit)))

  (setq nBlocks (sslength ssBlocks))
  (setq i 0)

  (repeat nBlocks
    (setq eBlock (ssname ssBlocks i))
    (setq vBlock (vlax-ename->vla-object eBlock))
    (setq insPt (vlax-get vBlock 'InsertionPoint))

    ;; Find the closest point on the curve to the XY-projected insertion point
    (setq pNearest2D (vlax-curve-getClosestPointTo eLine (xy-only insPt)))

    ;; Calculate the distance in XY only (in meters)
    (setq dDist2D (distance (xy-only insPt) (xy-only pNearest2D)))

    ;; Format as Δ⊥=0.00 (always two decimals)
    (setq valStr (strcat "\U+0394\U+22A5=" (fmt2 dDist2D)))

    (setq tagFound nil)
    (foreach vAtt (vlax-invoke vBlock 'GetAttributes)
      (if (= (strcase (vla-get-tagstring vAtt)) "DELTA")
        (progn
          (vla-put-textstring vAtt valStr)
          (setq tagFound T)
        )
      )
    )

    (if tagFound
      (princ (strcat "\nBlock at " (rtos (car insPt) 2 2) "," (rtos (cadr insPt) 2 2) ": DELTA set to " valStr))
      (princ "\nBlock does not have a DELTA attribute!")
    )

    (setq i (1+ i))
  )

  (princ)
)

;;; SetDeltaDAL_CIRCLE - Установка отклонений от окружности в атрибут DELTA
(defun c:SetDeltaDAL_CIRCLE ( / ssBlocks nBlocks eBlock vBlock insPt eCirc oCirc cen rad dPC off side mag vAtt tagFound i valStr)

  ;; Keep XY only (zero Z)
  (defun xy-only (pt) (list (car pt) (cadr pt) 0.0))

  ;; Format number with exactly two decimals (e.g., 0 -> 0.00)
  (defun fmt2 (x / s p nd)
    (setq s (rtos x 2 2))
    (if (null (vl-string-search "." s))
      (setq s (strcat s ".00"))
      (progn
        (setq p  (vl-string-search "." s))
        (setq nd (- (strlen s) p 1))
        (cond
          ((= nd 0) (setq s (strcat s "00")))
          ((= nd 1) (setq s (strcat s "0")))
        )
      )
    )
    s
  )

  ;; Absolute value
  (defun abs1 (x) (if (< x 0.0) (- x) x))

  ;; Tolerance for "on circle"
  (setq tol 1e-6)

  (princ "\nSelect blocks: ")
  (setq ssBlocks (ssget '((0 . "INSERT"))))
  (if (null ssBlocks) (progn (princ "\nNo blocks selected!") (exit)))

  (princ "\nSelect a circle as the axis: ")
  (setq eCirc (entsel))
  (if (null eCirc) (progn (princ "\nNothing selected.") (exit)))
  (if (/= (cdr (assoc 0 (entget (car eCirc)))) "CIRCLE")
    (progn (princ "\nSelected entity is not a circle.") (exit))
  )

  (setq oCirc (vlax-ename->vla-object (car eCirc)))
  (setq cen  (xy-only (vlax-get oCirc 'Center)))
  (setq rad  (vlax-get oCirc 'Radius))

  (setq nBlocks (sslength ssBlocks))
  (setq i 0)

  (repeat nBlocks
    (setq eBlock (ssname ssBlocks i))
    (setq vBlock (vlax-ename->vla-object eBlock))
    (setq insPt (xy-only (vlax-get vBlock 'InsertionPoint)))

    ;; Distance from point to circle center in XY
    (setq dPC (distance insPt cen))

    ;; Signed offset: outside => positive, inside => negative
    (setq off (- dPC rad))

    ;; Build text:
    ;; inside  -> "-" + |off|
    ;; outside -> "+" + |off|
    ;; on circle (|off|<tol) -> no sign, value 0.00
    (cond
      ((< (abs1 off) tol)
       (setq valStr (strcat "\U+0394\U+22A5=" (fmt2 0.0)))
      )
      ((> off 0.0)
       (setq valStr (strcat "\U+0394\U+22A5=+" (fmt2 (abs1 off))))
      )
      (T
       (setq valStr (strcat "\U+0394\U+22A5=-" (fmt2 (abs1 off))))
      )
    )

    ;; Write to attribute DELTA
    (setq tagFound nil)
    (foreach vAtt (vlax-invoke vBlock 'GetAttributes)
      (if (= (strcase (vla-get-tagstring vAtt)) "DELTA")
        (progn
          (vla-put-textstring vAtt valStr)
          (setq tagFound T)
        )
      )
    )

    (if tagFound
      (princ (strcat "\nDELTA set to " valStr))
      (princ "\nBlock does not have a DELTA attribute!")
    )

    (setq i (1+ i))
  )

  (princ)
)

;;; SetDelta_Point - Расчет отклонений X,Y от опорной точки
(defun c:SetDelta_Point ( / ssBlocks nBlocks eBlock vBlock insPt ePoint vPoint deltaX deltaY tagX tagY i attLst attObj)
  (princ "\nSelect blocks: ")
  (setq ssBlocks (ssget '((0 . "INSERT")))) ; select only blocks
  (if (null ssBlocks)
    (progn (princ "\nNo blocks selected!") (exit))
  )
  (princ "\nSelect reference point: ")
  (setq ePoint (car (entsel "\nSelect a POINT entity: ")))
  (if (null ePoint)
    (progn (princ "\nNo point selected!") (exit))
  )
  (setq vPoint (cdr (assoc 10 (entget ePoint)))) ; get point coordinates

  (setq nBlocks (sslength ssBlocks))
  (setq i 0)
  (repeat nBlocks
    (setq eBlock (ssname ssBlocks i))
    (setq vBlock (vlax-ename->vla-object eBlock))
    (setq insPt (vlax-get vBlock 'InsertionPoint))
    (setq deltaX (- (car insPt) (car vPoint)))
    (setq deltaY (- (cadr insPt) (cadr vPoint)))
    ; Write value as ΔX=1.05cm, ΔY=0.14cm
    (setq attLst (vlax-invoke vBlock 'GetAttributes))
    (setq tagX nil tagY nil)
    (foreach attObj attLst
      (cond
        ((= (strcase (vla-get-TagString attObj)) "DELTA_X")
          (vla-put-TextString attObj (strcat "\\U+0394E=" (rtos deltaX 2 2)))
          (setq tagX T)
        )
        ((= (strcase (vla-get-TagString attObj)) "DELTA_Y")
          (vla-put-TextString attObj (strcat "\\U+0394N=" (rtos deltaY 2 2)))
          (setq tagY T)
        )
      )
    )
    (if (not tagX) (princ "\nDELTA_X attribute not found in block."))
    (if (not tagY) (princ "\nDELTA_Y attribute not found in block."))
    (setq i (1+ i))
  )
  (princ "\nDeviation calculation complete.")
  (princ)
)

(princ "\nDeviation tools loaded: SetDeltaDAL, SetDeltaDAL_CIRCLE, SetDelta_Point")
(princ)
