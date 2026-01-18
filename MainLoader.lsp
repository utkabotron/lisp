;;; Main AutoCAD LISP Loader
;;; Loads all utility scripts from current directory
;;; Just load this one file to get access to all commands

(vl-load-com)

;; Get the directory where this main file is located
(setq *main-lsp-path* (findfile "MainLoader.lsp"))
(if *main-lsp-path*
  (setq *main-lsp-dir* (vl-filename-directory *main-lsp-path*))
  (setq *main-lsp-dir* "c:/1")
)
;; Ensure directory ends with /
(if (not (wcmatch *main-lsp-dir* "*[/\\]"))
  (setq *main-lsp-dir* (strcat *main-lsp-dir* "/"))
)

;; List of all LISP files to load (in order of dependency)
;; Naming convention:
;; HK_ - Hotkeys (горячие клавиши)
;; BL_ - Block tools (работа с блоками)
;; AT_ - Attribute tools (атрибуты)
;; PT_ - Point tools (точки)
;; DV_ - Deviation tools (отклонения)
;; LY_ - Layer tools (слои)
;; UT_ - Utility tools (утилиты)

(setq *lsp-files* '(
  "HK_Hotkeys.lsp"
  "BL_Block_Tools.lsp"
  "BL_Copy_Attributes_Properties.lsp"
  "BL_Delete_Duplicate_Blocks.lsp"
  "BL_Select_Blocks_On_Line.lsp"
  "BL_Insert_Block_To_Points.lsp"
  "AT_Attribute_Tools.lsp"
  "AT_Attribute_Visibility.lsp"
  "PT_Point_Tools.lsp"
  "PT_Points_Intersection.lsp"
  "PT_Point_At_Block_Center.lsp"
  "DV_Deviation_Tools.lsp"
  "LY_Set_All_By_Layer.lsp"
  "LY_Xref_Layers.lsp"
  "UT_Export_To_CSV.lsp"
))

;; Function to load individual LISP file
(defun load-lsp-file (filename / filepath)
  (setq filepath (strcat *main-lsp-dir* filename))
  (if (findfile filepath)
    (progn
      (princ (strcat "\nLoading " filename "..."))
      (load filepath)
      (princ " OK")
    )
    (princ (strcat "\nWarning: " filename " not found!"))
  )
)

;; Load all LISP files
(defun c:LoadAllLSP ()
  (foreach file *lsp-files*
    (load-lsp-file file)
  )
)

;; Auto-load all utilities on this file load
(c:LoadAllLSP)
(princ "\n===BRICK LISP LOADED===")
