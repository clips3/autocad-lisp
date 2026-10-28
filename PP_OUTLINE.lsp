;;; ==========================================================================
;;; OutlineObjectsV1-1.lsp
;;; ==========================================================================

(vl-load-com)

;;; ==========================================================================
;;; Main Command: c:outline
;;; ==========================================================================
(defun c:outline (/ *error* doc idx sel) 

  ;; Initialize ActiveX Objects
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))

  ;; Error Handler
  (defun *error* (msg) 
    (if (and msg (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))) 
      (princ (strcat "\nError: " msg))
    )
    (LM:endundo doc)
    (princ)
  )

  ;; Main Execution
  (if (setq sel (ssget)) 
    (progn 
      (LM:startundo doc)
      (LM:outline sel)
      (initget "Yes No")
      (if (/= "No" (getkword "\nErase original objects? [Yes/No] <Yes>: ")) 
        (repeat (setq idx (sslength sel)) 
          (entdel (ssname sel (setq idx (1- idx))))
        )
      )
      (LM:endundo doc)
    )
  )
  (princ)
)

;;; ==========================================================================
;;; Function: LM:outline
;;; Description: Generates a boundary outline for the selected objects.
;;; ==========================================================================
(defun LM:outline (sel / app are box cmd dis enl ent lst obj rtn tmp) 
  (if (setq box (LM:ssboundingbox sel)) 
    (progn 
      (setq app (vlax-get-acad-object)
            dis (/ (apply 'distance box) 20.0)
            lst (mapcar '(lambda (a o) (mapcar o a (list dis dis))) box '(- +))
            are (apply '* (apply 'mapcar (cons '- (reverse lst))))
            dis (* dis 1.5)
            ent (entmakex 
                  (append 
                    '((000 . "LWPOLYLINE")
                      (100 . "AcDbEntity")
                      (100 . "AcDbPolyline")
                      (090 . 4)
                      (070 . 1)
                     )
                    (mapcar 
                      '(lambda (x) 
                         (cons 10 (mapcar '(lambda (y) ((eval y) lst)) x))
                       )
                      '((caar cadar)
                        (caadr cadar)
                        (caadr cadadr)
                        (caar cadadr)
                       )
                    )
                  )
                )
      )
      (apply 'vlax-invoke 
             (vl-list* app 
                       'zoomwindow
                       (mapcar '(lambda (a o) (mapcar o a (list dis dis 0.0))) 
                               box
                               '(- +)
                       )
             )
      )
      (setq cmd (getvar 'cmdecho)
            enl (entlast)
            rtn (ssadd)
      )
      (while (setq tmp (entnext enl)) (setq enl tmp))
      (setvar 'cmdecho 0)
      (command 
        "_.-boundary"
        "_a"
        "_b"
        "_n"
        sel
        ent
        ""
        "_i"
        "_y"
        "_o"
        "_p"
        ""
        "_non"
        (trans (mapcar '- (car box) (list (/ dis 3.0) (/ dis 3.0))) 0 1)
        ""
      )
      (while (< 0 (getvar 'cmdactive)) (command ""))
      (entdel ent)
      (while (setq enl (entnext enl)) 
        (if 
          (and 
            (vlax-property-available-p (setq obj (vlax-ename->vla-object enl)) 
                                       'area
            )
            (equal (vla-get-area obj) are 1e-4)
          )
          (entdel enl)
          (ssadd enl rtn)
        )
      )
      (vla-zoomprevious app)
      (setvar 'cmdecho cmd)
      rtn
    )
  )
)

;;; ==========================================================================
;;; Helper Functions
;;; ==========================================================================

;;; LM:ssboundingbox - Returns the bounding box of a selection set
(defun LM:ssboundingbox (s / a b i m n o) 
  (repeat (setq i (sslength s)) 
    (if 
      (and 
        (setq o (vlax-ename->vla-object (ssname s (setq i (1- i)))))
        (vlax-method-applicable-p o 'getboundingbox)
        (not 
          (vl-catch-all-error-p 
            (vl-catch-all-apply 'vla-getboundingbox (list o 'a 'b))
          )
        )
      )
      (setq m (cons (vlax-safearray->list a) m)
            n (cons (vlax-safearray->list b) n)
      )
    )
  )
  (if (and m n) 
    (mapcar '(lambda (a b) (apply 'mapcar (cons a b))) '(min max) (list m n))
  )
)

;;; LM:startundo - Starts an Undo Mark
(defun LM:startundo (doc) 
  (LM:endundo doc)
  (vla-startundomark doc)
)

;;; LM:endundo - Ends an Undo Mark
(defun LM:endundo (doc) 
  (while (= 8 (logand 8 (getvar 'undoctl))) 
    (vla-endundomark doc)
  )
)

;;; LM:acdoc - Returns the Active Document (Memoized)
;;; Kept for compatibility if used elsewhere or if needed by other lisp files.
(defun LM:acdoc nil 
  (eval (list 'defun 
              'LM:acdoc
              'nil
              (vla-get-activedocument (vlax-get-acad-object))
        )
  )
  (LM:acdoc)
)

(princ)