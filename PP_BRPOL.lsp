(defun c:QB (/ *error* doc oldOsMode oldEcho ss ssLen i ent obj ptlst idx endparam) 
  (vl-load-com)

  ;; 1. Setup ActiveX Objects
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))

  ;; 2. Define Error Handler
  (defun *error* (msg) 
    (if (and msg (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*"))) 
      (princ (strcat "\nError: " msg))
    )
    (if oldOsMode (setvar "OSMODE" oldOsMode))
    (if oldEcho (setvar "CMDECHO" oldEcho))
    (vla-EndUndoMark doc)
    (princ)
  )

  ;; 3. Main Execution Setup
  (vla-StartUndoMark doc)
  (setq oldOsMode (getvar "OSMODE"))
  (setq oldEcho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (setvar "OSMODE" 0)

  ;; 4. Get selection: pre-selected objects OR prompt user
  ;; Try to get the current selection first (pickfirst / implied selection)
  (setq ss (ssget "_I" '((0 . "LWPOLYLINE"))))
  (if (not ss) 
    ;; If no pre-selection, prompt user to select
    (setq ss (ssget '((0 . "LWPOLYLINE"))))
  )

  (if (not ss) 
    (princ "\nNenhuma polyline selecionada.")
    (progn 
      (setq ssLen (sslength ss))
      (setq i 0)
      (while (< i ssLen) 
        (setq ent (ssname ss i))
        (setq obj (vlax-ename->vla-object ent))
        ;; Build point list for this polyline
        (setq ptlst (list (vlax-curve-getStartPoint obj))
              idx   1
        )
        (if (zerop (vlax-get obj 'Closed)) 
          (setq endparam (vlax-curve-getEndParam obj))
          (setq endparam (fix (vlax-curve-getEndParam obj)))
        )
        (while (<= idx endparam) 
          (setq ptlst (cons (vlax-curve-getPointAtParam obj idx) ptlst)
                idx   (1+ idx)
          )
        )
        ;; BREAK at each vertex
        (mapcar 
          (function 
            (lambda (x) 
              (vl-cmdf "_.break" ent "_non" x "_non" x)
            )
          )
          ptlst
        )
        (setq i (1+ i))
      )
      (princ (strcat "\n" (itoa ssLen) " polyline(s) processada(s)."))
    )
  )

  ;; 5. Clean Exit
  (*error* nil)
  (princ)
)