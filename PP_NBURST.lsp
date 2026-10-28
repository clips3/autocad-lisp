(vl-load-com)

;;----------------------------------------------------------------------;;
;;                         1. Utility Functions                         ;;
;;----------------------------------------------------------------------;;

;; Active Document - Returns the VLA Active Document Object
(defun LM:acdoc nil 
  (eval (list 'defun 
              'LM:acdoc
              'nil
              (vla-get-activedocument (vlax-get-acad-object))
        )
  )
  (LM:acdoc)
)

;; entlast - Wrapper to return the last subentity in the database
(defun LM:entlast (/ ent tmp) 
  (setq ent (entlast))
  (while (setq tmp (entnext ent)) (setq ent tmp))
  ent
)

;; ssget - Wrapper for ssget with custom selection prompt
(defun LM:ssget (msg arg / sel) 
  (princ msg)
  (setvar 'nomutt 1)
  (setq sel (vl-catch-all-apply 'ssget arg))
  (setvar 'nomutt 0)
  (if (not (vl-catch-all-error-p sel)) sel)
)

;; Uniformly Scaled Block - Returns T if uniformly scaled
(defun LM:usblock-p (obj / s) 
  (if (vlax-property-available-p obj 'xeffectivescalefactor) 
    (setq s "effectivescalefactor")
    (setq s "scalefactor")
  )
  (eval 
    (list 'defun 
          'LM:usblock-p
          '(obj)
          (list 'and 
                (list 'equal 
                      (list 'abs (list 'vlax-get-property 'obj (strcat "x" s)))
                      (list 'abs (list 'vlax-get-property 'obj (strcat "y" s)))
                      1e-8
                )
                (list 'equal 
                      (list 'abs (list 'vlax-get-property 'obj (strcat "x" s)))
                      (list 'abs (list 'vlax-get-property 'obj (strcat "z" s)))
                      1e-8
                )
          )
    )
  )
  (LM:usblock-p obj)
)

;;----------------------------------------------------------------------;;
;;                    2. Attribute Conversion Helpers                    ;;
;;----------------------------------------------------------------------;;

;; Remove all pairs with matching DXF group codes
(defun LM:burst:removepairs (itm lst) 
  (vl-remove-if '(lambda (x) (member (car x) itm)) lst)
)

;; Remove only the first occurrence of each matching DXF group code
(defun LM:burst:remove1stpairs (itm lst) 
  (vl-remove-if 
    '(lambda (x) 
       (if (member (car x) itm) (progn (setq itm (vl-remove (car x) itm)) t))
     )
    lst
  )
)

;; Convert single-line attribute to TEXT entity
(defun LM:burst:att2text (enx) 
  (entmakex 
    (append '((0 . "TEXT")) 
            (LM:burst:removepairs 
              '(000 002 003 070 074 100 280 440)
              (subst (cons 73 (cdr (assoc 74 enx))) (assoc 74 enx) enx)
            )
    )
  )
)

;; Convert multi-line attribute to MTEXT entity
(defun LM:burst:matt2mtext (enx) 
  (entmakex 
    (append '((0 . "MTEXT") (100 . "AcDbEntity") (100 . "AcDbMText")) 
            (LM:burst:remove1stpairs 
              (if (= "ATTDEF" (cdr (assoc 0 enx))) 
                '(001 003 007 010 040 041 050 071 072 073 210)
                '(001 007 010 040 041 050 071 072 073 210)
              )
              (LM:burst:removepairs 
                '(000 002 011 042 043 051 070 074 100 101 102 280 330 360 440)
                enx
              )
            )
            (list (assoc 011 (reverse enx)))
    )
  )
)

;;----------------------------------------------------------------------;;
;;                      3. Core Burst Functions                         ;;
;;----------------------------------------------------------------------;;

;; Process a selection set of blocks to burst
(defun LM:burstsel (sel nst / idx) 
  (if (= 'pickset (type sel)) 
    (repeat (setq idx (sslength sel)) 
      (LM:burstobject 
        (vlax-ename->vla-object (ssname sel (setq idx (1- idx))))
        nst
      )
    )
  )
)

;; Burst a single block reference object
(defun LM:burstobject (obj nst / cmd col ent err lay lin lst qaf tmp) 
  (if 
    (and 
      (= "AcDbBlockReference" (vla-get-objectname obj))
      (not (vlax-property-available-p obj 'path))
      (vlax-write-enabled-p obj)
      (or 
        (and (LM:usblock-p obj) 
             (not 
               (vl-catch-all-error-p 
                 (setq err (vl-catch-all-apply 'vlax-invoke (list obj 'explode)))
               )
             )
             (setq lst err)
        )
        (progn 
          (setq tmp (vla-copy obj)
                ent (LM:entlast)
                cmd (getvar 'cmdecho)
                qaf (getvar 'qaflags)
          )
          (setvar 'cmdecho 0)
          (setvar 'qaflags 0)
          (vl-cmdf "_.explode" (vlax-vla-object->ename tmp))
          (setvar 'qaflags qaf)
          (setvar 'cmdecho cmd)
          (while (setq ent (entnext ent)) 
            (setq lst (cons (vlax-ename->vla-object ent) lst))
          )
          lst
        )
      )
    )
    (progn 
      (setq lay (vla-get-layer obj)
            col (vla-get-color obj)
            lin (vla-get-linetype obj)
      )
      (foreach att (vlax-invoke obj 'getattributes) 
        (if (vlax-write-enabled-p att) 
          (progn 
            (if (= "0" (vla-get-layer att)) 
              (vla-put-layer att lay)
            )
            (if (= acbyblock (vla-get-color att)) 
              (vla-put-color att col)
            )
            (if (= "byblock" (strcase (vla-get-linetype att) t)) 
              (vla-put-linetype att lin)
            )
          )
        )
        (if 
          (and 
            (= :vlax-false (vla-get-invisible att))
            (= :vlax-true (vla-get-visible att))
          )
          ((if 
             (and (vlax-property-available-p att 'mtextattribute) 
                  (= :vlax-true (vla-get-mtextattribute att))
             )
             LM:burst:matt2mtext
             LM:burst:att2text
           ) 
            (entget (vlax-vla-object->ename att))
          )
        )
      )
      (foreach new lst 
        (cond 
          ((not (vlax-write-enabled-p new)))
          ((= :vlax-false (vla-get-visible new))
           (vla-delete new)
          )
          (t
           (if (= "0" (vla-get-layer new)) 
             (vla-put-layer new lay)
           )
           (if (= acbyblock (vla-get-color new)) 
             (vla-put-color new col)
           )
           (if (= "byblock" (strcase (vla-get-linetype new) t)) 
             (vla-put-linetype new lin)
           )
           (if (= "AcDbAttributeDefinition" (vla-get-objectname new)) 
             (progn 
               (if 
                 (and 
                   (= :vlax-true (vla-get-constant new))
                   (= :vlax-false (vla-get-invisible new))
                 )
                 ((if 
                    (and (vlax-property-available-p new 'mtextattribute) 
                         (= :vlax-true (vla-get-mtextattribute new))
                    )
                    LM:burst:matt2mtext
                    LM:burst:att2text
                  ) 
                   (entget (vlax-vla-object->ename new))
                 )
               )
               (vla-delete new)
             )
             (if nst (LM:burstobject new nst))
           )
          )
        )
      )
      (vla-delete obj)
    )
  )
)

;;----------------------------------------------------------------------;;
;;                    4. Main Command & Entry Point                     ;;
;;----------------------------------------------------------------------;;

;; Main burst orchestrator
(defun LM:burst (nst / *error* doc oldEcho oldQa) 

  (setq doc (LM:acdoc))

  (defun *error* (msg) 
    (if oldEcho (setvar 'cmdecho oldEcho))
    (if oldQa (setvar 'qaflags oldQa))
    (while (= 8 (logand 8 (getvar 'undoctl))) 
      (vla-endundomark doc)
    )
    (if (and msg (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))) 
      (princ (strcat "\nError: " msg))
    )
    (princ)
  )

  (vla-startundomark doc)
  (setq oldEcho (getvar 'cmdecho)
        oldQa   (getvar 'qaflags)
  )

  (LM:burstsel 
    (LM:ssget 
      "\nSelect blocks to burst: "
      (list "_:L" 
            (append '((0 . "INSERT")) 
                    ((lambda (/ def lst) 
                       (while (setq def (tblnext "block" (null def))) 
                         (if (= 4 (logand 4 (cdr (assoc 70 def)))) 
                           (setq lst (vl-list* "," (cdr (assoc 2 def)) lst))
                         )
                       )
                       (if lst 
                         (list '(-4 . "<NOT") 
                               (cons 2 (apply 'strcat (cdr lst)))
                               '(-4 . "NOT>")
                         )
                       )
                     ) 
                    )
                    (if (= 1 (getvar 'cvport)) 
                      (list (cons 410 (getvar 'ctab)))
                      '((410 . "Model"))
                    )
            )
      )
    )
    nst
  )

  (*error* nil)
)

;; Command aliases
(defun c:pburst nil (LM:burst nil) (princ))
(defun c:nburst nil (LM:burst t) (princ))

;;----------------------------------------------------------------------;;

(princ 
  (strcat 
    "\n:: BurstUpgraded.lsp | Version 1.7 | \U+00A9 Lee Mac "
    (menucmd "m=$(edtime,0,yyyy)")
    " www.lee-mac.com ::"
    "\n:: \"pburst\" to burst primary  |  \"nburst\" to burst primary + nested ::"
  )
)
(princ)