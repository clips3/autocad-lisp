;; Arc2seg.lsp
;; Converts Arcs, Circles, and Polyline Arcs into straight line segments.
;; Based on ARC2SEG v1.03 by Gilles (gile)
;; Organized for CAD standards.

(vl-load-com)

;;----------------------------------------------------------------------------;;
;; Global Helper Functions
;;----------------------------------------------------------------------------;;

;; BulgeData: Returns (angle radius center) from bulge, p1, and p2
(defun BulgeData (bu p1 p2 / alpha rad cen) 
  (setq alpha (* 2 (atan bu))
        rad   (/ (distance p1 p2) (* 2 (sin alpha)))
        cen   (polar p1 (+ (angle p1 p2) (- (/ pi 2) alpha)) rad)
  )
  (list (* alpha 2.0) rad cen)
)

;; Ang<2pi: Normalizes an angle to [0, 2pi)
(defun Ang<2pi (ang) 
  (if (and (<= 0 ang) (< ang (* 2 pi))) 
    ang
    (Ang<2pi (rem (+ ang (* 2 pi)) (* 2 pi)))
  )
)

;;----------------------------------------------------------------------------;;
;; Main Command: ARC2SEG
;;----------------------------------------------------------------------------;;
(defun c:Arc2Seg (/ *error* doc spc oldEcho oldOsMode Arc2Pol Pol2Pol seg del org ss 
                  n ent elst
                 ) 

  ;; 1. Setup ActiveX Objects
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq spc (if (= (getvar "CVPORT") 1) 
              (vla-get-PaperSpace doc)
              (vla-get-ModelSpace doc)
            )
  )

  ;; 2. Error Handler
  (defun *error* (msg) 
    (if (and msg (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*"))) 
      (princ (strcat "\nError: " msg))
    )
    (if oldEcho (setvar "CMDECHO" oldEcho))
    (if oldOsMode (setvar "OSMODE" oldOsMode))
    (vla-EndUndoMark doc)
    (princ)
  )

  ;; 3. Environment Setup
  (vla-StartUndoMark doc)
  (setq oldEcho   (getvar "CMDECHO")
        oldOsMode (getvar "OSMODE")
  )
  (setvar "CMDECHO" 0)

  ;;--------------------------------------------------------------------------;;
  ;; Local Helper Functions
  ;;--------------------------------------------------------------------------;;

  ;; Arc2Pol: Returns DXF list for a new polyline from an Arc or Circle
  (defun Arc2Pol (elst seg org / closed alpha delta cen elv rad lay nlst) 
    (and (= (cdr (assoc 0 elst)) "CIRCLE") (setq closed T))
    (setq alpha (if closed (* pi 2) (cdr (assoc 51 elst)))
          delta (if closed 
                  (/ alpha seg)
                  (/ (Ang<2pi (- alpha (cdr (assoc 50 elst)))) seg)
                )
          cen   (cdr (assoc 10 elst))
          elv   (caddr cen)
          cen   (list (car cen) (cadr cen))
          rad   (cdr (assoc 40 elst))
          lay   (if org (assoc 8 elst) (cons 8 (getvar "CLAYER")))
          nlst  (vl-remove-if-not 
                  (function (lambda (x) (member (car x) '(210 -3))))
                  elst
                )
          nlst  (cons (cons 10 (polar cen alpha rad)) nlst)
    )
    (repeat (if closed (1- seg) seg) 
      (setq nlst (cons (cons 10 (polar cen (setq alpha (- alpha delta)) rad)) nlst))
    )
    (setq nlst (cons '(0 . "LWPOLYLINE") 
                     (cons '(100 . "AcDbEntity") 
                           (cons (cons 410 (getvar "CTAB")) 
                                 (cons lay 
                                       (cons '(100 . "AcDbPolyline") 
                                             (cons 
                                               (cons 90 (if closed seg (1+ seg)))
                                               (cons (cons 70 (if closed 1 0)) 
                                                     (cons (cons 38 elv) nlst)
                                               )
                                             )
                                       )
                                 )
                           )
                     )
               )
    )
  ) ;; end Arc2Pol

  ;; Pol2Pol: Returns DXF list for a modified polyline
  (defun Pol2Pol (elst seg org / cnt closed nlst p0 p1 p2 bu larg inc bdata delta cen 
                  rad alpha n
                 ) 
    (setq closed (logand 1 (cdr (assoc 70 elst)))
          cnt    0
    )
    (and (= closed 1) (setq p0 (cdr (assoc 10 elst))))
    (while elst 
      (if (= (caar elst) 10) 
        (progn 
          (setq p1 (cdar elst)
                p2 (cdr (assoc 10 (cdr elst)))
                bu (cdr (assoc 42 elst))
          )
          (if (or (= 0 bu) (and (zerop closed) (null p2))) 
            (setq nlst (cons (cadddr elst) 
                             (cons (caddr elst) 
                                   (cons (cadr elst) (cons (car elst) nlst))
                             )
                       )
                  elst (cddddr elst)
            )
            (progn 
              (and (not p2) (= closed 1) (setq p2 p0))
              (setq larg  (cdr (assoc 40 elst))
                    inc   (/ (- (cdr (assoc 41 elst)) larg) seg)
                    bdata (BulgeData bu p1 p2)
                    delta (/ (car bdata) seg)
                    rad   (abs (cadr bdata))
                    cen   (caddr bdata)
                    alpha (angle cen p1)
                    n     0
                    cnt   (+ cnt seg -1)
              )
              (while (< n seg) 
                (setq nlst (cons (cons 10 (polar cen (+ alpha (* delta n)) rad)) 
                                 nlst
                           )
                      nlst (cons (cons 40 larg) nlst)
                      nlst (cons (cons 41 (setq larg (+ larg inc))) nlst)
                      nlst (cons '(42 . 0.0) nlst)
                      n    (1+ n)
                )
              )
              (setq elst (cddddr elst))
            )
          )
        )
        (setq nlst (cons (car elst) nlst)
              elst (cdr elst)
        )
      )
    )
    (if (not org) 
      (setq nlst (subst (cons 8 (getvar "CLAYER")) (assoc 8 nlst) nlst))
    )
    ((lambda (dxf90) 
       (subst (cons 90 (+ (cdr dxf90) cnt)) 
              dxf90
              (reverse (subst '(42 . 0.0) (assoc 42 nlst) nlst))
       )
     ) 
      (assoc 90 nlst)
    )
  ) ;; end Pol2Pol

  ;;--------------------------------------------------------------------------;;
  ;; Core Logic
  ;;--------------------------------------------------------------------------;;

  ;; 4. Get User Parameters
  (or (getenv "SegmentsNumberPerCircle") (setenv "SegmentsNumberPerCircle" "64"))
  (initget 6)
  (setq seg (getint 
              (strcat "\nNumber of Segments per Arc <" 
                      (getenv "SegmentsNumberPerCircle")
                      ">: "
              )
            )
  )
  (if seg 
    (setenv "SegmentsNumberPerCircle" (itoa seg))
    (setq seg (atoi (getenv "SegmentsNumberPerCircle")))
  )

  (setq del T) ; Default: Erase Source Objects
  (setq org T) ; Default: Use Original Layer

  ;; 5. Selection and Processing
  (princ "\nSelect Arcs, Circles or Polylines to segment...")
  (and 
    (or (setq ss (ssget '((0 . "ARC,CIRCLE,LWPOLYLINE")))) 
        (setq ss (ssget "_X" '((0 . "ARC,CIRCLE,LWPOLYLINE"))))
    )
    (setq n 0)
    (while (setq ent (ssname ss n)) 
      (setq elst (entget ent '("*")))
      (if (= (cdr (assoc 0 elst)) "LWPOLYLINE") 
        ((if del entmod entmake) (Pol2Pol elst seg org))
        (progn 
          (entmake (Arc2Pol elst seg org))
          (and del (entdel ent))
        )
      )
      (setq n (1+ n))
    )
  )

  ;; 6. Clean Exit
  (*error* nil)
  (princ)
) ;; end c:Arc2Seg

(princ "\nArc2Seg loaded. Type ARC2SEG to run.")
(princ)
