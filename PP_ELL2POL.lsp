;; PP_ELL2POL.lsp
;; Converts ellipses and elliptical arcs to polylines.

(vl-load-com)

;;----------------------------------------------------------------------------;;
;; Global Helper Functions
;;----------------------------------------------------------------------------;;

;; Ang<2pi: Normalizes an angle to [0, 2pi)
(defun ang<2pi (ang) 
  (if (and (<= 0 ang) (< ang (* 2 pi))) 
    ang
    (ang<2pi (rem (+ ang (* 2 pi)) (* 2 pi)))
  )
)

;; 3dTo2dPt: Returns 2D (x y) part of 3D point
(defun 3dTo2dPt (pt) (list (car pt) (cadr pt)))

;; Tan: Returns tangent of an angle
(defun tan (a) (/ (sin a) (cos a)))

;; sublist: Returns a sub-list
(defun sublist (lst start leng / n r) 
  (if (or (not leng) (< (- (length lst) start) leng)) 
    (setq leng (- (length lst) start))
  )
  (setq n (+ start leng))
  (while (< start n) 
    (setq r (cons (nth (setq n (1- n)) lst) r))
  )
  r
)

;; k*bulge: Returns proportional bulge
(defun k*bulge (b k / a) 
  (setq a (atan B))
  (/ (sin (* k a)) (cos (* k a)))
)

;;----------------------------------------------------------------------------;;
;; Core Conversion Function
;;----------------------------------------------------------------------------;;

;; EllipseToPolyline: Converts a VLA-Ellipse to VLA-Polyline
(defun EllipseToPolyline (el / doc spc cl norm cen elv pt0 pt1 pt2 pt3 pt4 ac0 ac4 
                          a04 a02 a24 bsc1 bsc2 bsc3 bsc4 plst blst spt spa fspa srat 
                          ept epa fepa erat n pl ang
                         ) 
  (setq doc  (vla-get-ActiveDocument (vlax-get-acad-object))
        spc  (if (= 1 (getvar 'cvport)) 
               (vla-get-PaperSpace doc)
               (vla-get-ModelSpace doc)
             )
        cl   (and (= (vla-get-StartAngle el) 0.0) 
                  (= (vla-get-EndAngle el) (* 2 pi))
             )
        norm (vlax-get el 'Normal)
        cen  (trans (vlax-get el 'Center) 0 norm)
        elv  (caddr cen)
        cen  (3dTo2dPt cen)
        pt0  (mapcar '+ (trans (vlax-get el 'MajorAxis) 0 norm) cen)
        ac0  (angle cen pt0)
        pt4  (mapcar '+ cen (trans (vlax-get el 'MinorAxis) 0 norm))
        pt2  (3dTo2dPt (trans (vlax-curve-getPointAtparam el (/ pi 4.)) 0 norm))
        ac4  (angle cen pt4)
        a04  (angle pt0 pt4)
        a02  (angle pt0 pt2)
        a24  (angle pt2 pt4)
        bsc1 (/ (ang<2pi (- a02 ac4)) 2.)
        bsc2 (/ (ang<2pi (- a04 a02)) 2.)
        bsc3 (/ (ang<2pi (- a24 a04)) 2.)
        bsc4 (/ (ang<2pi (- (+ ac0 pi) a24)) 2.)
        pt1  (inters pt0 
                     (polar pt0 (+ ac0 (/ pi 2.) bsc1) 1.)
                     pt2
                     (polar pt2 (+ a02 bsc2) 1.)
                     nil
             )
        pt3  (inters pt2 
                     (polar pt2 (+ a04 bsc3) 1.)
                     pt4
                     (polar pt4 (+ a24 bsc4) 1.)
                     nil
             )
        plst (list pt4 pt3 pt2 pt1 pt0)
        blst (mapcar '(lambda (B) (tan (/ b 2.))) 
                     (list bsc4 bsc3 bsc2 bsc1)
             )
  )
  (repeat 2 
    (foreach b blst 
      (setq blst (cons b blst))
    )
  )
  (foreach p (cdr plst) 
    (setq ang  (angle cen p)
          plst (cons 
                 (polar cen (+ ang (* 2 (- ac4 ang))) (distance cen p))
                 plst
               )
    )
  )
  (foreach p (cdr plst) 
    (setq ang  (angle cen p)
          plst (cons 
                 (polar cen (+ ang (* 2 (- ac0 ang))) (distance cen p))
                 plst
               )
    )
  )
  (setq pl (vlax-invoke 
             spc
             'AddLightWeightPolyline
             (apply 'append 
                    (setq plst (reverse 
                                 (if cl 
                                   (cdr plst)
                                   plst
                                 )
                               )
                    )
             )
           )
  )
  (vlax-put pl 'Normal norm)
  (vla-put-Elevation pl elv)
  (mapcar '(lambda (i v) (vla-SetBulge pl i v)) 
          '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)
          blst
  )
  (if cl 
    (vla-put-Closed pl :vlax-true)
    (progn 
      (setq spt  (vlax-curve-getClosestPointTo pl (vlax-get el 'Startpoint))
            spa  (vlax-curve-getParamAtPoint pl spt)
            fspa (fix spa)
            ept  (vlax-curve-getClosestPointTo pl (vlax-get el 'Endpoint))
            epa  (vlax-curve-getParamAtPoint pl ept)
            fepa (fix epa)
            n    0
      )
      (cond 
        ((equal spt (trans pt0 norm 0) 1e-9)
         (if (= epa fepa) 
           (setq plst (sublist plst 0 (1+ fepa))
                 blst (sublist blst 0 (1+ fepa))
           )
           (setq erat (/ 
                        (- (vlax-curve-getDistAtParam pl epa) 
                           (vlax-curve-getDistAtParam pl fepa)
                        )
                        (- (vlax-curve-getDistAtParam pl (1+ fepa)) 
                           (vlax-curve-getDistAtParam pl fepa)
                        )
                      )
                 plst (append (sublist plst 0 (1+ fepa)) 
                              (list (3dTo2dPt (trans ept 0 norm)))
                      )
                 blst (append (sublist blst 0 (1+ fepa)) 
                              (list (k*bulge (nth fepa blst) erat))
                      )
           )
         )
        )
        ((equal ept (trans pt0 norm 0) 1e-9)
         (if (= spa fspa) 
           (setq plst (sublist plst fspa nil)
                 blst (sublist blst fspa nil)
           )
           (setq srat (/ 
                        (- (vlax-curve-getDistAtParam pl (1+ fspa)) 
                           (vlax-curve-getDistAtParam pl spa)
                        )
                        (- (vlax-curve-getDistAtParam pl (1+ fspa)) 
                           (vlax-curve-getDistAtParam pl fspa)
                        )
                      )
                 plst (cons (3dTo2dPt (trans spt 0 norm)) 
                            (sublist plst (1+ fspa) nil)
                      )
                 blst (cons (k*bulge (nth fspa blst) srat) 
                            (sublist blst (1+ fspa) nil)
                      )
           )
         )
        )
        (T
         (setq srat (/ 
                      (- (vlax-curve-getDistAtParam pl (1+ fspa)) 
                         (vlax-curve-getDistAtParam pl spa)
                      )
                      (- (vlax-curve-getDistAtParam pl (1+ fspa)) 
                         (vlax-curve-getDistAtParam pl fspa)
                      )
                    )
               erat (/ 
                      (- (vlax-curve-getDistAtParam pl epa) 
                         (vlax-curve-getDistAtParam pl fepa)
                      )
                      (- (vlax-curve-getDistAtParam pl (1+ fepa)) 
                         (vlax-curve-getDistAtParam pl fepa)
                      )
                    )
         )
         (if (< epa spa) 
           (setq plst (append 
                        (if (= spa fspa) 
                          (sublist plst fspa nil)
                          (cons (3dTo2dPt (trans spt 0 norm)) 
                                (sublist plst (1+ fspa) nil)
                          )
                        )
                        (cdr (sublist plst 0 (1+ fepa)))
                        (if (/= epa fepa) 
                          (list (3dTo2dPt (trans ept 0 norm)))
                        )
                      )
                 blst (append 
                        (if (= spa fspa) 
                          (sublist blst fspa nil)
                          (cons 
                            (k*bulge (nth fspa blst) srat)
                            (sublist blst (1+ fspa) nil)
                          )
                        )
                        (sublist blst 0 fepa)
                        (if (= epa fepa) 
                          (list (nth fepa blst))
                          (list (k*bulge (nth fepa blst) erat))
                        )
                      )
           )
           (setq plst (append 
                        (if (= spa fspa) 
                          (sublist plst fspa (1+ (- fepa fspa)))
                          (cons (3dTo2dPt (trans spt 0 norm)) 
                                (sublist plst (1+ fspa) (- fepa fspa))
                          )
                        )
                        (list (3dTo2dPt (trans ept 0 norm)))
                      )
                 blst (append 
                        (if (= spa fspa) 
                          (sublist blst fspa (- fepa fspa))
                          (cons 
                            (k*bulge (nth fspa blst) srat)
                            (sublist blst (1+ fspa) (- fepa fspa))
                          )
                        )
                        (if (= epa fepa) 
                          (list (nth fepa blst))
                          (list (k*bulge (nth fepa blst) erat))
                        )
                      )
           )
         )
        )
      )
      (vla-delete pl)
      (setq pl (vlax-invoke spc 'AddLightWeightPolyline (apply 'append plst)))
      (vlax-put pl 'Normal norm)
      (vla-put-Elevation pl elv)
      (foreach b blst 
        (vla-SetBulge pl n B)
        (setq n (1+ n))
      )
    )
  )
  (or (zerop (getvar 'delobj)) (vla-delete el))
  pl
)

;;----------------------------------------------------------------------------;;
;; Commands
;;----------------------------------------------------------------------------;;

;; EL2PL: Converts selected ellipses to polylignes
(defun c:el2pl (/ *error* acdoc ss e) 
  (vl-load-com)
  (setq acdoc (vla-get-ActiveDocument (vlax-get-acad-object)))

  (defun *error* (msg) 
    (if (and msg (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*"))) 
      (princ (strcat (if (= "FRA" (getvar 'locale)) "\nErreur: " "\nError: ") msg))
    )
    (vla-EndUndoMark acdoc)
    (princ)
  )

  (if (setq ss (ssget '((0 . "ELLIPSE")))) 
    (progn 
      (vla-StartUndoMark acdoc)
      (setq ss (vla-get-ActiveSelectionSet acdoc))
      (vlax-for e ss 
        (EllipseToPolyline e)
      )
      (vla-delete ss)
      (vla-EndUndoMark acdoc)
    )
  )
  (princ)
)

;; PELL: Draws an approximate ellipse/elliptical arc as a polyline
(defun c:pell (/ *error* ec pe do old ent obj) 
  (vl-load-com)
  (setq ec  (getvar 'cmdecho)
        pe  (getvar 'pellipse)
        do  (getvar 'delobj)
        old (entlast)
  )

  (defun *error* (msg) 
    (if (and msg (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*"))) 
      (princ (strcat (if (= "FRA" (getvar 'locale)) "\nErreur: " "\nError: ") msg))
    )
    (setvar 'cmdecho ec)
    (setvar 'pellipse pe)
    (setvar 'delobj do)
    (princ)
  )

  (setvar 'cmdecho 1)
  (setvar 'pellipse 0)
  (command "_.ellipse")
  (while (/= 0 (getvar "cmdactive")) 
    (command pause)
  )
  (if (not (eq old (setq ent (entlast)))) 
    (progn 
      (setvar 'delobj 1)
      (setq obj (vlax-ename->vla-object ent))
      (EllipseToPolyline obj)
    )
  )
  (*error* nil)
  (princ)
)

(princ "\nBB_ELL2POL loaded. Commands: EL2PL, PELL")
(princ)
