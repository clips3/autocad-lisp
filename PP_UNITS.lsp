(defun c:UNI () 
  "Sets drawing units to standard: Decimal, 3 decimal places, Meters, International lighting."
  (setvar "CMDECHO" 0)

  ;; Length
  (setvar "LUNITS" 2) ; 2 = Decimal
  (setvar "LUPREC" 3) ; 3 = 0.000

  ;; Angle
  (setvar "AUNITS" 0) ; 0 = Decimal Degrees
  (setvar "AUPREC" 0) ; 0 = 0
  (setvar "ANGDIR" 0) ; 0 = Counter-Clockwise (Standard)

  ;; Insertion scale
  (setvar "INSUNITS" 6) ; 6 = Meters

  ;; Lighting
  (setvar "LIGHTINGUNITS" 1) ; 1 = International

  (setvar "CMDECHO" 1)
  (princ "\nDrawing units standardized: [Meters | Decimal 0.000 | Degrees 0].")
  (princ)
)

(princ "\nStandard Units script loaded. Type 'UNI' to execute.")
(princ)
