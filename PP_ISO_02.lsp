;; -----------------------------------------------------------------
;; Command: SBS
;; Purpose: Sets the X, Y, and Z scale of selected blocks
;;          to fixed values (X=1.2260, Y=0.7078, Z=1.2260).
;; -----------------------------------------------------------------
(defun c:SBS (/ ss i num ename edata old_x new_x old_y new_y old_z new_z)
  
  (prompt "\nSelect blocks to rescale: ")
  
  ;; Filter the selection set to only include blocks ("INSERT")
  (setq ss (ssget '((0 . "INSERT"))))
  
  ;; Check if any blocks were selected
  (if ss
    (progn
      (setq i 0) ; Initialize a counter
      (setq num (sslength ss)) ; Get the total number of selected blocks
      
      (prompt (strcat "\n" (itoa num) " block(s) selected. Rescaling..."))
      
      ;; Loop through each block in the selection set
      (repeat num
        (setq ename (ssname ss i)) ; Get the entity name
        (setq edata (entget ename)) ; Get the entity's data list
        
        ;; --- Modify X Scale (DXF Code 41) ---
        (setq old_x (assoc 41 edata))       ; Find the old X scale pair
        (setq new_x (cons 41 1.2260))       ; Create the new X scale pair
        (setq edata (subst new_x old_x edata)) ; Substitute the new pair for the old one

        ;; --- Modify Y Scale (DXF Code 42) ---
        (setq old_y (assoc 42 edata))
        (setq new_y (cons 42 0.7078))
        (setq edata (subst new_y old_y edata))

        ;; --- Modify Z Scale (DXF Code 43) ---
        (setq old_z (assoc 43 edata))
        (setq new_z (cons 43 1.2260))
        (setq edata (subst new_z old_z edata))
        
        ;; Apply the modified data back to the entity
        (entmod edata)
        
        ;; Increment the counter
        (setq i (1+ i))
      )
      (prompt (strcat "\n" (itoa num) " block(s) rescaled successfully."))
    )
    (prompt "\nNo blocks were selected.") ; Message if no blocks were found
  )
  
  (princ) ; Cleanly exits the LISP routine
)