; --- Block Replace at Center with Random Name (BlockReplaceRandom.lsp) ---
; Defines a command "BLOCKREPLACERANDOM"
; 1. Converts the selection to a block with a random name.
; 2. Uses the geometric center as the base point.
; 3. Inserts the new block back at the center point, so it
;    remains in the same position as the original geometry.

(defun c:BlockReplaceRandom ( / ss i ename e ent-min ent-max pt1 pt2 min-pt 
                             max-pt center-pt blk-name base-name suffix)
  
  (vl-load-com) ; Ensure Visual LISP functions are loaded
  
  ; Prompt the user to select objects
  (princ "\nSelect objects to convert to a block in-place: ")
  (if (setq ss (ssget))
    (progn ; --- If objects are selected ---
      
      ; Initialize overall bounding box corners
      (setq min-pt '(1e99 1e99 1e99)
            max-pt '(-1e99 -1e99 -1e99)
            i 0
      )
      
      ; --- Loop through the selection set to find bounding box ---
      (while (< i (sslength ss))
        (setq ename (ssname ss i)) ; Get entity name
        (setq e (vlax-ename->vla-object ename)) ; Get VLA-object
        
        ; Get the bounding box of the individual entity
        (if (not (vl-catch-all-error-p 
                   (vl-catch-all-apply 'vla-getboundingbox (list e 'ent-min 'ent-max))
                 )
            )
          (progn
            (setq pt1 (vlax-safearray->list ent-min))
            (setq pt2 (vlax-safearray->list ent-max))
            (setq min-pt (mapcar 'min min-pt pt1))
            (setq max-pt (mapcar 'max max-pt pt2))
          )
        )
        (setq i (1+ i))
      ) ; --- End of while loop ---
      
      ; --- Calculate the center point ---
      (setq center-pt (mapcar '/ (mapcar '+ min-pt max-pt) '(2.0 2.0 2.0)))
      
      ; --- Generate a unique, random-ish block name ---
      (setq base-name (strcat "RndBlk-" (vl-string-subst "" "." (rtos (getvar "CDATE") 2 8))))
      (setq blk-name base-name)
      (setq suffix 1)
      
      ; Check if the name already exists and add a suffix if it does
      (while (tblsearch "BLOCK" blk-name)
        (setq blk-name (strcat base-name "-" (itoa suffix)))
        (setq suffix (1+ suffix))
      )
      
      ; --- Step 1: Create the block definition (this deletes the originals) ---
      (command "-BLOCK" blk-name center-pt ss "")
      
      ; --- Step 2: Insert the new block back at the center point ---
      ; Inserts block [blk-name] at [center-pt] with 
      ; X-scale=1, Y-scale=1, and Rotation=0
      (command "-INSERT" blk-name center-pt 1 1 0)
      
      (princ (strcat "\nConverted selection to block '" blk-name "' at geometric center."))
      
    ) ; --- End of progn ---
    
    (princ "\nNo objects selected.") ; If ssget returned nil
  )
  (princ) ; Suppress last return value (quiet exit)
)

(princ "\nLISP loaded. Type BLOCKREPLACERANDOM to run.")
(princ)