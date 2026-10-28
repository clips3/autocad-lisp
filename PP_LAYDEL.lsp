(defun c:ldl (/ dcl_id dcl_file fn layer_list selected_index selected_layer ss result 
              action target_layer acad_obj doc blocks is_used target_list t_idx
             ) 
  (vl-load-com)

  ;; ============================================================
  ;; Generate DCL file on the fly (Embedded DCL method)
  ;; This creates a temp file, loads it, and deletes it after use
  ;; ============================================================
  (setq dcl_file (vl-filename-mktemp "dlg" nil ".dcl"))
  (setq fn (open dcl_file "w"))

  ;; Write DCL content line by line
  (write-line "delete_layer_dcl : dialog {" fn)
  (write-line "    label = \"Delete Layer\";" fn)
  (write-line "    : text {" fn)
  (write-line "        label = \"Selecione a layer para deletar:\";" fn)
  (write-line "    }" fn)
  (write-line "    : list_box {" fn)
  (write-line "        key = \"k_layers\";" fn)
  (write-line "        width = 40;" fn)
  (write-line "        height = 15;" fn)
  (write-line "        multiple_select = false;" fn)
  (write-line "        allow_accept = true;" fn)
  (write-line "    }" fn)
  (write-line "    : row {" fn)
  (write-line "        : button {" fn)
  (write-line "            key = \"k_delete\";" fn)
  (write-line "            label = \"Deletar\";" fn)
  (write-line "            is_default = true;" fn)
  (write-line "        }" fn)
  (write-line "        : button {" fn)
  (write-line "            key = \"cancel\";" fn)
  (write-line "            label = \"Fechar\";" fn)
  (write-line "            is_cancel = true;" fn)
  (write-line "        }" fn)
  (write-line "    }" fn)
  (write-line "}" fn)
  (write-line "" fn)
  (write-line "options_dcl : dialog {" fn)
  (write-line "    label = \"Opcoes de Exclusao\";" fn)
  (write-line "    : text {" fn)
  (write-line "        label = \"A layer contem objetos. O que deseja fazer?\";" 
              fn
  )
  (write-line "        alignment = centered;" fn)
  (write-line "    }" fn)
  (write-line "    : row {" fn)
  (write-line "        : button {" fn)
  (write-line "            key = \"k_opt_delete\";" fn)
  (write-line "            label = \"Excluir Objetos e Layer\";" fn)
  (write-line "        }" fn)
  (write-line "        : button {" fn)
  (write-line "            key = \"k_opt_move\";" fn)
  (write-line "            label = \"Mover Objetos para outra Layer\";" fn)
  (write-line "        }" fn)
  (write-line "    }" fn)
  (write-line "    : button {" fn)
  (write-line "        key = \"cancel\";" fn)
  (write-line "        label = \"Cancelar\";" fn)
  (write-line "        is_cancel = true;" fn)
  (write-line "    }" fn)
  (write-line "}" fn)
  (write-line "" fn)
  (write-line "select_target_dcl : dialog {" fn)
  (write-line "    label = \"Selecione Layer de Destino\";" fn)
  (write-line "    : text {" fn)
  (write-line "        label = \"Mover objetos para:\";" fn)
  (write-line "    }" fn)
  (write-line "    : list_box {" fn)
  (write-line "        key = \"k_target_layers\";" fn)
  (write-line "        width = 40;" fn)
  (write-line "        height = 15;" fn)
  (write-line "        multiple_select = false;" fn)
  (write-line "        allow_accept = true;" fn)
  (write-line "    }" fn)
  (write-line "    : row {" fn)
  (write-line "        : button {" fn)
  (write-line "            key = \"k_ok_move\";" fn)
  (write-line "            label = \"Mover e Deletar\";" fn)
  (write-line "            is_default = true;" fn)
  (write-line "        }" fn)
  (write-line "        : button {" fn)
  (write-line "            key = \"cancel\";" fn)
  (write-line "            label = \"Cancelar\";" fn)
  (write-line "            is_cancel = true;" fn)
  (write-line "        }" fn)
  (write-line "    }" fn)
  (write-line "}" fn)

  (close fn)

  ;; Debug: Print the temp file path
  (princ (strcat "\nDCL temp file: " dcl_file))

  ;; Load the temporary DCL
  (setq dcl_id (load_dialog dcl_file))

  (if (< dcl_id 0) 
    (progn 
      (alert (strcat "Erro ao carregar DCL temporario:\n" dcl_file))
      (exit)
    )
  )

  ;; Helper function to get all layer names
  (defun get-all-layers (/ lyr l_list) 
    (setq l_list '())
    (while (setq lyr (tblnext "LAYER" (null lyr))) 
      (setq l_list (cons (cdr (assoc 2 lyr)) l_list))
    )
    (vl-sort l_list '<)
  )

  ;; Helper function to populate DCL list
  (defun fill-list-box (key list_data) 
    (start_list key)
    (mapcar 'add_list list_data)
    (end_list)
  )

  ;; ---------------------------------------------------------
  ;; CORE FUNCTION: Process All Entities via ActiveX
  ;; ---------------------------------------------------------
  (defun process-all-entities-vla (target_lyr_name action_type new_lyr_name / 
                                   acad_obj doc blocks blk obj lyr_name
                                  ) 
    (setq acad_obj (vlax-get-acad-object))
    (setq doc (vla-get-ActiveDocument acad_obj))
    (setq blocks (vla-get-Blocks doc))

    (vlax-for blk blocks 
      (vlax-for obj blk 
        (setq lyr_name (vla-get-Layer obj))

        (if (= (strcase lyr_name) (strcase target_lyr_name)) 
          (cond 
            ((= action_type 1)
             (vla-Delete obj)
            )
            ((= action_type 2)
             (if new_lyr_name 
               (vla-put-Layer obj new_lyr_name)
             )
            )
          )
        )
      )
    )

    (vla-Regen doc acAllViewports)
  )

  ;; Helper to check if layer is used
  (defun layer-used-p (target_lyr_name / acad_obj doc blocks blk obj found) 
    (setq acad_obj (vlax-get-acad-object))
    (setq doc (vla-get-ActiveDocument acad_obj))
    (setq blocks (vla-get-Blocks doc))
    (setq found nil)

    (vlax-for blk blocks 
      (if (not found) 
        (vlax-for obj blk 
          (if 
            (and (not found) 
                 (= (strcase (vla-get-Layer obj)) (strcase target_lyr_name))
            )
            (setq found T)
          )
        )
      )
    )
    found
  )

  ;; ---------------------------------------------------------
  ;; Main Logic Flow
  ;; ---------------------------------------------------------
  (setq layer_list (get-all-layers))

  ;; 1. Show Main Dialog
  (if (not (new_dialog "delete_layer_dcl" dcl_id)) 
    (progn 
      (alert "Erro ao abrir dialogo principal.")
      (unload_dialog dcl_id)
      (vl-file-delete dcl_file)
      (exit)
    )
  )

  (fill-list-box "k_layers" layer_list)

  (action_tile "k_delete" 
               "(progn
       (setq selected_index (atoi (get_tile \"k_layers\")))
       (setq selected_layer (nth selected_index layer_list))
       (if selected_layer
         (done_dialog 1)
         (alert \"Selecione uma layer da lista primeiro.\")
       )
    )"
  )
  (action_tile "cancel" "(done_dialog 0)")

  (setq result (start_dialog))

  ;; ---------------------------------------------------------
  ;; Execute Selection
  ;; ---------------------------------------------------------
  (if (and (= result 1) selected_layer) 
    (progn 
      (if (= (strcase selected_layer) (strcase (getvar "CLAYER"))) 
        (alert "Nao e possivel deletar a layer atual.")
        (progn 
          (setq is_used (layer-used-p selected_layer))

          (if is_used 
            (progn 
              (setq action 0)
              (if (new_dialog "options_dcl" dcl_id) 
                (progn 
                  (action_tile "k_opt_delete" "(done_dialog 2)")
                  (action_tile "k_opt_move" "(done_dialog 3)")
                  (action_tile "cancel" "(done_dialog 0)")
                  (setq action (start_dialog))
                )
              )
            )
            (setq action 2)
          )

          (cond 
            ((= action 2)
             (progn 
               (princ "\nProcessando exclusao de objetos...")
               (process-all-entities-vla selected_layer 1 nil)
               (command "-layer" "delete" selected_layer "")
               (princ (strcat "\nLayer " selected_layer " deletada."))
             )
            )

            ((= action 3)
             (progn 
               (if (new_dialog "select_target_dcl" dcl_id) 
                 (progn 
                   (setq target_list (vl-remove selected_layer layer_list))
                   (fill-list-box "k_target_layers" target_list)
                   (action_tile "k_ok_move" 
                                "(progn
                            (setq t_idx (atoi (get_tile \"k_target_layers\")))
                            (setq target_layer (nth t_idx target_list))
                            (if target_layer (done_dialog 4) (alert \"Selecione destino.\"))
                         )"
                   )
                   (action_tile "cancel" "(done_dialog 0)")
                   (setq action (start_dialog))
                 )
               )

               (if (and (= action 4) target_layer) 
                 (progn 
                   (princ (strcat "\nMovendo objetos para " target_layer "..."))
                   (process-all-entities-vla selected_layer 2 target_layer)
                   (command "-layer" "delete" selected_layer "")
                   (princ "\nConcluido.")
                 )
               )
             )
            )
          )
        )
      )
    )
  )

  ;; Cleanup
  (unload_dialog dcl_id)
  (vl-file-delete dcl_file)
  (princ)
)

(princ "\nComando 'ldl' carregado. Digite 'ldl' para iniciar.")
