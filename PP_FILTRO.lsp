;;; ==========================================================================
;;; PP_FILTRO.lsp
;;; Comando: FI
;;; Objetivo: Filtrar e selecionar objetos baseando-se no sistema atribuído (XData)
;;; ==========================================================================

(vl-load-com)

(defun c:FI (/ ss_first ss ss_final system_list system_names dcl_id dcl_file dcl_handle 
             choice selected_system result sys_target i ent ent-data xdata xdata_content val)

  ;; 1. Captura selecao previa (Pickfirst) se houver
  (setq ss_first (ssget "_I"))
  (sssetfirst nil nil) ;; Limpa os grips para nao atrapalhar o dialog

  ;; 2. Definicao dos Sistemas (identica ao BB_SISTEMA)
  (setq system_list
    '(
      ("ESGOTO GORDUROSO")
      ("ESGOTO SANIT\301RIO")
      ("\301GUAS PLUVIAIS")
      ("\301GUAS SERVIDAS")
      ("DRENO A.C.")
      ("ESGOTO ESPUMOSO")
      ("VENTILA\307\303O")
      ("\301GUA QUENTE PEX")
      ("\301GUA QUENTE PVC")
      ("\301GUA FRIA PEX")
      ("\301GUA FRIA PVC")
      ("\301GUA FRIA - ALIMENTA\307\303O")
      ("\301GUA FRIA - PRESSURIZADA")
      ("\301GUA FRIA - VRP")
      ("\301GUA FRIA - EXTRAVASOR")
      ("PISCINA - RETORNO")
      ("PISCINA - ASPIRA\307\303O")
      ("PISCINA - ALIMENTA\307\303O")
    )
  )

  (setq system_names (mapcar 'car system_list))

  ;; 3. Criar e exibir a janela (DCL)
  (setq dcl_file (vl-filename-mktemp "bb_filtro.dcl"))
  (setq dcl_handle (open dcl_file "w"))
  (write-line "bb_filtro_dialog : dialog { label = \"Filtrar por Sistema\";" dcl_handle)
  (write-line "  : list_box { label = \"Selecione o Sistema para filtrar:\"; key = \"system_list\"; height = 20; width = 40; allow_accept = true; }" dcl_handle)
  (write-line "  : popup_list { label = \"Condicao de Teto:\"; key = \"teto_cond\"; }" dcl_handle)
  (write-line "  : row { alignment = centered;" dcl_handle)
  (write-line "    : button { label = \"Filtrar\"; key = \"accept\"; is_default = true; width = 12; fixed_width = true; }" dcl_handle)
  (write-line "    : button { label = \"Cancelar\"; key = \"cancel\"; is_cancel = true; width = 12; fixed_width = true; }" dcl_handle)
  (write-line "  }" dcl_handle)
  (write-line "}" dcl_handle)
  (close dcl_handle)

  (setq dcl_id (load_dialog dcl_file))
  (if (not (new_dialog "bb_filtro_dialog" dcl_id))
    (progn (princ "\nErro ao carregar DCL.") (exit))
  )

  (start_list "system_list")
  (mapcar 'add_list system_names)
  (end_list)

  (start_list "teto_cond")
  (add_list "Qualquer (Ignorar)")
  (add_list "Somente no Teto")
  (add_list "Nao pode ser Teto")
  (end_list)

  (set_tile "system_list" "0")
  (setq choice "0")
  
  (set_tile "teto_cond" "0")
  (setq choice_teto "0")

  (action_tile "system_list" "(setq choice $value)")
  (action_tile "teto_cond" "(setq choice_teto $value)")
  (action_tile "accept" "(done_dialog 1)")
  (action_tile "cancel" "(done_dialog 0)")

  (setq result (start_dialog))
  (unload_dialog dcl_id)
  (vl-file-delete dcl_file)

  ;; 4. Logica Pos-Dialogo
  (if (= result 1)
    (progn
      (setq selected_system (nth (atoi choice) system_list))
      (setq sys_target (car selected_system))
      (setq teto_val (atoi choice_teto))
      
      (if ss_first
        (setq ss ss_first)
        (progn
          (princ (strcat "\nSelecione a area para filtrar os objetos do sistema '" sys_target "': "))
          (setq ss (ssget))
        )
      )

      ;; 5. Filtragem
      (setq ss_final (ssadd))
      (if ss
        (progn
          (setq i 0)
          (while (< i (sslength ss))
            (setq ent (ssname ss i))
            ;; Tenta ler o XData BB_SISTEMA
            (if (setq ent-data (entget ent '("BB_SISTEMA")))
              (progn
                (setq xdata (assoc -3 ent-data))
                (if xdata
                  (progn
                    (setq xdata_content (cadr xdata)) ;; Pega os dados do APPID BB_SISTEMA
                    (if (and (listp xdata_content) (= (car xdata_content) "BB_SISTEMA"))
                      (progn
                        (setq val (cdr (assoc 1000 (cdr xdata_content))))
                        (if (= val sys_target)
                          (progn
                            ;; O sistema confere. Agora checa condicao de Teto.
                            (setq has_teto nil)
                            (if (setq ent-data-teto (entget ent '("BB_TETO")))
                              (if (assoc -3 ent-data-teto)
                                (setq has_teto T)
                              )
                            )
                            
                            (setq pass_teto T)
                            (if (= teto_val 1) ; Somente no Teto
                              (if (not has_teto) (setq pass_teto nil))
                            )
                            (if (= teto_val 2) ; Nao pode ser Teto
                              (if has_teto (setq pass_teto nil))
                            )
                            
                            (if pass_teto
                              (ssadd ent ss_final) ;; Adiciona a lista final
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
            (setq i (1+ i))
          )
        )
      )

      ;; 6. Resultados Visuais
      (if (> (sslength ss_final) 0)
        (progn
          (sssetfirst nil ss_final)
          (princ (strcat "\nSucesso! " (itoa (sslength ss_final)) " objetos do sistema '" sys_target "' selecionados."))
        )
        (princ (strcat "\nNenhum objeto encontrado que atenda a essas condicoes."))
      )
    )
    (princ "\nComando cancelado.")
  )
  (princ)
)

(princ "\nComando FI carregado. Digite FI para iniciar o filtro de sistema.")
(princ)
