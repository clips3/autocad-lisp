;;; PP_SISTEMA.lsp
;;; Comando: TO
;;; Objetivo: Atribuir sistemas e layers a objetos selecionados.
;;; Autor: Antigravity

(vl-load-com)

;;; Função auxiliar para adicionar XData do Sistema
(defun bb:add-system-xdata (ent sys_name / obj dataType dataValue)
  (if (and ent (entget ent))
    (progn
      (if (not (tblsearch "APPID" "BB_SISTEMA"))
        (regapp "BB_SISTEMA")
      )
      (setq obj (vlax-ename->vla-object ent))
      (setq dataType (vlax-make-safearray vlax-vbInteger '(0 . 1)))
      (vlax-safearray-put-element dataType 0 1001)
      (vlax-safearray-put-element dataType 1 1000)
      
      (setq dataValue (vlax-make-safearray vlax-vbVariant '(0 . 1)))
      (vlax-safearray-put-element dataValue 0 "BB_SISTEMA")
      (vlax-safearray-put-element dataValue 1 sys_name)
      
      (vl-catch-all-apply 'vla-SetXData (list obj dataType dataValue))
    )
  )
)

;;; Função auxiliar para adicionar XData de Teto
(defun bb:add-teto-xdata (ent / obj dataType dataValue)
  (if (and ent (entget ent))
    (progn
      (if (not (tblsearch "APPID" "BB_TETO"))
        (regapp "BB_TETO")
      )
      (setq obj (vlax-ename->vla-object ent))
      (setq dataType (vlax-make-safearray vlax-vbInteger '(0 . 1)))
      (vlax-safearray-put-element dataType 0 1001)
      (vlax-safearray-put-element dataType 1 1000)
      
      (setq dataValue (vlax-make-safearray vlax-vbVariant '(0 . 1)))
      (vlax-safearray-put-element dataValue 0 "BB_TETO")
      (vlax-safearray-put-element dataValue 1 "SIM")
      
      (vl-catch-all-apply 'vla-SetXData (list obj dataType dataValue))
    )
  )
)

(defun c:TO (/ ss system_list system_names dcl_id dcl_file dcl_handle choice selected_system layer_l layer_h color_l color_h i ename_list ent obj_type data linetypes vla_obj)

  ;; 1. Obter seleção (prévia ou pós-comando)
  (setq ss (ssget "_I"))
  (if (not ss)
    (setq ss (ssget))
  )

  (if (not ss)
    (progn
      (princ "\nNenhum objeto selecionado.")
      (exit)
    )
  )

  ;; CONVERTER SELEÇÃO PARA LISTA E LIMPAR SS
  (setq ename_list nil)
  (setq i 0)
  (repeat (sslength ss)
    (setq ename_list (cons (ssname ss i) ename_list))
    (setq i (1+ i))
  )
  (setq ss nil) 
  (sssetfirst nil nil) ;; Limpa seleção prévia/grips
  (gc)

  ;; 2. Definição dos Sistemas (Nome, RGB_L, RGB_H)
  ;; Usando códigos octais para acentos para garantir compatibilidade de encoding (\301=Á, \303=Ã, \307=Ç, \315=Í)
  (setq system_list
    '(
      ("ESGOTO GORDUROSO" (138 123 25) (237 224 75))
      ("ESGOTO SANIT\301RIO" (78 53 36) (158 101 46))
      ("\301GUAS PLUVIAIS" (0 75 135) (0 181 226))
      ("\301GUAS SERVIDAS" (0 43 73) (0 78 168))
      ("DRENO A.C." (53 46 101) (104 91 199))
      ("ESGOTO ESPUMOSO" (87 44 95) (155 38 182))
      ("VENTILA\307\303O" (128 34 95) (228 93 191))
      ("\301GUA QUENTE PEX" (118 35 47) (228 0 43))
      ("\301GUA QUENTE PVC" (155 90 26) (255 130 0))
      ("\301GUA FRIA PEX" (65 138 167) (100 210 255))
      ("\301GUA FRIA PVC" (89 98 29) (170 219 30))
      ("\301GUA FRIA - ALIMENTA\307\303O" (51 85 37) (39 159 0))
      ("\301GUA FRIA - PRESSURIZADA" (0 102 79) (60 219 192))
      ("\301GUA FRIA - VRP" (0 43 73) (7 79 113))
      ("\301GUA FRIA - EXTRAVASOR" (101 102 53) (186 188 22))
      ("PISCINA - RETORNO" (136 24 58) (195 35 83))
      ("PISCINA - ASPIRA\307\303O" (50 25 82) (103 49 169))
      ("PISCINA - ALIMENTA\307\303O" (0 105 82) (0 164 129))
    )
  )

  (setq system_names (mapcar 'car system_list))

  ;; 3. Criar e exibir a janela (DCL)
  (setq dcl_file (vl-filename-mktemp "bb_sistema.dcl"))
  (setq dcl_handle (open dcl_file "w"))
  (write-line "bb_sistema_dialog : dialog { label = \"Escolher Sistema\";" dcl_handle)
  (write-line "  : list_box { label = \"Selecione o Sistema:\"; key = \"system_list\"; height = 20; width = 40; allow_accept = true; }" dcl_handle)
  (write-line "  : row { alignment = centered;" dcl_handle)
  (write-line "    : button { label = \"OK\"; key = \"accept\"; is_default = true; width = 10; fixed_width = true; }" dcl_handle)
  (write-line "    : button { label = \"Teto\"; key = \"teto\"; width = 10; fixed_width = true; }" dcl_handle)
  (write-line "    : button { label = \"Cancel\"; key = \"cancel\"; is_cancel = true; width = 10; fixed_width = true; }" dcl_handle)
  (write-line "  }" dcl_handle)
  (write-line "}" dcl_handle)
  (close dcl_handle)

  (setq dcl_id (load_dialog dcl_file))
  (if (not (new_dialog "bb_sistema_dialog" dcl_id))
    (progn (princ "\nErro ao carregar DCL.") (exit))
  )

  (start_list "system_list")
  (mapcar 'add_list system_names)
  (end_list)

  (set_tile "system_list" "0")
  (setq choice "0")

  (action_tile "system_list" "(setq choice $value)")
  (action_tile "accept" "(done_dialog 1)")
  (action_tile "teto" "(done_dialog 2)")
  (action_tile "cancel" "(done_dialog 0)")

  (setq result (start_dialog))
  (unload_dialog dcl_id)
  (vl-file-delete dcl_file)

  (cond
    ((= result 1)
      (progn
        (setq selected_system (nth (atoi choice) system_list))
        (setq layer_l (strcat "S_" (car selected_system) " L"))
        (setq layer_h (strcat "S_" (car selected_system) " H"))
        (setq color_l (nth 1 selected_system))
        (setq color_h (nth 2 selected_system))

        ;; 4. Garantir que as layers existem
        (bb_create_layer layer_l color_l)
        (bb_create_layer layer_h color_h)

        ;; 5. Aplicar layers aos objetos via entmod
        (foreach ent ename_list
          (setq data (entget ent))
          ;; Ignorar objetos nas layers PP_LINHA CENTRAL e PP_GUIAS
          (if (not (member (strcase (cdr (assoc 8 data))) '("PP_LINHA CENTRAL" "PP_GUIAS")))
            (progn
              (setq obj_type (cdr (assoc 0 data)))
              (if (or (= obj_type "HATCH") (= obj_type "INSERT"))
                (progn
                  ;; Mudar layer para o sufixo H
                  (setq data (subst (cons 8 layer_h) (assoc 8 data) data))
                  (entmod data)
                  
                  ;; Tenta transparência via VLA (aplica a hachuras e agora blocos também se possível)
                  (vl-catch-all-apply 
                    '(lambda () 
                      (vla-put-EntityTransparency (vlax-ename->vla-object ent) "0")
                    )
                  )
                )
                (progn
                  ;; Mudar layer para o sufixo L
                  (setq data (subst (cons 8 layer_l) (assoc 8 data) data))
                  (entmod data)
                )
              )
              ;; Adiciona a identificacao invisivel do sistema no objeto
              (bb:add-system-xdata ent (car selected_system))
            )
          )
        )
        (princ (strcat "\nSistema '" (car selected_system) "' aplicado com sucesso."))
      )
    )
    ((= result 2)
      (progn
        ;; Carregar linetype DASHED se não existir
        (vl-catch-all-apply
          '(lambda ()
            (setq linetypes (vla-get-linetypes (vla-get-ActiveDocument (vlax-get-acad-object))))
            (if (vl-catch-all-error-p (vl-catch-all-apply 'vla-item (list linetypes "DASHED")))
              (if (vl-catch-all-error-p (vl-catch-all-apply 'vla-load (list linetypes "DASHED" "acad.lin")))
                (vl-catch-all-apply 'vla-load (list linetypes "DASHED" "acadiso.lin"))
              )
            )
          )
        )

        ;; Aplicar apenas propriedades Teto
        (foreach ent ename_list
          (setq data (entget ent))
          ;; Ignorar objetos nas layers PP_LINHA CENTRAL e PP_GUIAS
          (if (not (member (strcase (cdr (assoc 8 data))) '("PP_LINHA CENTRAL" "PP_GUIAS")))
            (progn
              (setq obj_type (cdr (assoc 0 data)))
              (if (or (= obj_type "HATCH") (= obj_type "INSERT"))
                (progn
                  ;; Transparência 80
                  (vl-catch-all-apply 
                    '(lambda () 
                      (vla-put-EntityTransparency (vlax-ename->vla-object ent) "80")
                    )
                  )
                )
                (progn
                  ;; Mudar linetype para DASHED e LinetypeScale para 0.5
                  (vl-catch-all-apply 
                    '(lambda () 
                      (setq vla_obj (vlax-ename->vla-object ent))
                      (vla-put-Linetype vla_obj "DASHED")
                      (vla-put-LinetypeScale vla_obj 0.5)
                    )
                  )
                )
              )
              ;; Adiciona o XData de Teto
              (bb:add-teto-xdata ent)
            )
          )
        )
        (princ "\nPropriedades de TETO aplicadas com sucesso.")
      )
    )
    (t
      (princ "\nOperação cancelada.")
    )
  )
  (princ)
)

;; Função auxiliar para criar layer com TrueColor
(defun bb_create_layer (name rgb / layers layer objColor)
  (setq layers (vla-get-layers (vla-get-ActiveDocument (vlax-get-acad-object))))
  (if (vl-catch-all-error-p (setq layer (vl-catch-all-apply 'vla-item (list layers name))))
    (setq layer (vla-add layers name))
  )
  (setq objColor (vla-get-TrueColor layer))
  (vla-SetRGB objColor (nth 0 rgb) (nth 1 rgb) (nth 2 rgb))
  (vla-put-TrueColor layer objColor)
  (vla-put-linetype layer "Continuous")
)

(princ "\nBB_SISTEMA carregado. Digite TO para iniciar.")
(princ)
