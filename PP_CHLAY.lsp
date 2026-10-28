;; Função auxiliar: normalização de strings (remove acentos e converte para maiúsculas)
(defun chlay:norm-str (s)
  (if (and s (= (type s) 'STR))
    (strcase (vl-string-translate "áàãâéêíóõôúçÁÀÃÂÉÊÍÓÕÔÚÇ"
                                 "aaaaeeioooucaaaaeeiooouc"
                                 s))
    ""
  )
)

;; Função auxiliar: busca case-insensitive e tolerante a acentos na lista de layers
(defun chlay:find-layer (name lst / item result target-norm test-norm)
  (if (and name (= (type name) 'STR))
    (progn
      (setq target-norm (chlay:norm-str name))
      ;; 1. Busca exata case-insensitive
      (foreach item lst
        (if (not result)
          (if (= (strcase (car item)) (strcase name))
            (setq result item)
          )
        )
      )
      ;; 2. Busca tolerante a acentos
      (if (not result)
        (foreach item lst
          (if (not result)
            (progn
              (setq test-norm (chlay:norm-str (car item)))
              (if (or (= test-norm target-norm)
                      (wcmatch (strcase name) (strcase (car item)))
                      (wcmatch target-norm test-norm)
                  )
                (setq result item)
              )
            )
          )
        )
      )
    )
  )
  result
)

;; Retorna a lista completa de definições de camadas padrão com propriedades originais do CHL
(defun chlay:get-predefined-layers ()
  (list 
    (cons "PP_LAYOUT" 
          '((62 . 253)
            (6 . "Continuous")
           )
    )
    (cons "PP_ARQUITETURA" 
          '((62 . 7)
            (6 . "Continuous")
            ("HatchPattern" . "ANSI31")
            ("HatchScale" . 0.02)
           )
    )
    (cons "PP_COTAS" '((62 . 10) (6 . "Continuous")))
    (cons "PP_EQUIPAMENTO" 
          '((62 . 6)
            (6 . "Continuous")
            ("Plot" . "No")
           )
    )
    (cons "PP_FORRO" 
          '((62 . 43)
            (6 . "Continuous")
            ("HatchPattern" . "SOLID")
            ("HatchTransparency" . "65")
           )
    )
    (cons "PP_GARAGEM" '((62 . 254) (6 . "Continuous")))
    (cons "PP_CUBETA" 
          '((62 . 254)
            (6 . "Continuous")
            ("Plot" . "No")
           )
    )
    (cons "PP_COBERTURA" '((62 . 254) (6 . "Continuous")))
    (cons "PP_PILAR" 
          '((62 . 30)
            (6 . "Continuous")
            ("HatchPattern" . "ANSI37")
            ("HatchScale" . 0.03)
           )
    )
    (cons "PP_PROJEÇÃO" '((62 . 253) (6 . "DASHED")))
    (cons "PP_REGIÃO" '((62 . 12) (6 . "PHANTOM2")))
    (cons "PP_TAG" '((62 . 7) (6 . "Continuous")))
    (cons "PP_TAG COLUNA" '((62 . 7) (6 . "Continuous")))
    (cons "PP_TEXTO" '((62 . 7) (6 . "Continuous")))
    (cons "PP_TEXTO APOIO" '((62 . 7) (6 . "Continuous")))
    (cons "PP_TEXTO PILAR" '((62 . 32) (6 . "Continuous")))
    (cons "PP_TEXTO VIGA" 
          '((62 . 32)
            (6 . "Continuous")
            ("Plot" . "No")
           )
    )
    (cons "PP_VEGETAÇÃO" 
          '((62 . 254)
            (6 . "Continuous")
            ("HatchPattern" . "GRASS")
            ("HatchScale" . 0.002)
           )
    )
    (cons "PP_VIGA" '((62 . 30) (6 . "DASHED")))
    (cons "PP_MKP_ALVENARIA" 
          '((62 . 1) (6 . "Continuous"))
    )
    (cons "PP_MKP_LAJE" '((62 . 5) (6 . "Continuous")))
    (cons "PP_MKP_VIGA" '((62 . 6) (6 . "Continuous")))
  )
)

;; Garante e REAFIRMA que TODAS as camadas do padrão PP existam e tenham suas propriedades corretas (Cores, Tipos de Linha e Plotabilidade)
(defun chlay:ensure-all-predefined-layers (/ doc layers ltypes predefined_layers lay_item 
                                             lay_name props lay_color lay_ltype lay_plot lay_obj
                                            )
  (vl-load-com)
  (setq doc (vla-get-activedocument (vlax-get-acad-object)))
  (setq layers (vla-get-layers doc))
  (setq ltypes (vla-get-linetypes doc))
  (setq predefined_layers (chlay:get-predefined-layers))

  (foreach lay_item predefined_layers
    (setq lay_name (car lay_item))
    (setq props (cdr lay_item))
    (setq lay_color (cdr (assoc 62 props)))
    (setq lay_ltype (cdr (assoc 6 props)))
    (setq lay_plot (cdr (assoc "Plot" props)))

    ;; 1. Obter ou criar a camada
    (setq lay_obj (vl-catch-all-apply 'vla-add (list layers lay_name)))

    (if (and lay_obj (not (vl-catch-all-error-p lay_obj)))
      (progn
        ;; 2. Reafirmar e forçar a cor padrão da camada
        (if lay_color
          (vl-catch-all-apply '(lambda () (vla-put-color lay_obj lay_color)))
        )

        ;; 3. Carregar e reafirmar o tipo de linha padrão da camada
        (if (and lay_ltype (/= (strcase lay_ltype) "CONTINUOUS"))
          (progn
            (if (not (tblsearch "LTYPE" lay_ltype))
              (progn
                (vl-catch-all-apply '(lambda () (vla-load ltypes lay_ltype "acadiso.lin")))
                (if (not (tblsearch "LTYPE" lay_ltype))
                  (vl-catch-all-apply '(lambda () (vla-load ltypes lay_ltype "acad.lin")))
                )
              )
            )
            (if (tblsearch "LTYPE" lay_ltype)
              (vl-catch-all-apply '(lambda () (vla-put-linetype lay_obj lay_ltype)))
            )
          )
          (vl-catch-all-apply '(lambda () (vla-put-linetype lay_obj "Continuous")))
        )

        ;; 4. Reafirmar se a camada é plotável ou não
        (if (and lay_plot (= (strcase lay_plot) "NO"))
          (vl-catch-all-apply '(lambda () (vla-put-plottable lay_obj :vlax-false)))
          (vl-catch-all-apply '(lambda () (vla-put-plottable lay_obj :vlax-true)))
        )
      )
    )
  )
  ;; Padronizar LTSCALE em 0.03 e restaurar camada ativa para "0" se existir
  (vl-catch-all-apply '(lambda () (setvar "LTSCALE" 0.03)))
  (if (tblsearch "LAYER" "0")
    (vl-catch-all-apply '(lambda () (setvar "CLAYER" "0")))
  )
  (princ)
)

;; Aplica transparência diretamente via DXF 440 e VLA (sem chamadas ao CHPROP)
(defun chlay:set-transparency (ent val / vla-obj elist alpha dxf440 val_str val_int)
  (if (and ent (setq elist (entget ent)))
    (progn
      (setq val_int (if (numberp val) val (atoi (vl-princ-to-string (if val val 0)))))
      (setq val_str (itoa val_int))
      (setq vla-obj (vlax-ename->vla-object ent))

      ;; 1. Aplicação via DXF Group 440 (Fórmula oficial: 33554432 + Alpha_Byte)
      (if (and (>= val_int 0) (<= val_int 90))
        (progn
          (setq alpha (fix (* (- 100 val_int) 2.55)))
          (setq dxf440 (+ 33554432 alpha))
          (if (assoc 440 elist)
            (entmod (subst (cons 440 dxf440) (assoc 440 elist) elist))
            (entmod (append elist (list (cons 440 dxf440))))
          )
          (entupd ent)
        )
      )

      ;; 2. Aplicação via VLA ActiveX
      (vl-catch-all-apply '(lambda () (vla-put-Transparency vla-obj val_str)))
    )
  )
)

;; Calcula a assinatura geométrica espacial de uma hachura (BoundingBox + Centroide + Área)
(defun chlay:get-hatch-sig (ent / vla-obj minPt maxPt min_pt max_pt area min_x min_y max_x max_y cen_x cen_y)
  (if (and ent (entget ent) (= (cdr (assoc 0 (entget ent))) "HATCH"))
    (progn
      (setq vla-obj (vlax-ename->vla-object ent))
      (setq minPt nil maxPt nil)
      (vl-catch-all-apply 'vla-GetBoundingBox (list vla-obj 'minPt 'maxPt))
      (if (and minPt maxPt)
        (progn
          (setq min_pt (vlax-safearray->list minPt))
          (setq max_pt (vlax-safearray->list maxPt))
          (setq area (vl-catch-all-apply 'vla-get-Area (list vla-obj)))
          (if (or (vl-catch-all-error-p area) (not (numberp area)))
            (setq area 0.0)
          )
          (setq min_x (car min_pt) min_y (cadr min_pt))
          (setq max_x (car max_pt) max_y (cadr max_pt))
          (setq cen_x (/ (+ min_x max_x) 2.0))
          (setq cen_y (/ (+ min_y max_y) 2.0))
          (strcat (rtos min_x 2 2) "_" 
                  (rtos min_y 2 2) "_" 
                  (rtos max_x 2 2) "_" 
                  (rtos max_y 2 2) "_" 
                  (rtos cen_x 2 2) "_" 
                  (rtos cen_y 2 2) "_" 
                  (rtos area 2 2)
          )
        )
        nil
      )
    )
    nil
  )
)

;; Calcula a assinatura geométrica espacial de uma polilinha (BoundingBox + Centroide + Área)
(defun chlay:get-pline-sig (ent / vla-obj minPt maxPt min_pt max_pt area min_x min_y max_x max_y cen_x cen_y)
  (if (and ent (entget ent))
    (progn
      (setq vla-obj (vlax-ename->vla-object ent))
      (setq minPt nil maxPt nil)
      (vl-catch-all-apply 'vla-GetBoundingBox (list vla-obj 'minPt 'maxPt))
      (if (and minPt maxPt)
        (progn
          (setq min_pt (vlax-safearray->list minPt))
          (setq max_pt (vlax-safearray->list maxPt))
          (setq area (vl-catch-all-apply 'vla-get-Area (list vla-obj)))
          (if (or (vl-catch-all-error-p area) (not (numberp area)))
            (setq area 0.0)
          )
          (setq min_x (car min_pt) min_y (cadr min_pt))
          (setq max_x (car max_pt) max_y (cadr max_pt))
          (setq cen_x (/ (+ min_x max_x) 2.0))
          (setq cen_y (/ (+ min_y max_y) 2.0))
          (strcat (rtos min_x 2 2) "_" 
                  (rtos min_y 2 2) "_" 
                  (rtos max_x 2 2) "_" 
                  (rtos max_y 2 2) "_" 
                  (rtos cen_x 2 2) "_" 
                  (rtos cen_y 2 2) "_" 
                  (rtos area 2 2)
          )
        )
        nil
      )
    )
    nil
  )
)

;; Formatação exata de Hachuras (Cria o par SOLID 80 + ANSI37 no topo para PP_PILAR ou aplica padrão da camada)
(defun chlay:format-hatch (ent lay_name / vla-obj predefined_layers lay_def props 
                                          hatch_pattern hatch_scale hatch_transparency 
                                          is_pilar vla-obj-copy none_color ent_name
                          )
  (if (and ent (entget ent) (= (cdr (assoc 0 (entget ent))) "HATCH"))
    (progn
      (setq vla-obj (vlax-ename->vla-object ent))
      (setq ent_name ent)
      (setq predefined_layers (chlay:get-predefined-layers))
      (setq lay_def (chlay:find-layer lay_name predefined_layers))

      (if lay_def
        (progn
          (setq props (cdr lay_def))
          (setq hatch_pattern (cdr (assoc "HatchPattern" props)))
          (setq hatch_scale (cdr (assoc "HatchScale" props)))
          (setq hatch_transparency (cdr (assoc "HatchTransparency" props)))

          (if (not hatch_pattern) (setq hatch_pattern "SOLID"))
          (setq is_pilar (= (strcase lay_name) "PP_PILAR"))

          (if is_pilar
            (progn
              ;; 1. Hachura de fundo: SOLID com transparência 80
              (vl-catch-all-apply
                '(lambda ()
                   (vla-SetPattern vla-obj 1 "SOLID")
                   (vla-put-PatternAngle vla-obj 0.0)
                   (vla-evaluate vla-obj)
                 )
              )
              (chlay:set-transparency ent_name 80)

              ;; Remover cor de fundo do SOLID
              (vl-catch-all-apply
                '(lambda ()
                   (setq none_color (vla-get-backgroundcolor vla-obj))
                   (if (and none_color (not (vl-catch-all-error-p none_color)))
                     (progn
                       (vla-put-entitycolor none_color -939524096)
                       (vla-put-backgroundcolor vla-obj none_color)
                     )
                   )
                 )
              )

              ;; 2. Hachura no topo: ANSI37 (Copiada DEPOIS, ficando nativamente POR CIMA)
              (setq vla-obj-copy (vl-catch-all-apply 'vla-Copy (list vla-obj)))
              (if (and vla-obj-copy (not (vl-catch-all-error-p vla-obj-copy)))
                (progn
                  (vl-catch-all-apply
                    '(lambda ()
                       (vla-SetPattern vla-obj-copy 1 (if hatch_pattern hatch_pattern "ANSI37"))
                       (vla-put-PatternAngle vla-obj-copy 0.0)
                       (vla-put-PatternScale vla-obj-copy (if (numberp hatch_scale) hatch_scale 0.03))
                       (vla-evaluate vla-obj-copy)
                     )
                  )
                  (chlay:set-transparency (vlax-vla-object->ename vla-obj-copy) 0)

                  ;; Remover cor de fundo da hachura do topo
                  (vl-catch-all-apply
                    '(lambda ()
                       (setq none_color (vla-get-backgroundcolor vla-obj-copy))
                       (if (and none_color (not (vl-catch-all-error-p none_color)))
                         (progn
                           (vla-put-entitycolor none_color -939524096)
                           (vla-put-backgroundcolor vla-obj-copy none_color)
                         )
                       )
                     )
                  )
                )
              )
            )
            (progn
              ;; Comportamento Padrão para outras camadas
              (vl-catch-all-apply
                '(lambda ()
                   (vla-SetPattern vla-obj 1 hatch_pattern)
                   (vla-put-PatternAngle vla-obj 0.0)
                   (if (and hatch_scale (numberp hatch_scale)) (vla-put-PatternScale vla-obj hatch_scale))
                   (vla-evaluate vla-obj)
                 )
              )

              ;; Remover cor de fundo (None = -939524096)
              (vl-catch-all-apply
                '(lambda ()
                   (setq none_color (vla-get-backgroundcolor vla-obj))
                   (if (and none_color (not (vl-catch-all-error-p none_color)))
                     (progn
                       (vla-put-entitycolor none_color -939524096)
                       (vla-put-backgroundcolor vla-obj none_color)
                     )
                   )
                 )
              )

              ;; Aplicar transparência se definida no cadastro
              (if hatch_transparency
                (chlay:set-transparency ent_name hatch_transparency)
              )
            )
          )
        )
      )
    )
  )
)

;; Deduplica hachuras sobrepostas:
;; Para PP_PILAR: garante exatamente 2 hachuras (SOLID transp 80 por baixo e ANSI37 por cima) e DELETA todas as excedentes.
;; Para outras camadas: garante exatamente 1 hachura padronizada por local e DELETA duplicatas.
(defun chlay:deduplicate-hatches (target_layer / ss i ent elist lay sig groups 
                                                 group_key item group_ents lay_name 
                                                 is_pilar del_count h_solid h_ansi 
                                                 vla_solid vla_ansi h_extra vla_extra
                                                 props hatch_pattern hatch_scale 
                                                 hatch_transparency predefined_layers lay_def
                                                 none_color filter_list
                                 )
  (vl-load-com)
  (setq del_count 0)
  (setq predefined_layers (chlay:get-predefined-layers))

  ;; Montar filtro de seleção de hachuras
  (if (and target_layer (/= target_layer ""))
    (setq filter_list (list '(0 . "HATCH") (cons 8 target_layer)))
    (setq filter_list '((0 . "HATCH") (8 . "PP_*")))
  )

  (setq ss (ssget "_X" filter_list))
  (if ss
    (progn
      (setq groups nil)
      (setq i 0)
      ;; 1. Agrupar todas as hachuras por (Camada + Assinatura Espacial)
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (if (and ent (setq elist (entget ent)))
          (progn
            (setq lay (cdr (assoc 8 elist)))
            (setq sig (chlay:get-hatch-sig ent))
            (if sig
              (progn
                (setq group_key (strcase (strcat lay "_" sig)))
                (setq item (assoc group_key groups))
                (if item
                  (setq groups (subst (cons group_key (cons ent (cdr item))) item groups))
                  (setq groups (cons (cons group_key (list ent)) groups))
                )
              )
            )
          )
        )
        (setq i (1+ i))
      )

      ;; 2. Processar cada grupo de hachuras
      (foreach item groups
        (setq group_ents (cdr item))
        (if (and group_ents (entget (car group_ents)))
          (progn
            (setq lay_name (cdr (assoc 8 (entget (car group_ents)))))
            (setq is_pilar (= (strcase lay_name) "PP_PILAR"))
            (setq lay_def (chlay:find-layer lay_name predefined_layers))
            (setq props (if lay_def (cdr lay_def) nil))
            (setq hatch_pattern (cdr (assoc "HatchPattern" props)))
            (setq hatch_scale (cdr (assoc "HatchScale" props)))
            (setq hatch_transparency (cdr (assoc "HatchTransparency" props)))

            (cond
              ;; --- PP_PILAR: EXATAMENTE 2 HACHURAS (SOLID + ANSI37) ---
              (is_pilar
               (cond
                 ;; Caso 1: Apenas 1 hachura existe no local -> formata como SOLID (80) e cria cópia ANSI37 (0)
                 ((= (length group_ents) 1)
                  (setq h_solid (car group_ents))
                  (setq vla_solid (vlax-ename->vla-object h_solid))
                  
                  ;; Fundo: SOLID, Transparência 80
                  (vl-catch-all-apply
                    '(lambda ()
                       (vla-SetPattern vla_solid 1 "SOLID")
                       (vla-put-PatternAngle vla_solid 0.0)
                       (vla-evaluate vla_solid)
                     )
                  )
                  (chlay:set-transparency h_solid 80)
                  (vl-catch-all-apply
                    '(lambda ()
                       (setq none_color (vla-get-backgroundcolor vla_solid))
                       (if (and none_color (not (vl-catch-all-error-p none_color)))
                         (progn
                           (vla-put-entitycolor none_color -939524096)
                           (vla-put-backgroundcolor vla_solid none_color)
                         )
                       )
                     )
                  )

                  ;; Topo: ANSI37 (diagonal cruzada), Transparência 0
                  (setq vla_ansi (vl-catch-all-apply 'vla-Copy (list vla_solid)))
                  (if (and vla_ansi (not (vl-catch-all-error-p vla_ansi)))
                    (progn
                      (vl-catch-all-apply
                        '(lambda ()
                           (vla-SetPattern vla_ansi 1 (if hatch_pattern hatch_pattern "ANSI37"))
                           (vla-put-PatternAngle vla_ansi 0.0)
                           (vla-put-PatternScale vla_ansi (if (numberp hatch_scale) hatch_scale 0.03))
                           (vla-evaluate vla_ansi)
                         )
                      )
                      (setq h_ansi (vlax-vla-object->ename vla_ansi))
                      (chlay:set-transparency h_ansi 0)
                      (vl-catch-all-apply
                        '(lambda ()
                           (setq none_color (vla-get-backgroundcolor vla_ansi))
                           (if (and none_color (not (vl-catch-all-error-p none_color)))
                             (progn
                               (vla-put-entitycolor none_color -939524096)
                               (vla-put-backgroundcolor vla_ansi none_color)
                             )
                           )
                         )
                      )
                    )
                  )
                 )

                 ;; Caso 2: Existem 2 ou mais hachuras no local -> formata as 2 e DELETA todas as sobressalentes
                 (t
                  (setq h_solid (car group_ents))
                  (setq h_ansi (cadr group_ents))
                  (setq vla_solid (vlax-ename->vla-object h_solid))
                  (setq vla_ansi (vlax-ename->vla-object h_ansi))

                  ;; 1ª Hachura: SOLID Fundo (Transparência 80)
                  (vl-catch-all-apply
                    '(lambda ()
                       (vla-SetPattern vla_solid 1 "SOLID")
                       (vla-put-PatternAngle vla_solid 0.0)
                       (vla-evaluate vla_solid)
                     )
                  )
                  (chlay:set-transparency h_solid 80)
                  (vl-catch-all-apply
                    '(lambda ()
                       (setq none_color (vla-get-backgroundcolor vla_solid))
                       (if (and none_color (not (vl-catch-all-error-p none_color)))
                         (progn
                           (vla-put-entitycolor none_color -939524096)
                           (vla-put-backgroundcolor vla_solid none_color)
                         )
                       )
                     )
                  )

                  ;; 2ª Hachura: ANSI37 Topo (Transparência 0)
                  (vl-catch-all-apply
                    '(lambda ()
                       (vla-SetPattern vla_ansi 1 (if hatch_pattern hatch_pattern "ANSI37"))
                       (vla-put-PatternAngle vla_ansi 0.0)
                       (vla-put-PatternScale vla_ansi (if (numberp hatch_scale) hatch_scale 0.03))
                       (vla-evaluate vla_ansi)
                     )
                  )
                  (chlay:set-transparency h_ansi 0)
                  (vl-catch-all-apply
                    '(lambda ()
                       (setq none_color (vla-get-backgroundcolor vla_ansi))
                       (if (and none_color (not (vl-catch-all-error-p none_color)))
                         (progn
                           (vla-put-entitycolor none_color -939524096)
                           (vla-put-backgroundcolor vla_ansi none_color)
                         )
                       )
                     )
                  )

                  ;; DELETAR TODAS AS HACHURAS SOBRESSALENTES (da 3ª em diante)
                  (if (> (length group_ents) 2)
                    (foreach h_extra (cddr group_ents)
                      (if (and h_extra (entget h_extra))
                        (progn
                          (entdel h_extra)
                          (setq del_count (1+ del_count))
                        )
                      )
                    )
                  )
                 )
               )
              )

              ;; --- DEMAIS CAMADAS (incluindo PP_LAYOUT): EXATAMENTE 1 HACHURA POR LOCAL ---
              (t
               (setq h_solid (car group_ents))
               (setq vla_solid (vlax-ename->vla-object h_solid))
               (if hatch_pattern
                 (vl-catch-all-apply
                   '(lambda ()
                      (vla-SetPattern vla_solid 1 hatch_pattern)
                      (vla-put-PatternAngle vla_solid 0.0)
                      (if (and hatch_scale (numberp hatch_scale)) (vla-put-PatternScale vla_solid hatch_scale))
                      (vla-evaluate vla_solid)
                    )
                 )
               )
               (if hatch_transparency
                 (chlay:set-transparency h_solid hatch_transparency)
               )
               (vl-catch-all-apply
                 '(lambda ()
                    (setq none_color (vla-get-backgroundcolor vla_solid))
                    (if (and none_color (not (vl-catch-all-error-p none_color)))
                      (progn
                        (vla-put-entitycolor none_color -939524096)
                        (vla-put-backgroundcolor vla_solid none_color)
                      )
                    )
                  )
               )
               ;; Deletar quaisquer duplicatas extras
               (if (> (length group_ents) 1)
                 (foreach h_extra (cdr group_ents)
                   (if (and h_extra (entget h_extra))
                     (progn
                       (entdel h_extra)
                       (setq del_count (1+ del_count))
                     )
                   )
                 )
               )
              )
            )
          )
        )
      )
    )
  )
  del_count
)

;; Cria hachura a partir de uma polilinha fechada via VLA sem usar linha de comando
(defun chlay:create-polyline-hatch (ent / doc space vla-poly hatchObj outerLoop res old_cetrans)
  (vl-load-com)
  (setq res nil)
  (setq old_cetrans (getvar "CETRANSPARENCY"))
  (vl-catch-all-apply '(lambda () (setvar "CETRANSPARENCY" "80")))

  (setq res
    (vl-catch-all-apply
      '(lambda ()
         (setq doc (vla-get-activedocument (vlax-get-acad-object)))
         (setq space (vla-get-modelspace doc))
         (setq vla-poly (vlax-ename->vla-object ent))
         (setq hatchObj (vla-AddHatch space 1 "SOLID" :vlax-false))
         (setq outerLoop (vlax-make-safearray vlax-vbObject '(0 . 0)))
         (vlax-safearray-put-element outerLoop 0 vla-poly)
         (vla-AppendOuterLoop hatchObj outerLoop)
         (vla-Evaluate hatchObj)
         (vlax-vla-object->ename hatchObj)
       )
    )
  )
  (vl-catch-all-apply '(lambda () (setvar "CETRANSPARENCY" old_cetrans)))
  (if (and res (not (vl-catch-all-error-p res)) (entget res))
    res
    nil
  )
)

;; Caches de alto desempenho para evitar lookups e checagens redundantes
(setq *chlay-canonical-cache* nil)
(setq *chlay-verified-layers* nil)

(defun chlay:get-canonical-layer (target_name / cached lay_def target_canonical)
  (if (setq cached (assoc (strcase target_name) *chlay-canonical-cache*))
    (cdr cached)
    (progn
      (setq lay_def (chlay:find-layer target_name (chlay:get-predefined-layers)))
      (setq target_canonical (if lay_def (car lay_def) target_name))
      (setq *chlay-canonical-cache* (cons (cons (strcase target_name) target_canonical) *chlay-canonical-cache*))
      target_canonical
    )
  )
)

(defun chlay:ensure-target-layer (target_canonical / doc layers)
  (if (not (member (strcase target_canonical) *chlay-verified-layers*))
    (progn
      (if (not (tblsearch "LAYER" target_canonical))
        (vl-catch-all-apply 
          '(lambda () 
             (setq doc (vla-get-activedocument (vlax-get-acad-object)))
             (setq layers (vla-get-layers doc))
             (vla-add layers target_canonical)
           )
        )
      )
      (setq *chlay-verified-layers* (cons (strcase target_canonical) *chlay-verified-layers*))
    )
  )
)

;; Mover ou Copiar entidade individual diretamente para a camada alvo (ultra-rÃ¡pido)
(defun chlay:move-entity (ent target_name / vla-obj err copy-obj ent_to_format target_canonical)
  (if (and ent (entget ent) target_name (/= target_name ""))
    (progn
      ;; Identifica o nome canÃ´nico exato da camada de destino via cache O(1)
      (setq target_canonical (chlay:get-canonical-layer target_name))

      ;; Garante que a camada de destino exista no desenho (uma Ãºnica checagem)
      (chlay:ensure-target-layer target_canonical)

      (setq vla-obj (vlax-ename->vla-object ent))
      (setq ent_to_format ent)

      ;; Altera a camada e define propriedades ByLayer em chamada atÃ´mica
      (setq err (vl-catch-all-apply '(lambda () (vla-put-layer vla-obj target_canonical))))

      ;; Se a alteraÃ§Ã£o direta de camada falhar (objeto proxy ou restrito), fallback via cÃ³pia
      (if (vl-catch-all-error-p err)
        (progn
          (setq copy-obj (vl-catch-all-apply '(lambda () (vla-copy vla-obj))))
          (if (and copy-obj (not (vl-catch-all-error-p copy-obj)))
            (progn
              (vl-catch-all-apply '(lambda () (vla-put-layer copy-obj target_canonical)))
              (vl-catch-all-apply '(lambda () (vla-delete vla-obj)))
              (setq ent_to_format (vlax-vla-object->ename copy-obj))
              (setq vla-obj copy-obj)
            )
          )
        )
      )

      ;; Aplica propriedades ByLayer individualmente e com seguranÃ§a
      (vl-catch-all-apply '(lambda () (vla-put-color vla-obj 256)))
      (vl-catch-all-apply '(lambda () (vla-put-linetype vla-obj "ByLayer")))
      (vl-catch-all-apply '(lambda () (vla-put-linetypescale vla-obj 1.0)))
      (vl-catch-all-apply '(lambda () (vla-put-lineweight vla-obj -1)))

      ;; Aplicar formataÃ§Ã£o de hachuras APENAS para camadas que exigem padrÃ£o especÃ­fico
      (if (and ent_to_format 
               (entget ent_to_format) 
               (= (cdr (assoc 0 (entget ent_to_format))) "HATCH")
               (member (strcase target_canonical) '("PP_ARQUITETURA" "PP_PILAR" "PP_FORRO")))
        (chlay:format-hatch ent_to_format target_canonical)
      )
    )
  )
)

;; Aplica as propriedades padrão da camada lay_name à seleção de objetos ss (CHL Manual)
(defun chlay:apply-layer (ss lay_name / i ent count_hatch_del doc old_cmdecho predefined_layers lay_def target_canonical)
  (if (and ss lay_name (/= lay_name ""))
    (progn
      (vl-load-com)
      (setq doc (vla-get-activedocument (vlax-get-acad-object)))
      (vla-StartUndoMark doc)
      (setq old_cmdecho (getvar "CMDECHO"))
      (setvar "CMDECHO" 0)
      
      ;; 1. Garante e restaura/reafirma TODAS as camadas padrão PP com suas cores e tipos de linha
      (chlay:ensure-all-predefined-layers)
      
      ;; 2. Identifica o nome canônico da camada (tolerante a acentos e maiúsculas/minúsculas)
      (setq predefined_layers (chlay:get-predefined-layers))
      (setq lay_def (chlay:find-layer lay_name predefined_layers))
      (setq target_canonical (if lay_def (car lay_def) lay_name))
      
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (chlay:move-entity ent target_canonical)
        (setq i (1+ i))
      )
      ;; Deduplica apenas para PP_PILAR se aplicável
      (if (= (strcase target_canonical) "PP_PILAR")
        (setq count_hatch_del (chlay:deduplicate-hatches target_canonical))
        (setq count_hatch_del 0)
      )
      (vl-catch-all-apply '(lambda () (vla-regen doc acActiveViewport)))
      (if old_cmdecho (setvar "CMDECHO" old_cmdecho))
      (vla-EndUndoMark doc)
      (if (> count_hatch_del 0)
        (prompt (strcat "\n[CHL] " (itoa count_hatch_del) " hachura(s) sobreposta(s) sobressalente(s) removida(s)."))
      )
    )
  )
  (setvar "LTSCALE" 0.03)
  (setvar "CLAYER" "0")
  (princ)
)

;; Comando manual CHL: altera seleção para a camada digitada pelo usuário
(defun c:CHL (/ ss lay_name) 
  (setq ss (ssget "_I"))
  (setq lay_name (getstring T "\nDigite o nome da camada de destino: "))
  (if (not ss) 
    (progn 
      (prompt (strcat "\nSelecione os objetos para transferir para " (if (and lay_name (/= lay_name "")) lay_name "a camada") ": "))
      (setq ss (ssget))
    )
  )

  (if (and ss lay_name (/= lay_name "")) 
    (chlay:apply-layer ss lay_name)
    (if (not ss)
      (prompt "\nNenhum objeto selecionado.")
      (prompt "\nNome de camada inválido.")
    )
  )
  (princ)
)

;; Auxiliar: substitui ocorrências de uma substring em um texto
(defun chlay:replace-str (str old new / pos)
  (while (setq pos (vl-string-search old str))
    (setq str (strcat (substr str 1 pos) new (substr str (+ pos (strlen old) 1))))
  )
  str
)

;; Auxiliar: extrai o nome base da camada removendo prefixos de XREF (| ou $0$ ou $)
(defun chlay:get-base-layer (lay_name / pos)
  (setq lay_name (strcase lay_name))
  ;; 1. Remover prefixo de XREF padrão AutoCAD (ex: "01_BASE|I-FURN" -> "I-FURN")
  (if (setq pos (vl-string-search "|" lay_name))
    (while (setq pos (vl-string-search "|" lay_name))
      (setq lay_name (substr lay_name (+ pos 2)))
    )
  )
  ;; 2. Remover prefixo de XREF com bind $0$ (ex: "01_BASE$0$I-FURN" -> "I-FURN")
  (if (setq pos (vl-string-search "$0$" lay_name))
    (while (setq pos (vl-string-search "$0$" lay_name))
      (setq lay_name (substr lay_name (+ pos 4)))
    )
  )
  ;; 3. Remover prefixo $ genérico (ex: "01_BASE$I-FURN" -> "I-FURN")
  (if (setq pos (vl-string-search "$" lay_name))
    (while (setq pos (vl-string-search "$" lay_name))
      (setq lay_name (substr lay_name (+ pos 2)))
    )
  )
  lay_name
)

;; Auxiliar de correspondÃªncia rÃ¡pida com strings prÃ©-normalizadas
(defun chlay:fast-match-p (str_lay base_lay pat / str_pat wild1 wild2)
  (setq str_pat (vl-string-trim " \t\r\n" (strcase pat)))
  (cond
    ;; 1. Regra especÃ­fica para camada "0"
    ((= str_pat "0")
     (or (= str_lay "0") (= base_lay "0"))
    )

    ;; 2. PadrÃ£o iniciando com $0$ (ex: $0$ALVENARIA)
    ((vl-string-search "$0$" str_pat)
     (or (wcmatch str_lay (strcat "*" str_pat))
         (= base_lay (substr str_pat (+ (vl-string-search "$0$" str_pat) 4)))
     )
    )

    ;; 3. Igualdade exata no nome completo ou no nome base (sem prefixo XREF) ou wcmatch
    ((or (= str_lay str_pat)
         (= base_lay str_pat)
         (wcmatch str_lay str_pat)
         (wcmatch base_lay str_pat)
     )
     T
    )

    ;; 4. Se pat contÃ©m "____"
    ((vl-string-search "____" str_pat)
     (setq wild1 (chlay:replace-str str_pat "____" "*"))
     (setq wild2 (chlay:replace-str str_pat "-____-" "*"))
     (or (wcmatch str_lay wild1)
         (wcmatch str_lay wild2)
         (wcmatch base_lay wild1)
         (wcmatch base_lay wild2)
         (wcmatch str_lay (chlay:replace-str str_pat "-____" "*"))
         (wcmatch str_lay (chlay:replace-str str_pat "____-" "*"))
         (wcmatch base_lay (chlay:replace-str str_pat "-____" "*"))
         (wcmatch base_lay (chlay:replace-str str_pat "____-" "*"))
     )
    )

    ;; 5. Se pat contÃ©m "---" (ex: A---DIMS)
    ((vl-string-search "---" str_pat)
     (or (wcmatch str_lay (strcat (chlay:replace-str str_pat "---" "*") "*"))
         (wcmatch base_lay (strcat (chlay:replace-str str_pat "---" "*") "*"))
     )
    )

    (t nil)
  )
)

;; Auxiliar: verifica correspondÃªncia entre o nome da camada original no CAD e o padrÃ£o da regra
(defun chlay:layer-matches-p (lay_name pat / str_lay base_lay)
  (setq str_lay (vl-string-trim " \t\r\n" (strcase lay_name)))
  (setq base_lay (vl-string-trim " \t\r\n" (chlay:get-base-layer lay_name)))
  (chlay:fast-match-p str_lay base_lay pat)
)

;; Função auxiliar de resolução de ação por camada (usada pelo cache de camadas)
(defun chlay:resolve-layer-action (lay rules_del rules_mp / str_lay base_lay del_found target)
  (setq str_lay (vl-string-trim " \t\r\n" (strcase lay)))
  (setq base_lay (vl-string-trim " \t\r\n" (chlay:get-base-layer lay)))

  (cond
    ;; Condicional 1: MobiliÃ¡rio / I-FURN -> apaga hachuras; outros elementos vÃ£o para PP_LAYOUT
    ((or (chlay:fast-match-p str_lay base_lay "I-FURN")
         (chlay:fast-match-p str_lay base_lay "*FURN*")
         (chlay:fast-match-p str_lay base_lay "MÃ“VEIS")
         (chlay:fast-match-p str_lay base_lay "MOVEIS")
         (chlay:fast-match-p str_lay base_lay "*MOB*"))
     (cons "COND_FURN" "PP_LAYOUT")
    )

    ;; Condicional 2: A-DETL-GENF -> hachuras vÃ£o para PP_FORRO; outros elementos vÃ£o para PP_EQUIPAMENTO
    ((or (chlay:fast-match-p str_lay base_lay "A-DETL-GENF")
         (chlay:fast-match-p str_lay base_lay "*DETL-GENF*")
         (chlay:fast-match-p str_lay base_lay "*GENF*"))
     (cons "COND_GENF" "PP_EQUIPAMENTO")
    )

    ;; Condicional 3: TEXTO3 -> apaga apenas TEXT simples; MTEXT vai para PP_TEXTO APOIO
    ((chlay:fast-match-p str_lay base_lay "TEXTO3")
     (cons "COND_TEXTO3" "PP_TEXTO APOIO")
    )

    ;; Condicional 4: VAZIO -> apaga apenas HATCH; linhas/polilinhas vÃ£o para PP_LAYOUT
    ((chlay:fast-match-p str_lay base_lay "VAZIO")
     (cons "COND_VAZIO" "PP_LAYOUT")
    )

    ;; Condicional 5: A-AREA-IDEN -> move para PP_TEXTO (evita ser capturado por regras de exclusÃ£o de IDEN)
    ((or (chlay:fast-match-p str_lay base_lay "A-AREA-IDEN")
         (chlay:fast-match-p str_lay base_lay "*AREA-IDEN*")
         (chlay:fast-match-p str_lay base_lay "*AREA*IDEN*"))
     (cons "MOVE" "PP_TEXTO")
    )

    ;; Condicional 6: A-DETL-MBND / Vagas de Garagem -> apaga hachuras e textos; linhas/polilinhas vÃ£o para PP_GARAGEM
    ((or (chlay:fast-match-p str_lay base_lay "A-DETL-MBND")
         (chlay:fast-match-p str_lay base_lay "A-DETL-____-MBND")
         (chlay:fast-match-p str_lay base_lay "*DETL*MBND*")
         (chlay:fast-match-p str_lay base_lay "A-DETL-*-MBND"))
     (cons "COND_MBND" "PP_GARAGEM")
    )

    ;; Condicional 7: Selos de Zona -> textos e blocos vÃ£o para PP_TEXTO, linhas/polilinhas/hachuras sÃ£o apagadas
    ((or (chlay:fast-match-p str_lay base_lay "Selos de Zona*")
         (chlay:fast-match-p str_lay base_lay "*Selos de Zona*")
         (chlay:fast-match-p str_lay base_lay "*Selo*Zona*"))
     (cons "COND_ZON_TEXT" "PP_TEXTO")
    )

    ;; Condicional 8: Zonas (ZON _ Ambiente, ZON _ Ãreas e PerÃ­metros) -> textos e blocos vÃ£o para PP_TEXTO APOIO, linhas/polilinhas/hachuras sÃ£o apagadas
    ((and (or (chlay:fast-match-p str_lay base_lay "ZON _ *")
              (chlay:fast-match-p str_lay base_lay "*ZON _ Ambiente*")
              (chlay:fast-match-p str_lay base_lay "*ZON _ *rea*")
              (chlay:fast-match-p str_lay base_lay "*ZON*"))
          (not (chlay:fast-match-p str_lay base_lay "*APTO PCD*ZON*")))
     (cons "COND_ZON_APOIO" "PP_TEXTO APOIO")
    )

    ;; Caso padrÃ£o: verificar regras de exclusÃ£o e depois remapeamento
    (t
     (setq del_found nil)
     (foreach del_pat rules_del
       (if (and (not del_found) (chlay:fast-match-p str_lay base_lay del_pat))
         (setq del_found T)
       )
     )
     (if del_found
       (cons "DELETE" nil)
       (progn
         (setq target nil)
         (foreach map_entry rules_mp
           (if (and (not target) (chlay:fast-match-p str_lay base_lay (car map_entry)))
             (setq target (cdr map_entry))
           )
         )
         (if target
           (cons "MOVE" target)
           (cons "NONE" nil)
         )
       )
     )
    )
  )
)

;; Função auxiliar de limpeza automática baseada em regras de exclusão e remapeamento (CHLAUTO ultra-rápido com cache)
(defun chlay:clean-selection (ss / rules_delete rules_map i j ent elist ent_lay ent_type target_lay 
                                   count_del count_move count_ignored count_created 
                                   new_hatch old_cmdecho old_highlight doc
                                   existing_hatch_sigs ss_existing_hatches h_ent h_sig pline_sig
                                   layer_cache cache_entry action_code act_type u_lay
                             )
  (if ss
    (progn
      (vl-load-com)
      (setq doc (vla-get-activedocument (vlax-get-acad-object)))
      (vla-StartUndoMark doc)
      (setq old_cmdecho (getvar "CMDECHO"))
      (setq old_highlight (getvar "HIGHLIGHT"))
      (setvar "CMDECHO" 0)
      (if old_highlight (setvar "HIGHLIGHT" 0))
      (sssetfirst nil nil)
      (chlay:ensure-all-predefined-layers)
      
      ;; 1. Regras de exclusão estritas (tags de identificação, carimbos, lixos e camadas numéricas do projetista estrutural)
      (setq rules_delete '(
        ;; --- Camadas Numéricas a serem Excluídas (Projetista Estrutural) ---
        "225" "228" "218" "209" "207" "215" "230" "214" "237"
        "170" "213" "217" "245" "223" "204" "171" "224" "248"
        "226" "206" "242" "2" "70" "67" "63" "62" "66"
        "69" "65" "247" "5" "61" "235" "243" "251"
        "68" "197" "60" "220" "71" "161" "160" "64"

        ;; Tags / Identificadores especÃ­ficos a serem apagados
        "A-DOOR-IDEN" "A-DOOR-____-IDEN"
        "A-GLAZ-IDEN" "A-GLAZ-____-IDEN"
        "C-PRKG-IDEN" "C-PRKG-____-IDEN"
        "Q-CASE-IDEN" "Q-CASE-____-IDEN"
        "Q-SPCQ-IDEN" "Q-SPCQ-____-IDEN"
        "S-STRS-IDEN" "S-STRS-____-IDEN"

        ;; Marcadores de Porta e Tags de Janela a serem apagados
        "*Marcadores de Porta*" "*Marcador*Porta*" "*Marcador*Portas*"
        "*Marcadores de Janela*" "*Marcador*Janela*" "*Marcador*Janelas*"
        "*Marcador*" "*Marcadores*"
        "*TAG _ Porta*" "*TAG _ Portas*" "*TAG*Porta*" "*TAG*Portas*"
        "*TAG _ Janela*" "*TAG _ Janelas*" "*TAG*Janela*" "*TAG*Janelas*"
        "*DOOR*IDEN*" "*GLAZ*IDEN*" "*WIND*IDEN*"
        "*DOOR*TAG*" "*GLAZ*TAG*" "*WIND*TAG*"
        ;; Dimensões específicas a serem apagadas
        "A-ANNO-DIMS" "A-ANNO-DIMS*" "A-ANNO-DIMS-75" "A-ANNO-DIMS-200" "A---DIMS-50" "A---DIMS-300"

        ;; Elementos gerais / Carimbos / Tabelas / Símbolos / Eixos
        "G-IMPT"
        "S-GRID" "S-GRID-IDEN" "S-GRID-____-IDEN" "S-GRID-____-OTLN" "S-GRID-OTLN" "S-GRID*" "A-GRID*" "*GRID*" "EIXOS" "*EIXO*" "*EIXOS*"
        "G-ANNO-NPLT"
        "G-ANNO-TTLB" "G-ANNO-TTLB-MEDM" "G-ANNO-TTLB-WIDE" "G-ANNO-TTLB-THIN"
        "G-ANNO-SCHD"
        "G-ANNO-LEGN"
        "G-ANNO-SYMB" "G-____-____-SYMB"
        "G-XREF-____-OTLN" "G-XREF*"

        ;; Camadas legadas a serem apagadas
        "LINHA03"
        "LINHA 02"
        "LINHA02"
        "0L_CALL_DIM"
        "TEXTO1"
        "SIMBOLOS"
        "CORTE"
        "HACHURAS_MOVEIS"
        "HACHURAS LOUCAS"
        "HACHURA LOUCAS"
        "H-AS-CX"
        "H-AP-CX"
        "SETAS"
        "PISCINA"
        "TG_Symb"
        "CARIMBO"
        "*TAG _ Corte*"
        "*TAG _ Eleva*"
      ))
      
      ;; 2. Regras de remapeamento: '( ("CAMADA_ORIGEM_OU_PADRAO" . "CAMADA_DESTINO_PP") ... )
      ;; Organizadas por prioridade de especificidade
      (setq rules_map '(
        ;; --- PP_REGIÃO ---
        ("*PAVIMENTO TIPO*Guarda*Corpo*91*" . "PP_REGIÃO")
        ("PAVIMENTO TIPO ( 2º ao 14º )_1_OBJ _ Guarda-Corpo e Corrimão.3D_Caneta_No__91" . "PP_REGIÃO")
        ("*APTO PCD*ZON _ Ambientes.3D" . "PP_REGIÃO")
        ("*APTO PCD*ZON _ Ambientes.3D*No__5" . "PP_REGIÃO")
        ("APTO PCD_3_ZON _ Ambientes.3D" . "PP_REGIÃO")
        ("APTO PCD_3_ZON _ Ambientes.3D_Caneta_No__5" . "PP_REGIÃO")

        ;; --- PP_TEXTO PILAR (Prioridade máxima sobre regras de pilares) ---
        ("NOMEPILAR" . "PP_TEXTO PILAR")
        ("229" . "PP_TEXTO PILAR")
        ("S-COLS-____-IDEN" . "PP_TEXTO PILAR")
        ("S-COLS-IDEN" . "PP_TEXTO PILAR")
        ("*COLS*IDEN*" . "PP_TEXTO PILAR")
        ("*TEXTO PILAR*" . "PP_TEXTO PILAR")

        ;; --- PP_TEXTO VIGA (Prioridade máxima sobre regras de vigas) ---
        ("234" . "PP_TEXTO VIGA")
        ("227" . "PP_TEXTO VIGA")
        ("*BEAM*IDEN*" . "PP_TEXTO VIGA")
        ("*VIGA*IDEN*" . "PP_TEXTO VIGA")
        ("*TEXTO VIGA*" . "PP_TEXTO VIGA")

        ;; --- PP_TEXTO APOIO ---
        ("*ZON _ Ambiente*" . "PP_TEXTO APOIO")
        ("*ZON _ *reas e Per*metros*" . "PP_TEXTO APOIO")
        ("*TXT _ Legal*" . "PP_TEXTO APOIO")
        ("*TXT _ Matr*cula*" . "PP_TEXTO APOIO")
        ("*LEG _ Legenda*" . "PP_TEXTO APOIO")
        ("*TAG _ Shaft*" . "PP_TEXTO APOIO")
        ("*TAG*Shaft*" . "PP_TEXTO APOIO")
        ("*TAG _ *" . "PP_TEXTO APOIO")
        ("*TAG*" . "PP_TEXTO APOIO")
        ("G-ANNO-TEXT" . "PP_TEXTO APOIO")
        ("A-ANNO-NOTE" . "PP_TEXTO APOIO")
        ("A-____-NOTE-TEXT" . "PP_TEXTO APOIO")
        ("A-FLOR-LEVL" . "PP_TEXTO APOIO")
        ("S-STRS-ANNO" . "PP_TEXTO APOIO")
        ("TEXTO" . "PP_TEXTO APOIO")
        ("TEXTO2" . "PP_TEXTO APOIO")
        ("COMENTÁRIOS" . "PP_TEXTO APOIO")
        ("COMENTARIOS" . "PP_TEXTO APOIO")
        ("*LINHA_DE_CHAMADA*" . "PP_TEXTO APOIO")
        ("*ANNO-TEXT*" . "PP_TEXTO APOIO")
        ("*TEXTO APOIO*" . "PP_TEXTO APOIO")
        ("*COMENT*" . "PP_TEXTO APOIO")
        ("*NOTE*" . "PP_TEXTO APOIO")
        ("*LEVL*" . "PP_TEXTO APOIO")
        ("*NIVEL*" . "PP_TEXTO APOIO")

        ;; --- PP_TEXTO ---
        ("*Selos de Zona*" . "PP_TEXTO")
        ("A-AREA-IDEN" . "PP_TEXTO")
        ("A-AREA-____-IDEN" . "PP_TEXTO")
        ("G-____-____-TEXT" . "PP_TEXTO")
        ("*AREA-IDEN*" . "PP_TEXTO")
        ("*AREA*IDEN*" . "PP_TEXTO")
        ("*LEGENDA*" . "PP_TEXTO")
        ("*LEG*" . "PP_TEXTO")
        ("*TEXTO*" . "PP_TEXTO")
        ("*TXT*" . "PP_TEXTO")
        ("*DOC _ Textos e Rótulos*" . "PP_TEXTO")
        ("*DOC _ Textos e R*tulos*" . "PP_TEXTO")

        ;; --- PP_COTAS ---
        ("*CTA _ N*vel*" . "PP_COTAS")
        ("*CTA*" . "PP_COTAS")
        ("G-ANNO-DIMS" . "PP_COTAS")
        ("G-____-____-DIMS" . "PP_COTAS")
        ("A---DIMS" . "PP_COTAS")
        ("COTAS" . "PP_COTAS")
        ("*DIMS*" . "PP_COTAS")
        ("*COTA*" . "PP_COTAS")
        ("*COTAS*" . "PP_COTAS")
        ("*DOC _ Cotas*" . "PP_COTAS")

        ;; --- PP_PROJEÇÃO (ArchiCAD Shafts com prioridade sobre regras gerais) ---
        ("*OBJ _ Shafts e Vazios*" . "PP_PROJEÇÃO")
        ("*SFT _ Shaft*" . "PP_PROJEÇÃO")
        ("*SFT*" . "PP_PROJEÇÃO")
        ("*2D _ Proje*" . "PP_PROJEÇÃO")

        ;; --- PP_LAYOUT (Prioritários antes de Alvenaria) ---
        ("*Pele de Vidro*" . "PP_LAYOUT")
        ("*VED _ Pele de Vidro*" . "PP_LAYOUT")

        ;; --- PP_ARQUITETURA ---
        ("A-WALL" . "PP_ARQUITETURA")
        ("A-WALL-____-MCUT" . "PP_ARQUITETURA")
        ("A-WALL-____-OTLN" . "PP_ARQUITETURA")
        ("A-WALL-____-PATT" . "PP_ARQUITETURA")
        ("A-WALL-PATT" . "PP_ARQUITETURA")
        ("ALVENARIA" . "PP_ARQUITETURA")
        ("*VED _ Paredes de Alvenaria*" . "PP_ARQUITETURA")
        ("*VED _ Drywall*" . "PP_ARQUITETURA")
        ("*ALV _ Parede*" . "PP_ARQUITETURA")
        ("*ALV _ Garden*" . "PP_ARQUITETURA")
        ("*Parede baixa*" . "PP_ARQUITETURA")
        ("*Mureta*" . "PP_ARQUITETURA")
        ("*ACB _ Parede*" . "PP_ARQUITETURA")
        ("ENCHIMENTO" . "PP_ARQUITETURA")
        ("ACABAMENTO" . "PP_ARQUITETURA")
        ("ALV-LAJE" . "PP_ARQUITETURA")
        ("ALV-VIGA" . "PP_ARQUITETURA")
        ("$0$ALVENARIA" . "PP_ARQUITETURA")
        ("*WALL*" . "PP_ARQUITETURA")
        ("*ALVENARIA*" . "PP_ARQUITETURA")
        ("*ALV*" . "PP_ARQUITETURA")
        ("*PAREDE*" . "PP_ARQUITETURA")
        ("*REVESTIMENTO*" . "PP_ARQUITETURA")
        ("*REV*" . "PP_ARQUITETURA")
        ("*PEDRA*" . "PP_ARQUITETURA")
        ("*SHAFT*" . "PP_ARQUITETURA")
        ("*FACHADA*" . "PP_ARQUITETURA")

        ;; --- PP_PILAR ---
        ("*PIL _ Pilar*" . "PP_PILAR")
        ("*PIL*" . "PP_PILAR")
        ("S-COLS" . "PP_PILAR")
        ("S-COLS-____-MCUT" . "PP_PILAR")
        ("S-COLS-____-OTLN" . "PP_PILAR")
        ("ESTRUTURA-1" . "PP_PILAR")
        ("ESTRUTURA-3" . "PP_PILAR")
        ("240" . "PP_PILAR")
        ("*240*" . "PP_PILAR")
        ("232" . "PP_PILAR")
        ("233" . "PP_PILAR")
        ("*COLS*" . "PP_PILAR")
        ("*PILAR*" . "PP_PILAR")
        ("*COLUNA*" . "PP_PILAR")
        ("*BLOC*" . "PP_PILAR")

        ;; --- PP_VIGA ---
        ("S-BEAM" . "PP_VIGA")
        ("S-BEAM-____-MCUT" . "PP_VIGA")
        ("S-BEAM-____-OTLN" . "PP_VIGA")
        ("250" . "PP_VIGA")
        ("4" . "PP_VIGA")
        ("25" . "PP_VIGA")
        ("*BEAM*" . "PP_VIGA")
        ("*VIGA*" . "PP_VIGA")
        ("*VIG*" . "PP_VIGA")
        ("*EST _ Viga*" . "PP_VIGA")

        ;; --- PP_CUBETA ---
        ("200" . "PP_CUBETA")
        ("210" . "PP_CUBETA")
        ("*200*" . "PP_CUBETA")
        ("*210*" . "PP_CUBETA")
        ("*CUBETA*" . "PP_CUBETA")
        ("*NERVURA*" . "PP_CUBETA")

        ;; --- PP_FORRO ---
        ("A-CLNG" . "PP_FORRO")
        ("A-CLNG-PATT" . "PP_FORRO")
        ("A-DETL-GENF-OTLN" . "PP_FORRO")
        ("A-DETL-DEMO-OTLN" . "PP_FORRO")
        ("FORRO" . "PP_FORRO")
        ("*CLNG*" . "PP_FORRO")
        ("*FOR-HTC*" . "PP_FORRO")
        ("*FORRO*" . "PP_FORRO")
        ("*FOR-*" . "PP_FORRO")
        ("*FOR_*" . "PP_FORRO")
        ("*ACB _ Forro*" . "PP_FORRO")

        ;; --- PP_COBERTURA ---
        ("*COB _ Pergolado*" . "PP_COBERTURA")
        ("*Pergolado*" . "PP_COBERTURA")
        ("*COB*" . "PP_COBERTURA")
        ("A-ROOF" . "PP_COBERTURA")
        ("A-ROOF-OTLN" . "PP_COBERTURA")
        ("A-ROOF-PATT" . "PP_COBERTURA")
        ("A-ROOF-MCUT" . "PP_COBERTURA")
        ("A-ROOF-____-MCUT" . "PP_COBERTURA")
        ("A-ROOF-____-OTLN" . "PP_COBERTURA")
        ("A-ROOF-____-PATT" . "PP_COBERTURA")
        ("*ROOF*" . "PP_COBERTURA")
        ("*COBERTURA*" . "PP_COBERTURA")
        ("*COBERT*" . "PP_COBERTURA")
        ("*TELHA*" . "PP_COBERTURA")
        ("*TELHO*" . "PP_COBERTURA")

        ;; --- PP_GARAGEM ---
        ("*Estacionamento*" . "PP_GARAGEM")
        ("*Manobra*" . "PP_GARAGEM")
        ("*OBJ _ Vaga*" . "PP_GARAGEM")
        ("C-PRKG" . "PP_GARAGEM")
        ("C-PRKG-____-OTLN" . "PP_GARAGEM")
        ("A-DETL-MBND" . "PP_GARAGEM")
        ("A-DETL-____-MBND" . "PP_GARAGEM")
        ("*DETL*MBND*" . "PP_GARAGEM")
        ("VAGAS" . "PP_GARAGEM")
        ("HACHURA" . "PP_GARAGEM")
        ("*PRKG*" . "PP_GARAGEM")
        ("*VAGA*" . "PP_GARAGEM")
        ("*VAGAS*" . "PP_GARAGEM")
        ("*GARAGEM*" . "PP_GARAGEM")
        ("*GARAG*" . "PP_GARAGEM")

        ;; --- PP_VEGETAÇÃO ---
        ("*2D _ *rea - Verde*" . "PP_VEGETAÇÃO")
        ("*Perme*vel*" . "PP_VEGETAÇÃO")
        ("*VEG _ Grama*" . "PP_VEGETAÇÃO")
        ("*Grama*" . "PP_VEGETAÇÃO")
        ("L-PLNT" . "PP_VEGETAÇÃO")
        ("L-PLNT-____-OTLN" . "PP_VEGETAÇÃO")
        ("L-SITE" . "PP_VEGETAÇÃO")
        ("L-SITE-____-OTLN" . "PP_VEGETAÇÃO")
        ("TERRA" . "PP_VEGETAÇÃO")
        ("FORRAÇÃO" . "PP_VEGETAÇÃO")
        ("FORRACAO" . "PP_VEGETAÇÃO")
        ("*PLNT*" . "PP_VEGETAÇÃO")
        ("*SITE*" . "PP_VEGETAÇÃO")
        ("*VEGETAÇÃO*" . "PP_VEGETAÇÃO")
        ("*VEGETACAO*" . "PP_VEGETAÇÃO")
        ("*VEGETA*" . "PP_VEGETAÇÃO")
        ("*VEG*" . "PP_VEGETAÇÃO")
        ("*JARDIM*" . "PP_VEGETAÇÃO")
        ("*FORRAÇÃO*" . "PP_VEGETAÇÃO")
        ("*FORRACAO*" . "PP_VEGETAÇÃO")
        ("*PAI _ Paisagismo*" . "PP_VEGETAÇÃO")

        ;; --- PP_PROJEÇÃO ---
        ("A-DETL-DEMO" . "PP_PROJEÇÃO")
        ("A-FLOR-OVHD" . "PP_PROJEÇÃO")
        ("A-FLOR-____-OVHD" . "PP_PROJEÇÃO")
        ("_Acima_ Linha de corte do corrimão" . "PP_PROJEÇÃO")
        ("Acima Linha de corte do corrimão" . "PP_PROJEÇÃO")
        ("*ACIMA*" . "PP_PROJEÇÃO")
        ("*LINHA DE CORTE DO CORRIM*" . "PP_PROJEÇÃO")
        ("*LINHA DE CORTE*" . "PP_PROJEÇÃO")
        ("PROJEÇÕES" . "PP_PROJEÇÃO")
        ("PROJEÇÃO" . "PP_PROJEÇÃO")
        ("PROJECAO" . "PP_PROJEÇÃO")
        ("PROJECOES" . "PP_PROJEÇÃO")
        ("211" . "PP_PROJEÇÃO")
        ("*OVHD*" . "PP_PROJEÇÃO")
        ("*DEMO*" . "PP_PROJEÇÃO")
        ("*PROJEÇÃO*" . "PP_PROJEÇÃO")
        ("*PROJECAO*" . "PP_PROJEÇÃO")
        ("*PROJEC*" . "PP_PROJEÇÃO")

        ;; --- PP_EQUIPAMENTO ---
        ("E-ELEC-EQPM" . "PP_EQUIPAMENTO")
        ("E-ELEC-EQPM-OTLN" . "PP_EQUIPAMENTO")
        ("E-ELEC-FIXT" . "PP_EQUIPAMENTO")
        ("M-EQPM" . "PP_EQUIPAMENTO")
        ("M-EQPM-____-OTLN" . "PP_EQUIPAMENTO")
        ("P-PIPE" . "PP_EQUIPAMENTO")
        ("PP_FRIGORIFICA_EQUIPAMENTOS" . "PP_EQUIPAMENTO")
        ("PP_EXAUSTÃO_EQUIP" . "PP_EQUIPAMENTO")
        ("PP_EXAUSTÃO_DUTO" . "PP_EQUIPAMENTO")
        ("PP_FRIGORIFICA" . "PP_EQUIPAMENTO")
        ("H-TC-CX" . "PP_EQUIPAMENTO")
        ("H-AF-CX" . "PP_EQUIPAMENTO")
        ("H-INC-CX" . "PP_EQUIPAMENTO")
        ("QUADROS" . "PP_EQUIPAMENTO")
        ("LINHA01" . "PP_EQUIPAMENTO")
        ("CP-DIFUSÃO" . "PP_EQUIPAMENTO")
        ("TigreCad" . "PP_EQUIPAMENTO")
        ("Tinterno" . "PP_EQUIPAMENTO")
        ("*EQPM*" . "PP_EQUIPAMENTO")
        ("*ELEC*" . "PP_EQUIPAMENTO")
        ("*ELETRIC*" . "PP_EQUIPAMENTO")
        ("*PIPE*" . "PP_EQUIPAMENTO")
        ("*DUTO*" . "PP_EQUIPAMENTO")
        ("*EXAUST*" . "PP_EQUIPAMENTO")
        ("*FRIGORIF*" . "PP_EQUIPAMENTO")
        ("*TUB*" . "PP_EQUIPAMENTO")
        ("*HIDR*" . "PP_EQUIPAMENTO")
        ("*EQP*" . "PP_EQUIPAMENTO")
        ;; --- Reservatórios, Caixas Elétricas, Barriletes, Bombas e Passagens ---
        ("*RESERV*" . "PP_EQUIPAMENTO")
        ("*CISTERNA*" . "PP_EQUIPAMENTO")
        ("*BARRILETE*" . "PP_EQUIPAMENTO")
        ("*CAIXA*AGUA*" . "PP_EQUIPAMENTO")
        ("*CAIXA*ÁGUA*" . "PP_EQUIPAMENTO")
        ("*CX*AGUA*" . "PP_EQUIPAMENTO")
        ("*CX*ÁGUA*" . "PP_EQUIPAMENTO")
        ("*CAIXA*ELET*" . "PP_EQUIPAMENTO")
        ("*CX*ELET*" . "PP_EQUIPAMENTO")
        ("*QUADRO*ELET*" . "PP_EQUIPAMENTO")
        ("*QD*ELET*" . "PP_EQUIPAMENTO")
        ("*CAIXA*PASSAGEM*" . "PP_EQUIPAMENTO")
        ("*CX*PASSAGEM*" . "PP_EQUIPAMENTO")
        ("*PASSAGEM*" . "PP_EQUIPAMENTO")
        ("*INSPEC*" . "PP_EQUIPAMENTO")
        ("*BOMBA*" . "PP_EQUIPAMENTO")

        ;; --- PP_LAYOUT ---
        ("0" . "PP_LAYOUT")
        ("C-TOPO" . "PP_LAYOUT")
        ("C-TOPO-PATT" . "PP_LAYOUT")
        ("C-TOPO-MCUT" . "PP_LAYOUT")
        ("*TOPO*" . "PP_LAYOUT")
        ("C-PROP-LINE" . "PP_LAYOUT")
        ("C-PROP-LINE-OTLN" . "PP_LAYOUT")
        ("*PROP-LINE*" . "PP_LAYOUT")
        ("*DIVISA*" . "PP_LAYOUT")
        ("A-FLOR" . "PP_LAYOUT")
        ("A-FLOR-MCUT" . "PP_LAYOUT")
        ("A-FLOR-PATT" . "PP_LAYOUT")
        ("A-FLOR-____-MCUT" . "PP_LAYOUT")
        ("A-FLOR-____-OTLN" . "PP_LAYOUT")
        ("PISO" . "PP_LAYOUT")
        ("PISO-1" . "PP_LAYOUT")
        ("*FLOR*" . "PP_LAYOUT")
        ("*FLOOR*" . "PP_LAYOUT")
        ("*PISO*" . "PP_LAYOUT")
        ("A-DOOR" . "PP_LAYOUT")
        ("A-DOOR-FRAM" . "PP_LAYOUT")
        ("A-DOOR-FRAM-MCUT" . "PP_LAYOUT")
        ("A-DOOR-FRAM-OTLN" . "PP_LAYOUT")
        ("A-DOOR-GLAZ-MCUT" . "PP_LAYOUT")
        ("A-DOOR-HDLN" . "PP_LAYOUT")
        ("A-DOOR-____-HDLN" . "PP_LAYOUT")
        ("A-DOOR-____-MCUT" . "PP_LAYOUT")
        ("A-DOOR-____-OTLN" . "PP_LAYOUT")
        ("PORTAS" . "PP_LAYOUT")
        ("*DOOR*" . "PP_LAYOUT")
        ("*PORTA*" . "PP_LAYOUT")
        ("A-GLAZ" . "PP_LAYOUT")
        ("A-GLAZ-CURT" . "PP_LAYOUT")
        ("A-GLAZ-CWMG" . "PP_LAYOUT")
        ("A-GLAZ-CWMG-OTLN" . "PP_LAYOUT")
        ("A-GLAZ-____-OTLN" . "PP_LAYOUT")
        ("A-GLAZ-____-MCUT" . "PP_LAYOUT")
        ("CAIXILHOS" . "PP_LAYOUT")
        ("*GLAZ*" . "PP_LAYOUT")
        ("*JANELA*" . "PP_LAYOUT")
        ("*ESQUADRIA*" . "PP_LAYOUT")
        ("*VIDRO*" . "PP_LAYOUT")
        ("*CAIXILHO*" . "PP_LAYOUT")
        ("Q-CASE" . "PP_LAYOUT")
        ("Q-CASE-____-OTLN" . "PP_LAYOUT")
        ("*CASE*" . "PP_LAYOUT")
        ("*ARMARIO*" . "PP_LAYOUT")
        ("*MARCENARIA*" . "PP_LAYOUT")
        ("P-SANR-FIXT" . "PP_LAYOUT")
        ("P-SANR-FIXT-HDLN" . "PP_LAYOUT")
        ("P-SANR-FIXT-OTLN" . "PP_LAYOUT")
        ("LOUCAS" . "PP_LAYOUT")
        ("*SANR*" . "PP_LAYOUT")
        ("*LOUCA*" . "PP_LAYOUT")
        ("*LOUÇA*" . "PP_LAYOUT")
        ("*FIXT*" . "PP_LAYOUT")
        ("A-MEDQ" . "PP_LAYOUT")
        ("A-MEDQ-____-OTLN" . "PP_LAYOUT")
        ("*MEDQ*" . "PP_LAYOUT")
        ("A-FDSQ" . "PP_LAYOUT")
        ("*FDSQ*" . "PP_LAYOUT")
        ("A-GENM" . "PP_LAYOUT")
        ("A-GENM-____-OTLN" . "PP_LAYOUT")
        ("A-GENM-____-HDLN" . "PP_LAYOUT")
        ("*GENM*" . "PP_LAYOUT")
        ("Q-SPCQ" . "PP_LAYOUT")
        ("Q-SPCQ-____-OTLN" . "PP_LAYOUT")
        ("*SPCQ*" . "PP_LAYOUT")
        ("S-STRS" . "PP_LAYOUT")
        ("S-STRS-MBND" . "PP_LAYOUT")
        ("S-STRS-____-MBND" . "PP_LAYOUT")
        ("S-STRS-____-OTLN" . "PP_LAYOUT")
        ("Guarda-corpos" . "PP_LAYOUT")
        ("Balaústres" . "PP_LAYOUT")
        ("Corrimão" . "PP_LAYOUT")
        ("*STRS*" . "PP_LAYOUT")
        ("*ESCADA*" . "PP_LAYOUT")
        ("*CORRIM*" . "PP_LAYOUT")
        ("*BALAUST*" . "PP_LAYOUT")
        ("*GUARDA*" . "PP_LAYOUT")
        ("A-DETL" . "PP_LAYOUT")
        ("A-DETL-THIN" . "PP_LAYOUT")
        ("A-DETL-MEDM" . "PP_LAYOUT")
        ("A-DETL-____-MEDM" . "PP_LAYOUT")
        ("A-DETL-____-OTLN" . "PP_LAYOUT")
        ("A-DETL-____-THIN" . "PP_LAYOUT")
        ("*DETL*" . "PP_LAYOUT")
        ("I-FURN" . "PP_LAYOUT")
        ("I-FURN-____-OTLN" . "PP_LAYOUT")
        ("MÓVEIS" . "PP_LAYOUT")
        ("MOVEIS" . "PP_LAYOUT")
        ("*FURN*" . "PP_LAYOUT")
        ("*MOB*" . "PP_LAYOUT")
        ("LINHA015" . "PP_LAYOUT")
        ("LINHA025" . "PP_LAYOUT")
        ("LINHA 01" . "PP_LAYOUT")
        ("H-INC-TB" . "PP_LAYOUT")
        ("BLOCO" . "PP_LAYOUT")
        ("E-NURS" . "PP_LAYOUT")
        ("E-NURS-____-OTLN" . "PP_LAYOUT")
        ("E-NURS*" . "PP_LAYOUT")
        ("*NURS*" . "PP_LAYOUT")
        ("L-HRDS" . "PP_LAYOUT")
        ("L-HRDS-____-OTLN" . "PP_LAYOUT")
        ("L-HRDS-____-PATT" . "PP_LAYOUT")
        ("L-HRDS-PATT" . "PP_LAYOUT")
        ("L-HRDS*" . "PP_LAYOUT")
        ("*HRDS*" . "PP_LAYOUT")
        ;; --- ArchiCAD Layout ---
        ("0_Caneta_No__*" . "PP_LAYOUT")
        ("*LAJ _ Laje*" . "PP_LAYOUT")
        ("*LAJ*" . "PP_LAYOUT")
        ("*RMP _ Rampa*" . "PP_LAYOUT")
        ("*RMP*" . "PP_LAYOUT")
        ("*ESC _ Escada*" . "PP_LAYOUT")
        ("*ESQ _ Gradil*" . "PP_LAYOUT")
        ("*ESQ _ Guarda-corpo*" . "PP_LAYOUT")
        ("*ESQ*" . "PP_LAYOUT")
        ("*GRADIL*" . "PP_LAYOUT")
        ("*MOB _ PcD*" . "PP_LAYOUT")
        ("*SMB _ PcD*" . "PP_LAYOUT")
        ("*SMB*" . "PP_LAYOUT")
        ("*OBJ _ Elevador*" . "PP_LAYOUT")
        ("*OBJ _ Lixo*" . "PP_LAYOUT")
        ("*OBJ _ Churrasqueira*" . "PP_LAYOUT")
        ("*OBJ*" . "PP_LAYOUT")
        ("*PAV _ Guia*" . "PP_LAYOUT")
        ("*Sarjeta*" . "PP_LAYOUT")
        ("*PAV*" . "PP_LAYOUT")
        ("*EFS _ Lou*a*" . "PP_LAYOUT")
        ("*EFS _ Metal*" . "PP_LAYOUT")
        ("*EFS*" . "PP_LAYOUT")
        ("*PDR _ Peitoril*" . "PP_LAYOUT")
        ("*PDR _ Bancada*" . "PP_LAYOUT")
        ("*PDR _ Soleira*" . "PP_LAYOUT")
        ("*PDR _ Filete*" . "PP_LAYOUT")
        ("*PDR _ Borda de piscina*" . "PP_LAYOUT")
        ("*PDR*" . "PP_LAYOUT")
        ("*2D _ Cal*ada*" . "PP_LAYOUT")
        ("*2D _ Recuo*" . "PP_LAYOUT")
        ("*2D _ Espa*o m*nimo*" . "PP_LAYOUT")
        ("*CMP _ Planialti*" . "PP_LAYOUT")
        ("*Janelas Archicad*" . "PP_LAYOUT")
        ("*Portas Archicad*" . "PP_LAYOUT")
        ("*OBJ _ Eletrodom*" . "PP_LAYOUT")
        ("*OBJ _ Mobili*" . "PP_LAYOUT")
        ("*PED _ Bancadas*" . "PP_LAYOUT")
        ("*OBJ _ Lou*as e Metais*" . "PP_LAYOUT")
        ("*OBJ _ Elevadores e plataformas*" . "PP_LAYOUT")
        ("*EST _ Escadas e Rampas*" . "PP_LAYOUT")
        ("*OBJ _ Guarda-Corpo e Corrim*" . "PP_LAYOUT")
        ("*PAGINA*" . "PP_LAYOUT")
        ("*LAYOUT*" . "PP_LAYOUT")
      ))
      
      (setq count_del 0)
      (setq count_move 0)
      (setq count_ignored 0)
      (setq layer_cache nil)
      (setq i 0)
      
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (if (and ent (setq elist (entget ent)))
          (progn
            (setq ent_lay (cdr (assoc 8 elist)))
            (setq ent_type (cdr (assoc 0 elist)))
            (setq u_lay (strcase ent_lay))
            
            ;; Consulta rÃ¡pida no cache de decisÃµes de camada (O(1))
            (setq cache_entry (assoc u_lay layer_cache))
            (if (not cache_entry)
              (progn
                (setq cache_entry (cons u_lay (chlay:resolve-layer-action ent_lay rules_delete rules_map)))
                (setq layer_cache (cons cache_entry layer_cache))
              )
            )
            (setq action_code (cdr cache_entry))
            (setq act_type (car action_code))
            
            ;; Execução direta da ação pré-resolvida
            (cond
              ;; Exclusão direta da entidade (entdel é instantâneo em C)
              ((= act_type "DELETE")
               (entdel ent)
               (setq count_del (1+ count_del))
              )

              ;; Condicional Mobiliário: hachura apaga, resto vai para PP_LAYOUT
              ((= act_type "COND_FURN")
               (if (= ent_type "HATCH")
                 (progn
                   (entdel ent)
                   (setq count_del (1+ count_del))
                 )
                 (progn
                   (chlay:move-entity ent "PP_LAYOUT")
                   (setq count_move (1+ count_move))
                 )
               )
              )

              ;; Condicional Detalhe/Forro: hachura vai para PP_FORRO, resto para PP_EQUIPAMENTO
              ((= act_type "COND_GENF")
               (if (= ent_type "HATCH")
                 (progn
                   (chlay:move-entity ent "PP_FORRO")
                   (setq count_move (1+ count_move))
                 )
                 (progn
                   (chlay:move-entity ent "PP_EQUIPAMENTO")
                   (setq count_move (1+ count_move))
                 )
               )
              )

              ;; Condicional TEXTO3: TEXT simples apaga, MTEXT vai para PP_TEXTO APOIO
              ((= act_type "COND_TEXTO3")
               (if (= ent_type "TEXT")
                 (progn
                   (entdel ent)
                   (setq count_del (1+ count_del))
                 )
                 (progn
                   (chlay:move-entity ent "PP_TEXTO APOIO")
                   (setq count_move (1+ count_move))
                 )
               )
              )

              ;; Condicional VAZIO: hachura apaga, resto vai para PP_LAYOUT
              ((= act_type "COND_VAZIO")
               (if (= ent_type "HATCH")
                 (progn
                   (entdel ent)
                   (setq count_del (1+ count_del))
                 )
                 (progn
                   (chlay:move-entity ent "PP_LAYOUT")
                   (setq count_move (1+ count_move))
                 )
               )
              )

              ;; Condicional MBND (Demarcação de Vagas): hachura e texto apagam, linhas/polilinhas vão para PP_GARAGEM
              ((= act_type "COND_MBND")
               (if (or (= ent_type "HATCH")
                       (member ent_type '("TEXT" "MTEXT" "MULTILEADER" "LEADER")))
                 (progn
                   (entdel ent)
                   (setq count_del (1+ count_del))
                 )
                 (progn
                   (chlay:move-entity ent "PP_GARAGEM")
                   (setq count_move (1+ count_move))
                 )
               )
              )

              ;; Condicional Selos de Zona: textos/blocos vão para PP_TEXTO, linhas/polilinhas/hachuras apagam
              ((= act_type "COND_ZON_TEXT")
               (if (member ent_type '("TEXT" "MTEXT" "INSERT" "MULTILEADER" "LEADER"))
                 (progn
                   (chlay:move-entity ent "PP_TEXTO")
                   (setq count_move (1+ count_move))
                 )
                 (progn
                   (entdel ent)
                   (setq count_del (1+ count_del))
                 )
               )
              )

              ;; Condicional Zonas: textos/blocos vão para PP_TEXTO APOIO, linhas/polilinhas/hachuras apagam
              ((= act_type "COND_ZON_APOIO")
               (if (member ent_type '("TEXT" "MTEXT" "INSERT" "MULTILEADER" "LEADER"))
                 (progn
                   (chlay:move-entity ent "PP_TEXTO APOIO")
                   (setq count_move (1+ count_move))
                 )
                 (progn
                   (entdel ent)
                   (setq count_del (1+ count_del))
                 )
               )
              )

              ;; Remapeamento de camada
              ((= act_type "MOVE")
               (setq target_lay (cdr action_code))
               (cond
                 ;; ExceÃ§Ã£o HACHURAS e TEXTOS para PP_GARAGEM:
                 ;; Se for HATCH ou TEXTO e estiver sendo transferido para PP_GARAGEM de outra camada, apaga.
                 ((and (= (strcase target_lay) "PP_GARAGEM")
                       (or (= ent_type "HATCH")
                           (member ent_type '("TEXT" "MTEXT" "MULTILEADER" "LEADER")))
                       (/= (strcase ent_lay) "PP_GARAGEM"))
                  (entdel ent)
                  (setq count_del (1+ count_del))
                 )

                 ;; ExceÃ§Ã£o HACHURA para PP_LAYOUT:
                 ;; Se for HATCH e estiver sendo transferida para PP_LAYOUT de outra camada, apaga.
                 ((and (= (strcase target_lay) "PP_LAYOUT")
                       (= ent_type "HATCH")
                       (/= (strcase ent_lay) "PP_LAYOUT"))
                  (entdel ent)
                  (setq count_del (1+ count_del))
                 )

                 ;; Demais elementos a mover
                 (t
                  (if (/= (strcase ent_lay) (strcase target_lay))
                    (progn
                      (chlay:move-entity ent target_lay)
                      (setq count_move (1+ count_move))
                    )
                  )

                  ;; Se for pilar e polilinha, garante fechamento (Closed = Yes via DXF 70 rápido)
                  (if (= (strcase target_lay) "PP_PILAR")
                    (if (member ent_type '("LWPOLYLINE" "POLYLINE"))
                      (if (and (assoc 70 elist) (numberp (cdr (assoc 70 elist))))
                        (if (= (logand (cdr (assoc 70 elist)) 1) 0)
                          (entmod (subst (cons 70 (logior (cdr (assoc 70 elist)) 1)) (assoc 70 elist) elist))
                        )
                      )
                    )
                  )
                 )
               )
              )

              ;; Nenhuma regra aplicada / camada desconhecida -> mantido intacto
              (t 
               (setq count_ignored (1+ count_ignored))
              )
            )
          )
        )
        (setq i (1+ i))
      )
      
      (vl-catch-all-apply '(lambda () (vla-regen doc acActiveViewport)))
      (if old_highlight (setvar "HIGHLIGHT" old_highlight))
      (if old_cmdecho (setvar "CMDECHO" old_cmdecho))
      (vla-EndUndoMark doc)
      
      (prompt (strcat "\n[CHLAUTO] Limpeza concluída: " 
                      (itoa count_move) " objeto(s) modificado(s), " 
                      (itoa count_del) " excluído(s), " 
                      (itoa count_ignored) " ignorado(s)."
              )
      )
    )
  )
  (setvar "LTSCALE" 0.03)
  (setvar "CLAYER" "0")
  (princ)
)

;; Comando automático CHLAUTO: limpa a seleção deletando elementos das camadas configuradas, remapeando para o padrão PP e limpando hachuras extras
(defun c:CHLAUTO (/ ss)
  (setq ss (ssget "_I"))
  (if (not ss)
    (progn
      (prompt "\n[CHLAUTO] Selecione os objetos para limpeza automática (ou ENTER para todo o ModelSpace): ")
      (setq ss (ssget))
    )
  )
  (if (not ss)
    (progn
      (prompt "\n[CHLAUTO] Nenhum objeto selecionado manualmente. Processando todo o ModelSpace...")
      (setq ss (ssget "_X" '((410 . "Model"))))
    )
  )
  (sssetfirst nil nil)
  (if ss
    (chlay:clean-selection ss)
    (prompt "\n[CHLAUTO] Nenhum objeto selecionado.")
  )
  (princ)
)

;; Alias de compatibilidade: Comando LBR executa o CHLAUTO com as mesmas regras unificadas
(defun c:LBR ()
  (c:CHLAUTO)
)

;; Comando turbinado CHLHATCH: fecha polilinhas de pilares, cria novas hachuras onde faltar, e padroniza/deduplica
(defun c:CHLHATCH (/ ss_user ss_hatches i ent elist ent_type pline_sig 
                     count_closed count_created count_del old_cmdecho doc
                     existing_hatch_sigs h_ent h_sig new_hatch
                  )
  (vl-load-com)
  (setq doc (vla-get-activedocument (vlax-get-acad-object)))
  (vla-StartUndoMark doc)
  (setq old_cmdecho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (chlay:ensure-all-predefined-layers)
  
  (setq count_closed 0)
  (setq count_created 0)
  (setq count_del 0)
  
  (prompt "\n[CHLHATCH] Selecione os pilares/hachuras (ou ENTER para verificar todo o ModelSpace): ")
  (setq ss_user (ssget "_I"))
  (if (not ss_user)
    (setq ss_user (ssget))
  )
  (if (not ss_user)
    (progn
      (prompt "\n[CHLHATCH] Processando pilares e hachuras em todo o ModelSpace...")
      (setq ss_user (ssget "_X" '((410 . "Model"))))
    )
  )
  
  (if ss_user
    (progn
      ;; 1. Mapear todas as hachuras existentes em PP_PILAR
      (setq existing_hatch_sigs nil)
      (if (setq ss_hatches (ssget "_X" '((0 . "HATCH") (8 . "PP_PILAR"))))
        (progn
          (setq i 0)
          (while (< i (sslength ss_hatches))
            (setq h_ent (ssname ss_hatches i))
            (setq h_sig (chlay:get-hatch-sig h_ent))
            (if (and h_sig (not (member h_sig existing_hatch_sigs)))
              (setq existing_hatch_sigs (cons h_sig existing_hatch_sigs))
            )
            (setq i (1+ i))
          )
        )
      )
      
      ;; 2. Varrer polilinhas em PP_PILAR na seleção para fechar e criar hachuras faltantes
      (setq i 0)
      (while (< i (sslength ss_user))
        (setq ent (ssname ss_user i))
        (if (and ent (setq elist (entget ent)))
          (progn
            (if (= (strcase (cdr (assoc 8 elist))) "PP_PILAR")
              (progn
                (setq ent_type (cdr (assoc 0 elist)))
                (if (member ent_type '("LWPOLYLINE" "POLYLINE"))
                  (progn
                    ;; Fechar polilinha se estiver aberta
                    (if (and (assoc 70 elist) (numberp (cdr (assoc 70 elist))))
                      (if (= (logand (cdr (assoc 70 elist)) 1) 0)
                        (progn
                          (entmod (subst (cons 70 (logior (cdr (assoc 70 elist)) 1)) (assoc 70 elist) elist))
                          (setq count_closed (1+ count_closed))
                        )
                      )
                    )
                    ;; Verificar se já possui hachura
                    (setq pline_sig (chlay:get-pline-sig ent))
                    (if (and pline_sig (not (member pline_sig existing_hatch_sigs)))
                      (progn
                        (setq new_hatch (chlay:create-polyline-hatch ent))
                        (if (and new_hatch (entget new_hatch) (= (cdr (assoc 0 (entget new_hatch))) "HATCH"))
                          (progn
                            (chlay:move-entity new_hatch "PP_PILAR")
                            (setq existing_hatch_sigs (cons pline_sig existing_hatch_sigs))
                            (setq count_created (1+ count_created))
                          )
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
      
      ;; 3. Deduplicar e padronizar hachuras no desenho
      (setq count_del (chlay:deduplicate-hatches nil))
    )
  )
  
  (setvar "CMDECHO" old_cmdecho)
  (vla-EndUndoMark doc)
  
  (prompt (strcat "\n[CHLHATCH] Concluído: " 
                  (itoa count_closed) " polilinha(s) fechada(s), " 
                  (itoa count_created) " nova(s) hachura(s) de pilar criada(s), " 
                  (itoa count_del) " hachura(s) sobressalente(s) removida(s)."
          )
  )
  (princ)
)

;; Função de exclusão pura baseada nas regras de limpeza (CHLDEL)
(defun chlay:delete-selection (ss / rules_delete rules_map i ent elist ent_lay ent_type 
                                    count_del count_kept old_cmdecho old_highlight doc
                                    layer_cache cache_entry action_code act_type u_lay
                              )
  (if ss
    (progn
      (vl-load-com)
      (setq doc (vla-get-activedocument (vlax-get-acad-object)))
      (vla-StartUndoMark doc)
      (setq old_cmdecho (getvar "CMDECHO"))
      (setq old_highlight (getvar "HIGHLIGHT"))
      (setvar "CMDECHO" 0)
      (if old_highlight (setvar "HIGHLIGHT" 0))
      (sssetfirst nil nil)
      
      ;; 1. Regras de exclusão estritas
      (setq rules_delete '(
        ;; --- Camadas Numéricas a serem Excluídas (Projetista Estrutural) ---
        "225" "228" "218" "209" "207" "215" "230" "214" "237"
        "170" "213" "217" "245" "223" "204" "171" "224" "248"
        "226" "206" "242" "2" "70" "67" "63" "62" "66"
        "69" "65" "247" "5" "61" "235" "243" "251"
        "68" "197" "60" "220" "71" "161" "160" "64"

        ;; Tags / Identificadores especÃ­ficos a serem apagados
        "A-DOOR-IDEN" "A-DOOR-____-IDEN"
        "A-GLAZ-IDEN" "A-GLAZ-____-IDEN"
        "C-PRKG-IDEN" "C-PRKG-____-IDEN"
        "Q-CASE-IDEN" "Q-CASE-____-IDEN"
        "Q-SPCQ-IDEN" "Q-SPCQ-____-IDEN"
        "S-STRS-IDEN" "S-STRS-____-IDEN"

        ;; Marcadores de Porta e Tags de Janela a serem apagados
        "*Marcadores de Porta*" "*Marcador*Porta*" "*Marcador*Portas*"
        "*Marcadores de Janela*" "*Marcador*Janela*" "*Marcador*Janelas*"
        "*Marcador*" "*Marcadores*"
        "*TAG _ Porta*" "*TAG _ Portas*" "*TAG*Porta*" "*TAG*Portas*"
        "*TAG _ Janela*" "*TAG _ Janelas*" "*TAG*Janela*" "*TAG*Janelas*"
        "*DOOR*IDEN*" "*GLAZ*IDEN*" "*WIND*IDEN*"
        "*DOOR*TAG*" "*GLAZ*TAG*" "*WIND*TAG*"
        ;; Dimensões específicas a serem apagadas
        "A-ANNO-DIMS" "A-ANNO-DIMS*" "A-ANNO-DIMS-75" "A-ANNO-DIMS-200" "A---DIMS-50" "A---DIMS-300"

        ;; Elementos gerais / Carimbos / Tabelas / Símbolos / Eixos
        "G-IMPT"
        "S-GRID" "S-GRID-IDEN" "S-GRID-____-IDEN" "S-GRID-____-OTLN" "S-GRID-OTLN" "S-GRID*" "A-GRID*" "*GRID*" "EIXOS" "*EIXO*" "*EIXOS*"
        "G-ANNO-NPLT"
        "G-ANNO-TTLB" "G-ANNO-TTLB-MEDM" "G-ANNO-TTLB-WIDE" "G-ANNO-TTLB-THIN"
        "G-ANNO-SCHD"
        "G-ANNO-LEGN"
        "G-ANNO-SYMB" "G-____-____-SYMB"
        "G-XREF-____-OTLN" "G-XREF*"

        ;; Camadas legadas a serem apagadas
        "LINHA03"
        "LINHA 02"
        "LINHA02"
        "0L_CALL_DIM"
        "TEXTO1"
        "SIMBOLOS"
        "CORTE"
        "HACHURAS_MOVEIS"
        "HACHURAS LOUCAS"
        "HACHURA LOUCAS"
        "H-AS-CX"
        "H-AP-CX"
        "SETAS"
        "PISCINA"
        "TG_Symb"
        "CARIMBO"
        "*TAG _ Corte*"
        "*TAG _ Eleva*"
      ))
      
      ;; 2. Regras de remapeamento
      (setq rules_map '(
        ;; --- PP_REGIÃO ---
        ("*PAVIMENTO TIPO*Guarda*Corpo*91*" . "PP_REGIÃO")
        ("PAVIMENTO TIPO ( 2º ao 14º )_1_OBJ _ Guarda-Corpo e Corrimão.3D_Caneta_No__91" . "PP_REGIÃO")
        ("*APTO PCD*ZON _ Ambientes.3D" . "PP_REGIÃO")
        ("*APTO PCD*ZON _ Ambientes.3D*No__5" . "PP_REGIÃO")
        ("APTO PCD_3_ZON _ Ambientes.3D" . "PP_REGIÃO")
        ("APTO PCD_3_ZON _ Ambientes.3D_Caneta_No__5" . "PP_REGIÃO")

        ;; --- PP_TEXTO PILAR (Prioridade máxima sobre regras de pilares) ---
        ("NOMEPILAR" . "PP_TEXTO PILAR")
        ("229" . "PP_TEXTO PILAR")
        ("S-COLS-____-IDEN" . "PP_TEXTO PILAR")
        ("S-COLS-IDEN" . "PP_TEXTO PILAR")
        ("*COLS*IDEN*" . "PP_TEXTO PILAR")
        ("*TEXTO PILAR*" . "PP_TEXTO PILAR")

        ;; --- PP_TEXTO VIGA (Prioridade máxima sobre regras de vigas) ---
        ("234" . "PP_TEXTO VIGA")
        ("227" . "PP_TEXTO VIGA")
        ("*BEAM*IDEN*" . "PP_TEXTO VIGA")
        ("*VIGA*IDEN*" . "PP_TEXTO VIGA")
        ("*TEXTO VIGA*" . "PP_TEXTO VIGA")

        ;; --- PP_TEXTO APOIO ---
        ("*ZON _ Ambiente*" . "PP_TEXTO APOIO")
        ("*ZON _ *reas e Per*metros*" . "PP_TEXTO APOIO")
        ("*TXT _ Legal*" . "PP_TEXTO APOIO")
        ("*TXT _ Matr*cula*" . "PP_TEXTO APOIO")
        ("*LEG _ Legenda*" . "PP_TEXTO APOIO")
        ("*TAG _ Shaft*" . "PP_TEXTO APOIO")
        ("*TAG*Shaft*" . "PP_TEXTO APOIO")
        ("*TAG _ *" . "PP_TEXTO APOIO")
        ("*TAG*" . "PP_TEXTO APOIO")
        ("G-ANNO-TEXT" . "PP_TEXTO APOIO")
        ("A-ANNO-NOTE" . "PP_TEXTO APOIO")
        ("A-____-NOTE-TEXT" . "PP_TEXTO APOIO")
        ("A-FLOR-LEVL" . "PP_TEXTO APOIO")
        ("S-STRS-ANNO" . "PP_TEXTO APOIO")
        ("TEXTO" . "PP_TEXTO APOIO")
        ("TEXTO2" . "PP_TEXTO APOIO")
        ("COMENTÁRIOS" . "PP_TEXTO APOIO")
        ("COMENTARIOS" . "PP_TEXTO APOIO")
        ("*LINHA_DE_CHAMADA*" . "PP_TEXTO APOIO")
        ("*ANNO-TEXT*" . "PP_TEXTO APOIO")
        ("*TEXTO APOIO*" . "PP_TEXTO APOIO")
        ("*COMENT*" . "PP_TEXTO APOIO")
        ("*NOTE*" . "PP_TEXTO APOIO")
        ("*LEVL*" . "PP_TEXTO APOIO")
        ("*NIVEL*" . "PP_TEXTO APOIO")

        ;; --- PP_TEXTO ---
        ("*Selos de Zona*" . "PP_TEXTO")
        ("A-AREA-IDEN" . "PP_TEXTO")
        ("A-AREA-____-IDEN" . "PP_TEXTO")
        ("G-____-____-TEXT" . "PP_TEXTO")
        ("*AREA-IDEN*" . "PP_TEXTO")
        ("*AREA*IDEN*" . "PP_TEXTO")
        ("*LEGENDA*" . "PP_TEXTO")
        ("*LEG*" . "PP_TEXTO")
        ("*TEXTO*" . "PP_TEXTO")
        ("*TXT*" . "PP_TEXTO")
        ("*DOC _ Textos e Rótulos*" . "PP_TEXTO")
        ("*DOC _ Textos e R*tulos*" . "PP_TEXTO")

        ;; --- PP_COTAS ---
        ("*CTA _ N*vel*" . "PP_COTAS")
        ("*CTA*" . "PP_COTAS")
        ("G-ANNO-DIMS" . "PP_COTAS")
        ("G-____-____-DIMS" . "PP_COTAS")
        ("A---DIMS" . "PP_COTAS")
        ("COTAS" . "PP_COTAS")
        ("*DIMS*" . "PP_COTAS")
        ("*COTA*" . "PP_COTAS")
        ("*COTAS*" . "PP_COTAS")
        ("*DOC _ Cotas*" . "PP_COTAS")

        ;; --- PP_PROJEÇÃO (ArchiCAD Shafts com prioridade sobre regras gerais) ---
        ("*OBJ _ Shafts e Vazios*" . "PP_PROJEÇÃO")
        ("*SFT _ Shaft*" . "PP_PROJEÇÃO")
        ("*SFT*" . "PP_PROJEÇÃO")
        ("*2D _ Proje*" . "PP_PROJEÇÃO")

        ;; --- PP_LAYOUT (Prioritários antes de Alvenaria) ---
        ("*Pele de Vidro*" . "PP_LAYOUT")
        ("*VED _ Pele de Vidro*" . "PP_LAYOUT")

        ;; --- PP_ARQUITETURA ---
        ("A-WALL" . "PP_ARQUITETURA")
        ("A-WALL-____-MCUT" . "PP_ARQUITETURA")
        ("A-WALL-____-OTLN" . "PP_ARQUITETURA")
        ("A-WALL-____-PATT" . "PP_ARQUITETURA")
        ("A-WALL-PATT" . "PP_ARQUITETURA")
        ("ALVENARIA" . "PP_ARQUITETURA")
        ("*VED _ Paredes de Alvenaria*" . "PP_ARQUITETURA")
        ("*VED _ Drywall*" . "PP_ARQUITETURA")
        ("*ALV _ Parede*" . "PP_ARQUITETURA")
        ("*ALV _ Garden*" . "PP_ARQUITETURA")
        ("*Parede baixa*" . "PP_ARQUITETURA")
        ("*Mureta*" . "PP_ARQUITETURA")
        ("*ACB _ Parede*" . "PP_ARQUITETURA")
        ("ENCHIMENTO" . "PP_ARQUITETURA")
        ("ACABAMENTO" . "PP_ARQUITETURA")
        ("ALV-LAJE" . "PP_ARQUITETURA")
        ("ALV-VIGA" . "PP_ARQUITETURA")
        ("$0$ALVENARIA" . "PP_ARQUITETURA")
        ("*WALL*" . "PP_ARQUITETURA")
        ("*ALVENARIA*" . "PP_ARQUITETURA")
        ("*ALV*" . "PP_ARQUITETURA")
        ("*PAREDE*" . "PP_ARQUITETURA")
        ("*REVESTIMENTO*" . "PP_ARQUITETURA")
        ("*REV*" . "PP_ARQUITETURA")
        ("*PEDRA*" . "PP_ARQUITETURA")
        ("*SHAFT*" . "PP_ARQUITETURA")
        ("*FACHADA*" . "PP_ARQUITETURA")

        ;; --- PP_PILAR ---
        ("*PIL _ Pilar*" . "PP_PILAR")
        ("*PIL*" . "PP_PILAR")
        ("S-COLS" . "PP_PILAR")
        ("S-COLS-____-MCUT" . "PP_PILAR")
        ("S-COLS-____-OTLN" . "PP_PILAR")
        ("ESTRUTURA-1" . "PP_PILAR")
        ("ESTRUTURA-3" . "PP_PILAR")
        ("240" . "PP_PILAR")
        ("*240*" . "PP_PILAR")
        ("232" . "PP_PILAR")
        ("233" . "PP_PILAR")
        ("*COLS*" . "PP_PILAR")
        ("*PILAR*" . "PP_PILAR")
        ("*COLUNA*" . "PP_PILAR")
        ("*BLOC*" . "PP_PILAR")

        ;; --- PP_VIGA ---
        ("S-BEAM" . "PP_VIGA")
        ("S-BEAM-____-MCUT" . "PP_VIGA")
        ("S-BEAM-____-OTLN" . "PP_VIGA")
        ("250" . "PP_VIGA")
        ("4" . "PP_VIGA")
        ("25" . "PP_VIGA")
        ("*BEAM*" . "PP_VIGA")
        ("*VIGA*" . "PP_VIGA")
        ("*VIG*" . "PP_VIGA")
        ("*EST _ Viga*" . "PP_VIGA")

        ;; --- PP_CUBETA ---
        ("200" . "PP_CUBETA")
        ("210" . "PP_CUBETA")
        ("*200*" . "PP_CUBETA")
        ("*210*" . "PP_CUBETA")
        ("*CUBETA*" . "PP_CUBETA")
        ("*NERVURA*" . "PP_CUBETA")
        ("A-CLNG" . "PP_FORRO")
        ("A-CLNG-PATT" . "PP_FORRO")
        ("A-DETL-GENF-OTLN" . "PP_FORRO")
        ("A-DETL-DEMO-OTLN" . "PP_FORRO")
        ("FORRO" . "PP_FORRO")
        ("*CLNG*" . "PP_FORRO")
        ("*FOR-HTC*" . "PP_FORRO")
        ("*FORRO*" . "PP_FORRO")
        ("*FOR-*" . "PP_FORRO")
        ("*FOR_*" . "PP_FORRO")
        ("*ACB _ Forro*" . "PP_FORRO")
        ;; --- PP_COBERTURA ---
        ("*COB _ Pergolado*" . "PP_COBERTURA")
        ("*Pergolado*" . "PP_COBERTURA")
        ("*COB*" . "PP_COBERTURA")
        ("A-ROOF" . "PP_COBERTURA")
        ("A-ROOF-OTLN" . "PP_COBERTURA")
        ("A-ROOF-PATT" . "PP_COBERTURA")
        ("A-ROOF-MCUT" . "PP_COBERTURA")
        ("A-ROOF-____-MCUT" . "PP_COBERTURA")
        ("A-ROOF-____-OTLN" . "PP_COBERTURA")
        ("A-ROOF-____-PATT" . "PP_COBERTURA")
        ("*ROOF*" . "PP_COBERTURA")
        ("*COBERTURA*" . "PP_COBERTURA")
        ("*COBERT*" . "PP_COBERTURA")
        ("*TELHA*" . "PP_COBERTURA")
        ("*TELHO*" . "PP_COBERTURA")

        ;; --- PP_GARAGEM ---
        ("*Estacionamento*" . "PP_GARAGEM")
        ("*Manobra*" . "PP_GARAGEM")
        ("*OBJ _ Vaga*" . "PP_GARAGEM")
        ("C-PRKG" . "PP_GARAGEM")
        ("C-PRKG-____-OTLN" . "PP_GARAGEM")
        ("A-DETL-MBND" . "PP_GARAGEM")
        ("A-DETL-____-MBND" . "PP_GARAGEM")
        ("*DETL*MBND*" . "PP_GARAGEM")
        ("VAGAS" . "PP_GARAGEM")
        ("HACHURA" . "PP_GARAGEM")
        ("*PRKG*" . "PP_GARAGEM")
        ("*VAGA*" . "PP_GARAGEM")
        ("*VAGAS*" . "PP_GARAGEM")
        ("*GARAGEM*" . "PP_GARAGEM")
        ("*GARAG*" . "PP_GARAGEM")

        ;; --- PP_VEGETAÇÃO ---
        ("*2D _ *rea - Verde*" . "PP_VEGETAÇÃO")
        ("*Perme*vel*" . "PP_VEGETAÇÃO")
        ("*VEG _ Grama*" . "PP_VEGETAÇÃO")
        ("*Grama*" . "PP_VEGETAÇÃO")
        ("L-PLNT" . "PP_VEGETAÇÃO")
        ("L-PLNT-____-OTLN" . "PP_VEGETAÇÃO")
        ("L-SITE" . "PP_VEGETAÇÃO")
        ("L-SITE-____-OTLN" . "PP_VEGETAÇÃO")
        ("TERRA" . "PP_VEGETAÇÃO")
        ("FORRAÇÃO" . "PP_VEGETAÇÃO")
        ("FORRACAO" . "PP_VEGETAÇÃO")
        ("*PLNT*" . "PP_VEGETAÇÃO")
        ("*SITE*" . "PP_VEGETAÇÃO")
        ("*VEGETAÇÃO*" . "PP_VEGETAÇÃO")
        ("*VEGETACAO*" . "PP_VEGETAÇÃO")
        ("*VEGETA*" . "PP_VEGETAÇÃO")
        ("*VEG*" . "PP_VEGETAÇÃO")
        ("*JARDIM*" . "PP_VEGETAÇÃO")
        ("*FORRAÇÃO*" . "PP_VEGETAÇÃO")
        ("*FORRACAO*" . "PP_VEGETAÇÃO")
        ("*PAI _ Paisagismo*" . "PP_VEGETAÇÃO")
        ("A-DETL-DEMO" . "PP_PROJEÇÃO")
        ("A-FLOR-OVHD" . "PP_PROJEÇÃO")
        ("A-FLOR-____-OVHD" . "PP_PROJEÇÃO")
        ("_Acima_ Linha de corte do corrimão" . "PP_PROJEÇÃO")
        ("Acima Linha de corte do corrimão" . "PP_PROJEÇÃO")
        ("*ACIMA*" . "PP_PROJEÇÃO")
        ("*LINHA DE CORTE DO CORRIM*" . "PP_PROJEÇÃO")
        ("*LINHA DE CORTE*" . "PP_PROJEÇÃO")
        ("PROJEÇÕES" . "PP_PROJEÇÃO")
        ("PROJEÇÃO" . "PP_PROJEÇÃO")
        ("PROJECAO" . "PP_PROJEÇÃO")
        ("PROJECOES" . "PP_PROJEÇÃO")
        ("211" . "PP_PROJEÇÃO")
        ("*OVHD*" . "PP_PROJEÇÃO")
        ("*DEMO*" . "PP_PROJEÇÃO")
        ("*PROJEÇÃO*" . "PP_PROJEÇÃO")
        ("*PROJECAO*" . "PP_PROJEÇÃO")
        ("*PROJEC*" . "PP_PROJEÇÃO")
        ("E-ELEC-EQPM" . "PP_EQUIPAMENTO")
        ("E-ELEC-EQPM-OTLN" . "PP_EQUIPAMENTO")
        ("E-ELEC-FIXT" . "PP_EQUIPAMENTO")
        ("M-EQPM" . "PP_EQUIPAMENTO")
        ("M-EQPM-____-OTLN" . "PP_EQUIPAMENTO")
        ("P-PIPE" . "PP_EQUIPAMENTO")
        ("PP_FRIGORIFICA_EQUIPAMENTOS" . "PP_EQUIPAMENTO")
        ("PP_EXAUSTÃO_EQUIP" . "PP_EQUIPAMENTO")
        ("PP_EXAUSTÃO_DUTO" . "PP_EQUIPAMENTO")
        ("PP_FRIGORIFICA" . "PP_EQUIPAMENTO")
        ("H-TC-CX" . "PP_EQUIPAMENTO")
        ("H-AF-CX" . "PP_EQUIPAMENTO")
        ("H-INC-CX" . "PP_EQUIPAMENTO")
        ("QUADROS" . "PP_EQUIPAMENTO")
        ("LINHA01" . "PP_EQUIPAMENTO")
        ("CP-DIFUSÃO" . "PP_EQUIPAMENTO")
        ("TigreCad" . "PP_EQUIPAMENTO")
        ("Tinterno" . "PP_EQUIPAMENTO")
        ("*EQPM*" . "PP_EQUIPAMENTO")
        ("*ELEC*" . "PP_EQUIPAMENTO")
        ("*ELETRIC*" . "PP_EQUIPAMENTO")
        ("*PIPE*" . "PP_EQUIPAMENTO")
        ("*DUTO*" . "PP_EQUIPAMENTO")
        ("*EXAUST*" . "PP_EQUIPAMENTO")
        ("*FRIGORIF*" . "PP_EQUIPAMENTO")
        ("*TUB*" . "PP_EQUIPAMENTO")
        ("*HIDR*" . "PP_EQUIPAMENTO")
        ("*EQP*" . "PP_EQUIPAMENTO")
        ;; --- Reservatórios, Caixas Elétricas, Barriletes, Bombas e Passagens ---
        ("*RESERV*" . "PP_EQUIPAMENTO")
        ("*CISTERNA*" . "PP_EQUIPAMENTO")
        ("*BARRILETE*" . "PP_EQUIPAMENTO")
        ("*CAIXA*AGUA*" . "PP_EQUIPAMENTO")
        ("*CAIXA*ÁGUA*" . "PP_EQUIPAMENTO")
        ("*CX*AGUA*" . "PP_EQUIPAMENTO")
        ("*CX*ÁGUA*" . "PP_EQUIPAMENTO")
        ("*CAIXA*ELET*" . "PP_EQUIPAMENTO")
        ("*CX*ELET*" . "PP_EQUIPAMENTO")
        ("*QUADRO*ELET*" . "PP_EQUIPAMENTO")
        ("*QD*ELET*" . "PP_EQUIPAMENTO")
        ("*CAIXA*PASSAGEM*" . "PP_EQUIPAMENTO")
        ("*CX*PASSAGEM*" . "PP_EQUIPAMENTO")
        ("*PASSAGEM*" . "PP_EQUIPAMENTO")
        ("*INSPEC*" . "PP_EQUIPAMENTO")
        ("*BOMBA*" . "PP_EQUIPAMENTO")
        ("0" . "PP_LAYOUT")
        ("C-TOPO" . "PP_LAYOUT")
        ("C-TOPO-PATT" . "PP_LAYOUT")
        ("C-TOPO-MCUT" . "PP_LAYOUT")
        ("*TOPO*" . "PP_LAYOUT")
        ("C-PROP-LINE" . "PP_LAYOUT")
        ("C-PROP-LINE-OTLN" . "PP_LAYOUT")
        ("*PROP-LINE*" . "PP_LAYOUT")
        ("*DIVISA*" . "PP_LAYOUT")
        ("A-FLOR" . "PP_LAYOUT")
        ("A-FLOR-MCUT" . "PP_LAYOUT")
        ("A-FLOR-PATT" . "PP_LAYOUT")
        ("A-FLOR-____-MCUT" . "PP_LAYOUT")
        ("A-FLOR-____-OTLN" . "PP_LAYOUT")
        ("PISO" . "PP_LAYOUT")
        ("PISO-1" . "PP_LAYOUT")
        ("*FLOR*" . "PP_LAYOUT")
        ("*FLOOR*" . "PP_LAYOUT")
        ("*PISO*" . "PP_LAYOUT")
        ("A-DOOR" . "PP_LAYOUT")
        ("A-DOOR-FRAM" . "PP_LAYOUT")
        ("A-DOOR-FRAM-MCUT" . "PP_LAYOUT")
        ("A-DOOR-FRAM-OTLN" . "PP_LAYOUT")
        ("A-DOOR-GLAZ-MCUT" . "PP_LAYOUT")
        ("A-DOOR-HDLN" . "PP_LAYOUT")
        ("A-DOOR-____-HDLN" . "PP_LAYOUT")
        ("A-DOOR-____-MCUT" . "PP_LAYOUT")
        ("A-DOOR-____-OTLN" . "PP_LAYOUT")
        ("PORTAS" . "PP_LAYOUT")
        ("*DOOR*" . "PP_LAYOUT")
        ("*PORTA*" . "PP_LAYOUT")
        ("A-GLAZ" . "PP_LAYOUT")
        ("A-GLAZ-CURT" . "PP_LAYOUT")
        ("A-GLAZ-CWMG" . "PP_LAYOUT")
        ("A-GLAZ-CWMG-OTLN" . "PP_LAYOUT")
        ("A-GLAZ-____-OTLN" . "PP_LAYOUT")
        ("A-GLAZ-____-MCUT" . "PP_LAYOUT")
        ("CAIXILHOS" . "PP_LAYOUT")
        ("*GLAZ*" . "PP_LAYOUT")
        ("*JANELA*" . "PP_LAYOUT")
        ("*ESQUADRIA*" . "PP_LAYOUT")
        ("*VIDRO*" . "PP_LAYOUT")
        ("*CAIXILHO*" . "PP_LAYOUT")
        ("Q-CASE" . "PP_LAYOUT")
        ("Q-CASE-____-OTLN" . "PP_LAYOUT")
        ("*CASE*" . "PP_LAYOUT")
        ("*ARMARIO*" . "PP_LAYOUT")
        ("*MARCENARIA*" . "PP_LAYOUT")
        ("P-SANR-FIXT" . "PP_LAYOUT")
        ("P-SANR-FIXT-HDLN" . "PP_LAYOUT")
        ("P-SANR-FIXT-OTLN" . "PP_LAYOUT")
        ("LOUCAS" . "PP_LAYOUT")
        ("*SANR*" . "PP_LAYOUT")
        ("*LOUCA*" . "PP_LAYOUT")
        ("*LOUÇA*" . "PP_LAYOUT")
        ("*FIXT*" . "PP_LAYOUT")
        ("A-MEDQ" . "PP_LAYOUT")
        ("A-MEDQ-____-OTLN" . "PP_LAYOUT")
        ("*MEDQ*" . "PP_LAYOUT")
        ("A-FDSQ" . "PP_LAYOUT")
        ("*FDSQ*" . "PP_LAYOUT")
        ("A-GENM" . "PP_LAYOUT")
        ("A-GENM-____-OTLN" . "PP_LAYOUT")
        ("A-GENM-____-HDLN" . "PP_LAYOUT")
        ("*GENM*" . "PP_LAYOUT")
        ("Q-SPCQ" . "PP_LAYOUT")
        ("Q-SPCQ-____-OTLN" . "PP_LAYOUT")
        ("*SPCQ*" . "PP_LAYOUT")
        ("S-STRS" . "PP_LAYOUT")
        ("S-STRS-MBND" . "PP_LAYOUT")
        ("S-STRS-____-MBND" . "PP_LAYOUT")
        ("S-STRS-____-OTLN" . "PP_LAYOUT")
        ("Guarda-corpos" . "PP_LAYOUT")
        ("Balaústres" . "PP_LAYOUT")
        ("Corrimão" . "PP_LAYOUT")
        ("*STRS*" . "PP_LAYOUT")
        ("*ESCADA*" . "PP_LAYOUT")
        ("*CORRIM*" . "PP_LAYOUT")
        ("*BALAUST*" . "PP_LAYOUT")
        ("*GUARDA*" . "PP_LAYOUT")
        ("A-DETL" . "PP_LAYOUT")
        ("A-DETL-THIN" . "PP_LAYOUT")
        ("A-DETL-MEDM" . "PP_LAYOUT")
        ("A-DETL-____-MEDM" . "PP_LAYOUT")
        ("A-DETL-____-OTLN" . "PP_LAYOUT")
        ("A-DETL-____-THIN" . "PP_LAYOUT")
        ("*DETL*" . "PP_LAYOUT")
        ("I-FURN" . "PP_LAYOUT")
        ("I-FURN-____-OTLN" . "PP_LAYOUT")
        ("MÓVEIS" . "PP_LAYOUT")
        ("MOVEIS" . "PP_LAYOUT")
        ("*FURN*" . "PP_LAYOUT")
        ("*MOB*" . "PP_LAYOUT")
        ("LINHA015" . "PP_LAYOUT")
        ("LINHA025" . "PP_LAYOUT")
        ("LINHA 01" . "PP_LAYOUT")
        ("H-INC-TB" . "PP_LAYOUT")
        ("BLOCO" . "PP_LAYOUT")
        ("E-NURS" . "PP_LAYOUT")
        ("E-NURS-____-OTLN" . "PP_LAYOUT")
        ("E-NURS*" . "PP_LAYOUT")
        ("*NURS*" . "PP_LAYOUT")
        ("L-HRDS" . "PP_LAYOUT")
        ("L-HRDS-____-OTLN" . "PP_LAYOUT")
        ("L-HRDS-____-PATT" . "PP_LAYOUT")
        ("L-HRDS-PATT" . "PP_LAYOUT")
        ("L-HRDS*" . "PP_LAYOUT")
        ("*HRDS*" . "PP_LAYOUT")
        ;; --- ArchiCAD Layout ---
        ("0_Caneta_No__*" . "PP_LAYOUT")
        ("*LAJ _ Laje*" . "PP_LAYOUT")
        ("*LAJ*" . "PP_LAYOUT")
        ("*RMP _ Rampa*" . "PP_LAYOUT")
        ("*RMP*" . "PP_LAYOUT")
        ("*ESC _ Escada*" . "PP_LAYOUT")
        ("*ESQ _ Gradil*" . "PP_LAYOUT")
        ("*ESQ _ Guarda-corpo*" . "PP_LAYOUT")
        ("*ESQ*" . "PP_LAYOUT")
        ("*GRADIL*" . "PP_LAYOUT")
        ("*MOB _ PcD*" . "PP_LAYOUT")
        ("*SMB _ PcD*" . "PP_LAYOUT")
        ("*SMB*" . "PP_LAYOUT")
        ("*OBJ _ Elevador*" . "PP_LAYOUT")
        ("*OBJ _ Lixo*" . "PP_LAYOUT")
        ("*OBJ _ Churrasqueira*" . "PP_LAYOUT")
        ("*OBJ*" . "PP_LAYOUT")
        ("*PAV _ Guia*" . "PP_LAYOUT")
        ("*Sarjeta*" . "PP_LAYOUT")
        ("*PAV*" . "PP_LAYOUT")
        ("*EFS _ Lou*a*" . "PP_LAYOUT")
        ("*EFS _ Metal*" . "PP_LAYOUT")
        ("*EFS*" . "PP_LAYOUT")
        ("*PDR _ Peitoril*" . "PP_LAYOUT")
        ("*PDR _ Bancada*" . "PP_LAYOUT")
        ("*PDR _ Soleira*" . "PP_LAYOUT")
        ("*PDR _ Filete*" . "PP_LAYOUT")
        ("*PDR _ Borda de piscina*" . "PP_LAYOUT")
        ("*PDR*" . "PP_LAYOUT")
        ("*2D _ Cal*ada*" . "PP_LAYOUT")
        ("*2D _ Recuo*" . "PP_LAYOUT")
        ("*2D _ Espa*o m*nimo*" . "PP_LAYOUT")
        ("*CMP _ Planialti*" . "PP_LAYOUT")
        ("*Janelas Archicad*" . "PP_LAYOUT")
        ("*Portas Archicad*" . "PP_LAYOUT")
        ("*OBJ _ Eletrodom*" . "PP_LAYOUT")
        ("*OBJ _ Mobili*" . "PP_LAYOUT")
        ("*PED _ Bancadas*" . "PP_LAYOUT")
        ("*OBJ _ Lou*as e Metais*" . "PP_LAYOUT")
        ("*OBJ _ Elevadores e plataformas*" . "PP_LAYOUT")
        ("*EST _ Escadas e Rampas*" . "PP_LAYOUT")
        ("*OBJ _ Guarda-Corpo e Corrim*" . "PP_LAYOUT")
        ("*PAGINA*" . "PP_LAYOUT")
        ("*LAYOUT*" . "PP_LAYOUT")
      ))

      (setq count_del 0)
      (setq count_kept 0)
      (setq layer_cache nil)
      (setq i 0)
      
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (if (and ent (setq elist (entget ent)))
          (progn
            (setq ent_lay (cdr (assoc 8 elist)))
            (setq ent_type (cdr (assoc 0 elist)))
            (setq u_lay (strcase ent_lay))
            
            ;; Consulta rÃ¡pida no cache de decisÃµes de camada (O(1))
            (setq cache_entry (assoc u_lay layer_cache))
            (if (not cache_entry)
              (progn
                (setq cache_entry (cons u_lay (chlay:resolve-layer-action ent_lay rules_delete rules_map)))
                (setq layer_cache (cons cache_entry layer_cache))
              )
            )
            (setq action_code (cdr cache_entry))
            (setq act_type (car action_code))
            
            ;; Execução direta da ação de exclusão
            (cond
              ;; Exclusão direta da entidade
              ((= act_type "DELETE")
               (entdel ent)
               (setq count_del (1+ count_del))
              )

              ;; Condicional Mobiliário: hachura apaga
              ((= act_type "COND_FURN")
               (if (= ent_type "HATCH")
                 (progn
                   (entdel ent)
                   (setq count_del (1+ count_del))
                 )
                 (setq count_kept (1+ count_kept))
               )
              )

              ;; Condicional TEXTO3: TEXT simples apaga
              ((= act_type "COND_TEXTO3")
               (if (= ent_type "TEXT")
                 (progn
                   (entdel ent)
                   (setq count_del (1+ count_del))
                 )
                 (setq count_kept (1+ count_kept))
               )
              )

              ;; Condicional VAZIO: hachura apaga
              ((= act_type "COND_VAZIO")
               (if (= ent_type "HATCH")
                 (progn
                   (entdel ent)
                   (setq count_del (1+ count_del))
                 )
                 (setq count_kept (1+ count_kept))
               )
              )

              ;; Condicional MBND (Vagas de Garagem): hachura e texto apagam
              ((= act_type "COND_MBND")
               (if (or (= ent_type "HATCH")
                       (member ent_type '("TEXT" "MTEXT" "MULTILEADER" "LEADER")))
                 (progn
                   (entdel ent)
                   (setq count_del (1+ count_del))
                 )
                 (setq count_kept (1+ count_kept))
               )
              )

              ;; Condicional Selos de Zona ou Zonas: linhas/polilinhas/hachuras apagam, textos/blocos mantidos
              ((or (= act_type "COND_ZON_TEXT") (= act_type "COND_ZON_APOIO"))
               (if (member ent_type '("TEXT" "MTEXT" "INSERT" "MULTILEADER" "LEADER"))
                 (setq count_kept (1+ count_kept))
                 (progn
                   (entdel ent)
                   (setq count_del (1+ count_del))
                 )
               )
              )

              ;; Hachura ou Texto destinado a PP_GARAGEM: apaga se nÃ£o for nativo
              ((and (= act_type "MOVE")
                    (= (strcase (cdr action_code)) "PP_GARAGEM")
                    (or (= ent_type "HATCH")
                        (member ent_type '("TEXT" "MTEXT" "MULTILEADER" "LEADER")))
                    (/= (strcase ent_lay) "PP_GARAGEM"))
               (entdel ent)
               (setq count_del (1+ count_del))
              )

              ;; Hachura destinada a PP_LAYOUT: apaga se nÃ£o for nativa
              ((and (= act_type "MOVE")
                    (= (strcase (cdr action_code)) "PP_LAYOUT")
                    (= ent_type "HATCH")
                    (/= (strcase ent_lay) "PP_LAYOUT"))
               (entdel ent)
               (setq count_del (1+ count_del))
              )

              ;; Demais elementos mantidos intactos
              (t
               (setq count_kept (1+ count_kept))
              )
            )
          )
        )
        (setq i (1+ i))
      )
      
      (vl-catch-all-apply '(lambda () (vla-regen doc acActiveViewport)))
      (if old_highlight (setvar "HIGHLIGHT" old_highlight))
      (if old_cmdecho (setvar "CMDECHO" old_cmdecho))
      (vla-EndUndoMark doc)
      
      (prompt (strcat "\n[CHLDEL] Exclusão concluída: " 
                      (itoa count_del) " objeto(s) excluído(s), " 
                      (itoa count_kept) " mantido(s)."
              )
      )
    )
  )
  (princ)
)

;; Comando CHLDEL: executa apenas a exclusão dos elementos conforme as regras (sem alterar camadas)
;; Otimizado para fluxo rápido (Comando -> Seleção -> Enter), sem sobrecarga de GRIPS ou verificação de layers
(defun c:CHLDEL (/ ss)
  ;; 1. Desarmar grips ou seleções prévias residuais imediatamente
  (sssetfirst nil nil)
  (prompt "\n[CHLDEL] Selecione os objetos para exclusão (ou ENTER para todo o ModelSpace): ")
  (setq ss (ssget))
  (if (not ss)
    (progn
      (prompt "\n[CHLDEL] Nenhum objeto selecionado manualmente. Processando todo o ModelSpace...")
      (setq ss (ssget "_X" '((410 . "Model"))))
    )
  )
  (if ss
    (chlay:delete-selection ss)
    (prompt "\n[CHLDEL] Operação cancelada / Nenhum objeto selecionado.")
  )
  (princ)
)

;; Atalhos rápidos para exclusão
(defun c:CDEL ()
  (c:CHLDEL)
)
(defun c:CD ()
  (c:CHLDEL)
)

;; Comando PPLAYERS: restaura e reafirma as propriedades de todas as camadas do padrão PP
(defun c:PPLAYERS ()
  (chlay:ensure-all-predefined-layers)
  (prompt "\n[PPLAYERS] Todas as camadas do padrão PP foram restauradas e padronizadas com sucesso (Cores, Linetypes e Plottability).")
  (princ)
)

;; Alias de compatibilidade
(defun c:PPRESET ()
  (c:PPLAYERS)
)

;; Comando auxiliar para rodar OVERKILL / Purge rápido em todo o desenho antes ou depois do CHLAUTO
(defun c:CHOVERKILL (/ old_cmdecho)
  (setq old_cmdecho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (chlay:ensure-all-predefined-layers)
  (prompt "\n[CHOVERKILL] Executando limpeza de objetos sobrepostos (OVERKILL no ModelSpace)...")
  (vl-cmdf "._-OVERKILL" "_ALL" "" "")
  (setvar "CMDECHO" old_cmdecho)
  (prompt "\n[CHOVERKILL] Limpeza de sobreposições concluída.")
  (princ)
)

;; Comando ORGL: Organiza a "Draw Order" das layers padrão, definindo a prioridade
(defun c:ORGL (/ cmd_old layers_para_frente ss) 
  (setq cmd_old (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)

  (setq layers_para_frente (list "PP_FORRO" "PP_VEGETAÇÃO" "PP_GARAGEM" "PP_PROJEÇÃO" 
                                 "PP_LAYOUT" "PP_ARQUITETURA" "PP_VIGA" "PP_PILAR" 
                                 "PP_TEXTO" "PP_REGIÃO"
                            )
  )

  (foreach lay layers_para_frente 
    (if (setq ss (ssget "X" (list (cons 8 lay)))) 
      (vl-cmdf "_.draworder" ss "" "_Front")
    )
  )

  (setvar "CMDECHO" cmd_old)
  (princ "\n[ORGL] Ordem das layers organizada com sucesso.")
  (princ)
)

(princ "\n[BB_CHLAY] Carregado com sucesso. Comandos disponíveis: CHL, CHLDEL, CHLAUTO, LBR, CHLHATCH, PPLAYERS, CHOVERKILL, ORGL.")
(princ)

