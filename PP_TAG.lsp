;;; ==============================================================================
;;; Script: PP_TAG.lsp
;;; Comando: TAG
;;; Função: Insere uma instância do bloco "TAG - AC" no ponto de origem
;;;         (ponto de inserção) de cada bloco selecionado. Lê os parâmetros/
;;;         atributos do bloco de origem (Btu, Nome, Altura) e preenche os
;;;         atributos do bloco recém-inserido (BTU, NOME, ALTURA).
;;; ==============================================================================

(defun c:tag (/ ss i ent obj entData insPt oldCmd oldAttreq oldOsm valBtu valNome 
              valAltura tagObj atts get-prop-or-att scaleX scaleY scaleZ oldDimzin
             ) 
  (vl-load-com)

  (princ "\nSelecione os blocos de referência: ")

  (if (setq ss (ssget '((0 . "INSERT")))) 
    (progn 
      (setq oldCmd (getvar "CMDECHO"))
      (setq oldAttreq (getvar "ATTREQ"))
      (setq oldOsm (getvar "OSMODE"))

      (setvar "CMDECHO" 0)
      (setvar "ATTREQ" 0)
      (setvar "OSMODE" 0)

      ;; Função auxiliar para buscar valor de propriedade dinâmica ou atributo
      (defun get-prop-or-att (blkObj propName / val result props) 
        (setq result nil)

        ;; 1) Tentar Propriedades Dinâmicas (Custom Properties)
        (if (= (vla-get-IsDynamicBlock blkObj) :vlax-true) 
          (progn 
            ;; Captura as propriedades e ignora erros se houver falha
            (if 
              (not 
                (vl-catch-all-error-p 
                  (setq props (vl-catch-all-apply 'vlax-invoke 
                                                  (list blkObj 
                                                        'GetDynamicBlockProperties
                                                  )
                              )
                  )
                )
              )
              (foreach prop props 
                (if (= (strcase (vla-get-PropertyName prop)) (strcase propName)) 
                  (setq result (vlax-variant-value (vla-get-Value prop)))
                )
              )
            )
          )
        )

        ;; 2) Tentar Atributos se não achou nas propriedades dinâmicas
        (if (not result) 
          (if (= (vla-get-HasAttributes blkObj) :vlax-true) 
            (progn 
              (if 
                (not 
                  (vl-catch-all-error-p 
                    (setq props (vl-catch-all-apply 'vlax-invoke 
                                                    (list blkObj 'GetAttributes)
                                )
                    )
                  )
                )
                (foreach prop props 
                  (if (= (strcase (vla-get-TagString prop)) (strcase propName)) 
                    (setq result (vla-get-TextString prop))
                  )
                )
              )
            )
          )
        )

        ;; Converte o resultado para string
        (if result 
          (cond 
            ((= (type result) 'STR) result)
            ((= (type result) 'INT) (itoa result))
            ((= (type result) 'REAL) (rtos result))
            (t (vl-princ-to-string result))
          )
          "" ; Retorna vazio caso não encontre
        )
      )

      (setq i 0)
      (while (< i (sslength ss)) 
        (setq ent (ssname ss i))
        (setq obj (vlax-ename->vla-object ent))
        (setq entData (entget ent))

        ;; Busca os valores "Btu", "Nome", "Altura" no bloco original
        (setq valBtu (get-prop-or-att obj "Btu"))
        (setq valNome (get-prop-or-att obj "Nome"))
        (setq valAltura (get-prop-or-att obj "Altura"))

        ;; Formata a altura (limita a 2 casas decimais e suprime zeros à direita)
        (if (and (/= valAltura "") (distof valAltura)) 
          (progn 
            (setq oldDimzin (getvar "DIMZIN"))
            (setvar "DIMZIN" 8)
            (setq valAltura (rtos (distof valAltura) 2 2))
            (setvar "DIMZIN" oldDimzin)
          )
        )

        ;; Captura o ponto de inserção e as escalas do bloco
        (setq insPt (cdr (assoc 10 entData)))
        (setq scaleX (vla-get-XScaleFactor obj))
        (setq scaleY (vla-get-YScaleFactor obj))
        (setq scaleZ (vla-get-ZScaleFactor obj))

        ;; Insere o bloco "TAG - AC" com escala temporária de 1
        (command "_.-INSERT" "TAG - AC" insPt 1.0 1.0 0.0)

        ;; Pega o objeto do bloco recém inserido e aplica a escala
        (setq tagObj (vlax-ename->vla-object (entlast)))
        (vla-put-XScaleFactor tagObj scaleX)
        (vla-put-YScaleFactor tagObj scaleY)
        (vla-put-ZScaleFactor tagObj scaleZ)

        ;; Atualiza os atributos do "TAG - AC"
        (if (= (vla-get-HasAttributes tagObj) :vlax-true) 
          (progn 
            (if 
              (not 
                (vl-catch-all-error-p 
                  (setq atts (vl-catch-all-apply 'vlax-invoke 
                                                 (list tagObj 'GetAttributes)
                             )
                  )
                )
              )
              (foreach att atts 
                (cond 
                  ((= (strcase (vla-get-TagString att)) "NOME")
                   (if (/= valNome "") (vla-put-TextString att valNome))
                  )
                  ((= (strcase (vla-get-TagString att)) "BTU")
                   (if (/= valBtu "") (vla-put-TextString att valBtu))
                  )
                  ((= (strcase (vla-get-TagString att)) "ALTURA")
                   (if (/= valAltura "") 
                     (vla-put-TextString att (strcat "h = " valAltura "m"))
                   )
                  )
                )
              )
            )
          )
        )

        (setq i (1+ i))
      )

      (command "_.REGEN")

      (setvar "OSMODE" oldOsm)
      (setvar "ATTREQ" oldAttreq)
      (setvar "CMDECHO" oldCmd)

      (princ 
        (strcat "\nForam inseridos " 
                (itoa (sslength ss))
                " blocos 'TAG - AC' com sucesso."
        )
      )
    )
    (princ "\nNenhum bloco foi selecionado ou cancelado.")
  )

  (princ)
)

(princ "\nLisp BB_TAG carregada. Digite TAG para executar o comando.")
(princ)
