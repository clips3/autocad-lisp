;;; Comando TESC - Altera altura de textos baseado na escala e layer
;;; Desenvolvido para AutoCAD em AutoLISP

(defun c:TESC ( / *error* old_cmdecho ss i ent ent_data layer altura_princ altura_viga count_princ count_viga scale_choice scale_str)
  
  ;; 1. Tratamento de erros
  ;; A função local *error* é executada se o usuário cancelar (ESC) ou ocorrer uma falha.
  (defun *error* (msg)
    ;; Restaura a variável de sistema caso tenha sido alterada
    (if old_cmdecho (setvar "CMDECHO" old_cmdecho))
    ;; Exibe a mensagem de erro (ignorando cancelamentos padrão)
    (if (not (member msg '("Function cancelled" "quit / exit abort")))
      (princ (strcat "\nErro inesperado: " msg))
    )
    ;; Mensagem solicitada em caso de erro ou cancelamento
    (princ "\nComando TESC cancelado ou ocorreu um erro.")
    (princ)
  )

  ;; Salva o estado atual do CMDECHO e o desativa para não sujar a linha de comando
  (setq old_cmdecho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)

  ;; 2. Interação com o usuário
  ;; (initget 1) força o usuário a fornecer uma resposta (não permite Enter vazio)
  (initget 1 "100 75 50")
  (setq scale_choice (getkword "\nEscolha a escala desejada [100 / 75 / 50]: "))
  
  ;; 3. Definição das alturas de acordo com a escala escolhida
  (cond
    ((= scale_choice "100")
      (setq altura_princ 0.20
            altura_viga 0.17
            scale_str "1:100")
    )
    ((= scale_choice "75")
      (setq altura_princ 0.15
            altura_viga 0.12
            scale_str "1:75")
    )
    ((= scale_choice "50")
      (setq altura_princ 0.10
            altura_viga 0.07
            scale_str "1:50")
    )
  )

  ;; 4. Seleção automática no desenho inteiro
  ;; "X" varre o desenho todo. O filtro restringe apenas a TEXT e MTEXT.
  (setq ss (ssget "X" '((0 . "TEXT,MTEXT"))))
  
  ;; Inicializando contadores de alterações
  (setq count_princ 0
        count_viga 0)
        
  ;; 5. Lógica de Layers e Alturas
  (if ss
    (progn
      (setq i 0)
      ;; Iterando por cada entidade do selection set
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (setq ent_data (entget ent))
        ;; Pega o nome da layer e converte para maiúsculo para evitar erros de digitação (case-insensitive)
        (setq layer (strcase (cdr (assoc 8 ent_data))))
        
        (cond
          ;; Verifica se é layer principal
          ((or (= layer "PP_TEXTO") (= layer "PP_TEXTO APOIO"))
            ;; Atualiza a propriedade de altura (código DXF 40)
            (setq ent_data (subst (cons 40 altura_princ) (assoc 40 ent_data) ent_data))
            (entmod ent_data) ; Aplica a alteração à entidade
            (setq count_princ (1+ count_princ))
          )
          ;; Verifica se é layer secundária (viga/pilar)
          ((or (= layer "PP_TEXTO VIGA") (= layer "PP_TEXTO PILAR"))
            ;; Atualiza a propriedade de altura (código DXF 40)
            (setq ent_data (subst (cons 40 altura_viga) (assoc 40 ent_data) ent_data))
            (entmod ent_data) ; Aplica a alteração à entidade
            (setq count_viga (1+ count_viga))
          )
        )
        (setq i (1+ i)) ;; Próxima entidade
      )
    )
  )

  ;; 6. Relatório Final na Linha de Comando
  (if (and (= count_princ 0) (= count_viga 0))
    (princ "\nNenhum texto encontrado nas layers especificadas.")
    (princ (strcat "\nComando TESC concluído: " 
                   (itoa count_princ) " textos principais e " 
                   (itoa count_viga) " textos de viga/pilar alterados para a escala " 
                   scale_str "."))
  )

  ;; Restaura variáveis de sistema (CMDECHO)
  (setvar "CMDECHO" old_cmdecho)
  
  (princ) ;; Fim silencioso
)

;; Mensagem informativa ao carregar o arquivo
(princ "\nRotina carregada com sucesso. Digite TESC para executar.")
(princ)
