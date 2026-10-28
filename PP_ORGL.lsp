;;; ==============================================================================
;;; LISP: BB_ORGL
;;; COMANDO: ORGL
;;; DESCRIÇÃO: Organiza a "Draw Order" das layers padrão, definindo a prioridade.
;;; ==============================================================================
;;; Ordem de prioridade (da maior para a menor):
;;; 1. PP_REGIÃO (Ficará no topo de todas)
;;; 2. PP_TEXTO
;;; 3. PP_PILAR
;;; 4. PP_VIGA
;;; 5. PP_ARQUITETURA
;;; 6. PP_LAYOUT
;;; 7. PP_PROJEÇÃO
;;; 8. PP_GARAGEM
;;; 9. PP_VEGETAÇÃO
;;; 10. PP_FORRO (Ficará por baixo das citadas acima)
;;; ==============================================================================

(defun c:ORGL (/ cmd_old layers_para_frente ss) 
  ;; Salva a configuração atual do CMDECHO para não sujar o prompt
  (setq cmd_old (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)

  ;; Lista de layers na ordem INVERSA da prioridade (da menor para a maior).
  ;; O último item da lista será o último a ser trazido para frente, ficando por cima.
  (setq layers_para_frente (list "PP_FORRO" "PP_VEGETAÇÃO" "PP_GARAGEM" "PP_PROJEÇÃO" 
                                 "PP_LAYOUT" "PP_ARQUITETURA" "PP_VIGA" "PP_PILAR" 
                                 "PP_TEXTO" "PP_REGIÃO"
                           )
  )

  (princ "\nOrganizando a ordem das layers (Draw Order)... Favor aguardar.")

  (foreach lay layers_para_frente 
    ;; Seleciona todos os objetos no Model/Paper que estão nas layers do loop atual
    (if (setq ss (ssget "X" (list (cons 8 lay)))) 
      (progn 
        ;; Executa o comando Draw Order trazendo para a frente ("_Front")
        ;; A chamada à função command lida com a seleção e a opção _Front
        (command "_.draworder" ss "" "_Front")
      )
    )
  )

  ;; Restaura o CMDECHO
  (setvar "CMDECHO" cmd_old)

  (princ "\nOrdem das layers organizada com sucesso! (Maior prioridade: PP_REGIÃO)")
  (princ)
)

(princ "\nLisp BB_ORGL carregada. Digite ORGL para organizar a draw order das layers.")
(princ)
