;;; ==========================================================================
;;; Comando: MH (Merge Hatch - BB_MERGEHATCH)
;;; Descrição: Une várias hachuras independentes em uma única hachura,
;;;            mantendo os limites (islands/boundaries) originais de cada uma.
;;; ==========================================================================

(vl-load-com)

(defun c:MH ( / ss firstEnt lastent bound_ss newhatch oldCmd oldNomutt *error*)
  
  ;; Função de tratamento de erro para garantir o retorno das variáveis de sistema
  (defun *error* (msg)
    (if oldCmd (setvar "CMDECHO" oldCmd))
    (if oldNomutt (setvar "NOMUTT" oldNomutt))
    (if (not (wcmatch (strcase msg t) "*BREAK*,*CANCEL*,*EXIT*"))
      (princ (strcat "\nErro: " msg))
    )
    (princ)
  )

  ;; Salva configurações do sistema
  (setq oldCmd (getvar "CMDECHO"))
  (setq oldNomutt (getvar "NOMUTT"))
  (setvar "CMDECHO" 0)
  
  (princ "\nSelecione as hachuras para unir: ")
  ;; Filtra apenas seleções de Hatch
  (if (setq ss (ssget '((0 . "HATCH"))))
    (progn
      (if (> (sslength ss) 1)
        (progn
          (setvar "NOMUTT" 1) ;; silencia o output dos comandos temporários
          
          ;; Pega a primeira hachura para copiar as propriedades depois
          (setq firstEnt (ssname ss 0))
          
          ;; Grava a última entidade atual para saber quais serão os limites gerados
          (setq lastent (entlast))
          
          ;; Gera os limites (boundaries) das hachuras selecionadas
          (vl-cmdf "_.HATCHGENERATEBOUNDARY" ss "")
          
          ;; Coleta os novos limites gerados (polilinhas/linhas)
          (setq bound_ss (ssadd))
          (while (setq lastent (entnext lastent))
            (ssadd lastent bound_ss)
          )
          
          (if (> (sslength bound_ss) 0)
            (progn
              ;; Cria uma nova hachura usando os limites gerados
              (vl-cmdf "_.-HATCH" "_S" bound_ss "" "")
              
              ;; Pega a nova hachura criada
              (setq newhatch (entlast))
              
              ;; Aplica as propriedades da primeira hachura selecionada para a nova
              (vl-cmdf "_.MATCHPROP" firstEnt newhatch "")
              
              ;; Deleta os limites auxiliares gerados
              (vl-cmdf "_.ERASE" bound_ss "")
              
              ;; Deleta as hachuras originais selecionadas
              (vl-cmdf "_.ERASE" ss "")
              
              (setvar "NOMUTT" oldNomutt)
              (princ (strcat "\n" (itoa (sslength ss)) " hachuras foram unidas com sucesso em 1."))
            )
            (progn
              (setvar "NOMUTT" oldNomutt)
              (princ "\nErro: Não foi possível gerar os limites das hachuras.")
            )
          )
        )
        (princ "\nÉ necessário selecionar pelo menos 2 hachuras para unir.")
      )
    )
    (princ "\nNenhuma hachura selecionada.")
  )
  
  ;; Restaura configurações de sistema
  (setvar "CMDECHO" oldCmd)
  (if oldNomutt (setvar "NOMUTT" oldNomutt))
  (princ)
)

(princ "\nArquivo BB_MERGEHATCH carregado. Digite MH para usar o comando.")
(princ)
