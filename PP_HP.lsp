;;; =========================================================================
;;; Script: PP_HP.lsp
;;; Comando: HP
;;; Descrição: Configura as propriedades padrão de hachura:
;;;            Padrão Sólido, cor ByLayer, sem cor de fundo, 
;;;            transparência 0, na layer atual. Não cria hachura,
;;;            apenas ajusta as variáveis.
;;; =========================================================================

(defun c:hp ()

  ;; Configura as variáveis de hachura
  ;; Padrão Sólido
  (vl-catch-all-apply 'setvar (list "HPNAME" "SOLID"))
  
  ;; Cor em ByLayer
  (vl-catch-all-apply 'setvar (list "HPCOLOR" "BYLAYER"))
  
  ;; Cor de fundo não definida (nenhuma)
  (vl-catch-all-apply 'setvar (list "HPBACKGROUNDCOLOR" "."))
  
  ;; Transparência em 0
  (vl-catch-all-apply 'setvar (list "HPTRANSPARENCY" "0"))
  
  ;; Layer atual (uso de "." para o current layer)
  (vl-catch-all-apply 'setvar (list "HPLAYER" "."))

  (princ "\nPropriedades de hachura configuradas: Solid, ByLayer, Sem Fundo, Transp 0, Layer atual.")
  (princ)
)

(princ "\nComando 'HP' carregado com sucesso. Digite 'HP' para configurar a hachura.")
(princ)