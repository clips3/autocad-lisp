;;; PP_PPOFF.lsp
;;; Comando: PPO
;;; Funcionalidade: Desliga todas as layers que iniciam com "PP_"

(defun c:PPO (/ doc layers layName) 
  (vl-load-com)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq layers (vla-get-Layers doc))

  (vlax-for layObj layers 
    (setq layName (vla-get-Name layObj))
    ;; Verifica se o nome da layer comeca com "PP_" (ignorando maiusculas e minusculas)
    (if (wcmatch (strcase layName) "PP_*") 
      ;; Usa vl-catch-all-apply para evitar erro caso a layer seja a atual
      (vl-catch-all-apply 'vla-put-LayerOn (list layObj :vlax-false))
    )
  )
  (princ "\nTodas as layers iniciadas com 'PP_' foram desligadas.")
  (princ)
)

(princ "\nRotina BB_PPOFF carregada com sucesso. Digite PPO para executar.")
(princ)
