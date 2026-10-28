;;; ==========================================================================
;;; BB_DELETE SYSTEM (DS)
;;; ==========================================================================
;;; Descrição: Deleta objetos gerados pela lisp BB_PIPE utilizando a 
;;;            identificação via XData (-3 "BB_PIPE").
;;; Comando: DS
;;; ==========================================================================

(defun c:DS (/ doc ss i ent count ent-data xdata obj)
  (vl-load-com)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (princ "\nSelecione a area para deletar o sistema gerado pelo BB_PIPE: ")
  
  (if (not (tblsearch "APPID" "BB_PIPE"))
    (regapp "BB_PIPE")
  )
  
  ;; Realiza uma selecao normal para nao quebrar a interface de caixa de selecao (Window/Lasso)
  (if (setq ss (ssget))
    (progn
      (vla-StartUndoMark doc)
      (setq i 0)
      (setq count 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (if (and ent (setq ent-data (entget ent '("BB_PIPE"))))
          (progn
            ;; Verifica se o XData esta presente no objeto
            (setq xdata (assoc -3 ent-data))
            (if xdata
              (progn
                (entdel ent)
                (setq count (1+ count))
              )
            )
          )
        )
        (setq i (1+ i))
      )
      (princ (strcat "\nSucesso! Foram excluidos " (itoa count) " objetos do sistema BB_PIPE."))
      (vla-EndUndoMark doc)
    )
    (princ "\nNenhum objeto foi selecionado.")
  )
  (princ)
)

(princ "\nComando carregado. Digite DS para iniciar a delecao do sistema.")
(princ)
