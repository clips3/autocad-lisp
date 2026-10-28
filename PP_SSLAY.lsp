(defun c:SL (/ ss i ent lay layers layerStr)
  (setq ss (ssget "_I")) ; Tenta pegar a seleção atual (PickFirst)
  
  (if (not ss) ; Se não houver seleção prévia
    (setq ss (ssget)) ; Solicita seleção ao usuário
  )

  (if ss
    (progn
      (setq i 0)
      (setq layers '())
      
      ; Itera sobre os objetos selecionados para coletar as layers
      (repeat (sslength ss)
        (setq ent (entget (ssname ss i)))
        (setq lay (cdr (assoc 8 ent)))
        (if (not (member lay layers))
          (setq layers (cons lay layers))
        )
        (setq i (1+ i))
      )
      
      ; Constroi a string de filtro de layers (separada por vírgula)
      (setq layerStr "")
      (foreach l layers
        (if (= layerStr "")
          (setq layerStr l)
          (setq layerStr (strcat layerStr "," l))
        )
      )
      
      ; Seleciona todos os objetos das layers encontradas
      (if (setq ss (ssget "_X" (list (cons 8 layerStr))))
        (progn
          (sssetfirst nil ss)
          (princ (strcat "\nSelecionados " (itoa (sslength ss)) " objetos na(s) layer(s): " layerStr))
        )
        (princ "\nErro ao selecionar objetos.")
      )
    )
    (princ "\nNenhum objeto selecionado.")
  )
  (princ)
)
