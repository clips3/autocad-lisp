(defun c:DelZeroLines ( / ss i ent ed objType p1 p2 numDel obj len)
  (vl-load-com)
  (setq numDel 0)
  
  ;; Seleciona tanto LINHAS quanto POLILINHAS
  (if (setq ss (ssget "X" '((0 . "LINE,LWPOLYLINE,POLYLINE"))))
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (setq ed (entget ent))
        (setq objType (cdr (assoc 0 ed)))
        
        (setq len nil)
        
        ;; Se for uma linha simples
        (if (= objType "LINE")
          (progn
            (setq p1 (cdr (assoc 10 ed)))
            (setq p2 (cdr (assoc 11 ed)))
            (setq len (distance p1 p2))
          )
        )
        
        ;; Se for uma polilinha
        (if (or (= objType "LWPOLYLINE") (= objType "POLYLINE"))
          (progn
             (setq obj (vlax-ename->vla-object ent))
             ;; Usa tratamento de erro caso a polilinha esteja corrompida
             (setq len (vl-catch-all-apply 'vlax-curve-getDistAtParam 
                         (list obj (vl-catch-all-apply 'vlax-curve-getEndParam (list obj)))))
             (if (vl-catch-all-error-p len)
               (setq len nil)
             )
          )
        )
        
        ;; Se encontramos o comprimento e ele é menor ou igual a uma tolerância muito pequena (1e-5)
        (if (and len (<= len 1e-5))
          (progn
            ;; Tenta deletar a entidade (pode falhar se a layer estiver trancada)
            (if (not (vl-catch-all-error-p (vl-catch-all-apply 'entdel (list ent))))
               (setq numDel (1+ numDel))
               (princ "\nFalha ao deletar objeto. Verifique se a Layer está trancada (Locked).")
            )
          )
        )
        (setq i (1+ i))
      )
      (princ (strcat "\nComando concluído: Foram excluídas " (itoa numDel) " entidades com comprimento zero ou quase zero."))
    )
    (princ "\nNenhuma linha ou polilinha encontrada no desenho.")
  )
  (princ)
)
(princ "\nDigite 'DelZeroLines' para executar o comando atualizado.")
(princ)
