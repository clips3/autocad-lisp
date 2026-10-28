(defun c:RN ( / sel ent blkName vlaObj newName)
  (vl-load-com)
  
  ;; Verifica se há seleção prévia (implied selection)
  (setq sel (ssget "_I" '((0 . "INSERT"))))
  
  ;; Se não houver seleção prévia, pede ao usuário para selecionar
  (if (not sel)
    (progn
      (prompt "\nSelecione o bloco a ser renomeado: ")
      (setq sel (ssget "_+.:E:S" '((0 . "INSERT"))))
    )
  )
  
  (if sel
    (progn
      (setq ent (ssname sel 0))
      (setq blkName (cdr (assoc 2 (entget ent))))
      
      ;; Se for bloco dinâmico modificado, precisamos do nome original real (EffectiveName)
      (setq vlaObj (vlax-ename->vla-object ent))
      (if (vlax-property-available-p vlaObj 'EffectiveName)
        (setq blkName (vla-get-EffectiveName vlaObj))
      )
      
      ;; Verifica se é um bloco anônimo (começa com *)
      (if (eq (substr blkName 1 1) "*")
        (princ "\nErro: Não é possível renomear blocos anônimos via script básico.")
        (progn
          ;; Pede o novo nome (a flag 't' permite espaços no nome)
          (setq newName (getstring t (strcat "\nDigite o novo nome para o bloco [" blkName "]: ")))
          
          (cond
            ((eq newName "")
             (princ "\nOperação cancelada.")
            )
            ((tblobjname "BLOCK" newName)
             (princ (strcat "\nErro: Já existe um bloco no desenho com o nome '" newName "'."))
            )
            (t
             ;; Executa o comando RENAME nativo do AutoCAD
             (command "_.RENAME" "_B" blkName newName)
             (princ (strcat "\nSucesso: Bloco renomeado de '" blkName "' para '" newName "'."))
            )
          )
        )
      )
    )
    (princ "\nNenhum bloco selecionado.")
  )
  
  ;; Limpa a seleção e grips que possam ter ficado ativos
  (sssetfirst nil nil)
  (princ)
)

(princ "\nScript carregado com sucesso. Digite RN para usar o comando.")
(princ)
