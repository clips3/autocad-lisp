;;; PP_ATTSYNCALL.lsp
;;; Comando: SYNCALL
;;; Descrição: Aplica o comando ATTSYNC para todos os blocos do modelo

(defun c:SYNCALL (/ blk blkName flags cmdEcho nomutt)
  (setq cmdEcho (getvar "CMDECHO"))
  (setq nomutt (getvar "NOMUTT"))
  (setvar "CMDECHO" 0)
  (setvar "NOMUTT" 1)
  (princ "\nSincronizando os atributos dos blocos. Aguarde...")
  
  (setq blk (tblnext "BLOCK" T))
  (while blk
    (setq flags (cdr (assoc 70 blk)))
    (setq blkName (cdr (assoc 2 blk)))
    
    ;; Condições para executar o ATTSYNC:
    ;; 1. O bloco possui atributos (bit 2)
    ;; 2. Não é um bloco anônimo (bit 1)
    ;; 3. Não é uma referência externa / XREF (bit 4)
    (if (and 
          (= (logand flags 2) 2)
          (= (logand flags 1) 0)
          (= (logand flags 4) 0)
        )
      (vl-cmdf "_.ATTSYNC" "_N" blkName)
    )
    (setq blk (tblnext "BLOCK"))
  )
  
  (setvar "NOMUTT" nomutt)
  (setvar "CMDECHO" cmdEcho)
  (princ "\nSincronização concluída com sucesso (ATTSYNC em todos os blocos).")
  (princ)
)

(princ "\nRotina BB_ATTSYNCALL carregada. Digite SYNCALL para executar.")
(princ)
