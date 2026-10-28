;;; GIRATEXTO v1 - AutoCAD para Windows com suporte a AutoLISP/ActiveX.
;;; APPLOAD -> GIRATEXTO. TEXT e MTEXT; nao explode blocos.
;;; Centro = centro da caixa envolvente WCS, nao ponto de insercao.

(vl-load-com)

(defun gtxt:limites (obj / minimo maximo)
  (vla-GetBoundingBox obj 'minimo 'maximo)
  (list (vlax-safearray->list minimo) (vlax-safearray->list maximo)))

(defun gtxt:centro (limites)
  (mapcar '(lambda (a b) (/ (+ a b) 2.0))
          (car limites) (cadr limites)))

(defun gtxt:unir (a b)
  (list (mapcar 'min (car a) (car b))
        (mapcar 'max (cadr a) (cadr b))))

(defun c:GIRATEXTO (/ *error* doc selecao modo graus indice ent obj dados
                     layer normal normalbase limites total itens item
                     motivo centro eixo radianos undoativo alterados)
  (setq alterados 0)
  (defun *error* (mensagem)
    (if undoativo
      (progn
        (vl-catch-all-apply 'vla-EndUndoMark (list doc))
        (setq undoativo nil)))
    (if (and mensagem
             (not (wcmatch (strcase mensagem) "*CANCEL*,*QUIT*,*EXIT*,*BREAK*")))
      (princ (strcat "\nErro: " mensagem)))
    (if (> alterados 0)
      (princ "\nOperacao interrompida. Digite U para desfazer a rotacao deste lote."))
    (princ))

  (princ "\nGIRATEXTO - selecione somente os textos que deseja girar.")
  (setq selecao (ssget "_I" '((0 . "TEXT,MTEXT"))))
  (if (null selecao)
    (setq selecao (ssget '((0 . "TEXT,MTEXT")))))

  (if selecao
    (progn
      (initget "Conjunto Individual")
      (setq modo (getkword "\nGirar [Conjunto/Individual] <Conjunto>: "))
      (if (null modo) (setq modo "Conjunto"))
      (setq graus (getreal "\nAngulo relativo em graus <180>: "))
      (if (null graus) (setq graus 180.0))
      (setq doc (vla-get-ActiveDocument (vlax-get-acad-object))
            indice 0)

      ;; Prepara o lote inteiro antes de alterar qualquer objeto.
      (repeat (sslength selecao)
        (setq ent (ssname selecao indice)
              dados (entget ent)
              obj (vlax-ename->vla-object ent)
              layer (tblsearch "LAYER" (cdr (assoc 8 dados)))
              normal (cdr (assoc 210 dados)))
        (if (null normal) (setq normal '(0.0 0.0 1.0)))
        (if (null normalbase) (setq normalbase normal))
        (cond
          ((/= 0 (logand 4 (cdr (assoc 70 layer))))
           (setq motivo "Ha texto em layer bloqueada. Desbloqueie-a antes de girar."))
          ((not (vlax-write-enabled-p obj))
           (setq motivo "Ha texto sem permissao de edicao na selecao."))
          ((and (= modo "Conjunto") (not (equal normal normalbase 1e-8)))
           (setq motivo "No modo Conjunto, os textos precisam ter a mesma normal. Use Individual.")))
        (setq limites (gtxt:limites obj)
              itens (cons (list obj (gtxt:centro limites) normal) itens)
              total (if total (gtxt:unir total limites) limites)
              indice (1+ indice)))

      (cond
        (motivo (princ (strcat "\nNenhum texto alterado. " motivo)))
        ((equal (rem graus 360.0) 0.0 1e-10)
         (princ "\nAngulo equivalente a zero. Nenhum texto alterado."))
        (T
         (setq radianos (* pi (/ graus 180.0)))
         (vla-StartUndoMark doc)
         (setq undoativo T)
         (foreach item (reverse itens)
           (setq centro (if (= modo "Conjunto")
                          (gtxt:centro total)
                          (cadr item))
                 normal (if (= modo "Conjunto") normalbase (caddr item))
                 eixo (mapcar '+ centro normal))
           ;; Transformacao do objeto original: preserva texto e formatacao.
           ;; Dois pontos WCS definem o eixo perpendicular ao plano do texto.
           (vla-Rotate3D (car item)
                         (vlax-3d-point centro)
                         (vlax-3d-point eixo)
                         radianos)
           (setq alterados (1+ alterados)))
         (vla-EndUndoMark doc)
         (setq undoativo nil)
         (vla-Regen doc 1)
         (princ (strcat "\n" (itoa alterados) " texto(s) girado(s) em modo "
                        modo ". Digite U para desfazer o lote.")))))
    (princ "\nNenhum TEXT ou MTEXT selecionado."))
  (princ))

(princ "\nGIRATEXTO carregada. Digite GIRATEXTO para girar TEXT/MTEXT pelo centro.")
(princ)
