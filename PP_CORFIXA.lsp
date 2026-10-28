;;; CORFIXA v2 - grava sempre o RGB exato da layer como True Color.
;;; Carregue com APPLOAD e execute CORFIXA antes de trocar a layer.
;;; Substitui qualquer cor anterior, inclusive ACI, ByBlock e True Color.
;;; Altera somente os objetos selecionados, sem editar definicoes de blocos.

(vl-load-com)

(defun corfixa:ler-rgb (layer / cor resultado)
  ;; O AutoCAD resolve o RGB inclusive quando a layer usa indice ACI.
  (setq cor (vla-get-TrueColor layer))
  (setq resultado
    (vl-catch-all-apply
      '(lambda ()
         (list (vla-get-Red cor) (vla-get-Green cor) (vla-get-Blue cor)))))
  (vlax-release-object cor)
  resultado)

(defun corfixa:converter (ent layers / dados layer obj rgb cor resultado esperado)
  (setq dados (entget ent))
  (cond
    ((null dados) 'falha)
    ((= "VIEWPORT" (cdr (assoc 0 dados))) 'falha)
    (T
     (setq layer (vla-Item layers (cdr (assoc 8 dados))))
     (cond
       ((= :vlax-true (vla-get-Lock layer)) 'bloqueado)
       (T
        (setq rgb (corfixa:ler-rgb layer))
        (if (vl-catch-all-error-p rgb)
          rgb
          (progn
            (setq obj (vlax-ename->vla-object ent)
                  cor (vla-get-TrueColor obj))
            ;; SetRGB muda o metodo para RGB; copiar so TrueColor da layer
            ;; manteria o metodo ACI quando a layer tivesse cor indexada.
            (setq resultado
              (vl-catch-all-apply
                '(lambda ()
                   (vla-SetRGB cor (car rgb) (cadr rgb) (caddr rgb))
                   (vla-put-TrueColor obj cor)
                   (vla-Update obj))))
            (vlax-release-object cor)
            (if (vl-catch-all-error-p resultado)
              resultado
              (progn
                ;; Confere os 24 bits RGB gravados, sem aproximacao ACI.
                (setq esperado
                  (+ (* 65536 (car rgb)) (* 256 (cadr rgb)) (caddr rgb)))
                (if (equal esperado (cdr (assoc 420 (entget ent))))
                  rgb
                  'falha))))))))))

(defun c:CORFIXA (/ *error* doc layers selecao indice resultado alterados
                    bloqueados falhas undoativo ent)
  (defun *error* (mensagem)
    (if undoativo
      (progn
        (command-s "_.UNDO" "_End")
        (setq undoativo nil)))
    (if (and mensagem
             (not (wcmatch (strcase mensagem) "*CANCEL*,*QUIT*,*EXIT*,*BREAK*")))
      (princ (strcat "\nErro: " mensagem)))
    (princ))

  (princ "\nCORFIXA v2 - gravar RGB da layer nos objetos/blocos selecionados: ")
  ;; Aceita tambem os objetos pre-selecionados com PICKFIRST.
  (setq selecao (ssget "_I"))
  (if (null selecao) (setq selecao (ssget)))
  (if selecao
    (progn
      (setq doc (vla-get-ActiveDocument (vlax-get-acad-object))
            layers (vla-get-Layers doc)
            indice 0 alterados 0 bloqueados 0 falhas 0)
      (command-s "_.UNDO" "_Begin")
      (setq undoativo T)
      (repeat (sslength selecao)
        (setq ent (ssname selecao indice)
              resultado
          (vl-catch-all-apply 'corfixa:converter
            (list ent layers)))
        (cond
          ((vl-catch-all-error-p resultado)
           (setq falhas (1+ falhas))
           (princ (strcat "\nFalha: " (vl-catch-all-error-message resultado))))
          ((and (listp resultado) (= 3 (length resultado)))
           (setq alterados (1+ alterados))
           (princ
             (strcat "\nObjeto " (cdr (assoc 5 (entget ent))) " -> RGB "
                     (itoa (car resultado)) ","
                     (itoa (cadr resultado)) ","
                     (itoa (caddr resultado)))))
          ((eq resultado 'bloqueado) (setq bloqueados (1+ bloqueados)))
          (T (setq falhas (1+ falhas))))
        (setq indice (1+ indice)))
      (command-s "_.UNDO" "_End")
      (setq undoativo nil)
      (vla-Regen doc 1)
      (princ
        (strcat "\nRGB gravado e conferido: " (itoa alterados)
                "; em layers bloqueadas: " (itoa bloqueados)
                "; nao alterados por falha/restricao: " (itoa falhas)
                ".")))
    (princ "\nNenhum objeto selecionado."))
  (princ))

(defun c:CORRGB () (c:CORFIXA))

(princ "\nCORFIXA v2 carregada. Digite CORRGB ou CORFIXA para gravar RGB.")
(princ)
