;;; RENOMHVAC v1 - numera o atributo NOME das referencias selecionadas.
;;; APPLOAD -> RENOMHVAC -> selecione os blocos -> prefixo <UE->.
;;; Por linhas: Y decrescente; dentro de cada linha, X crescente (UCS atual).
;;; Nao edita definicoes, parametros dinamicos nem os outros atributos.

(defun rnm:valor (codigo dados padrao / par)
  (if (setq par (assoc codigo dados)) (cdr par) padrao))

(defun rnm:bloqueado (dados / layer)
  (setq layer (tblsearch "LAYER" (cdr (assoc 8 dados))))
  (/= 0 (logand 4 (rnm:valor 70 layer 0))))

(defun rnm:atributos (bloco / ent dados alvos)
  (if (= 1 (rnm:valor 66 (entget bloco) 0))
    (progn
      (setq ent (entnext bloco))
      (while (and ent (= "ATTRIB" (cdr (assoc 0 (setq dados (entget ent))))))
        (if (= "NOME" (strcase (rnm:valor 2 dados "")))
          (setq alvos (cons ent alvos)))
        (setq ent (entnext ent)))))
  (reverse alvos))

(defun rnm:motivo (bloco alvos / dados tabela motivo ent ad)
  (setq dados (entget bloco)
        tabela (tblsearch "BLOCK" (cdr (assoc 2 dados))))
  (cond
    ((/= "INSERT" (cdr (assoc 0 dados))) "nao e referencia de bloco")
    ((/= 0 (logand 12 (rnm:valor 70 tabela 0))) "referencia externa")
    ((or (> (rnm:valor 70 dados 1) 1) (> (rnm:valor 71 dados 1) 1)) "matriz MINSERT")
    ((rnm:bloqueado dados) "layer do bloco bloqueada")
    ((null alvos) "sem atributo NOME editavel")
    (T
     (foreach ent alvos
       (setq ad (entget ent))
       (cond
         ((rnm:bloqueado ad) (setq motivo "layer do atributo NOME bloqueada"))
         ((/= 0 (logand 2 (rnm:valor 70 ad 0))) (setq motivo "atributo NOME constante"))
         ((or (member '(100 . "AcDbMText") ad) (assoc 101 ad)) (setq motivo "NOME multilinha"))
         ((or (vl-string-search "%<" (rnm:valor 1 ad ""))
              (and (assoc 360 ad) (dictsearch (cdr (assoc 360 ad)) "ACAD_FIELD")))
          (setq motivo "NOME controlado por campo"))))
     motivo)))

;;; Item: (bloco lista-de-atributos ponto-UCS indice-da-selecao altura).
(defun rnm:coletar (selecao / indice bloco alvos motivo ponto altura ent itens ignorados)
  (setq indice 0)
  (repeat (sslength selecao)
    (setq bloco (ssname selecao indice)
          alvos (rnm:atributos bloco)
          motivo (rnm:motivo bloco alvos))
    (if motivo
      (setq ignorados (cons (list bloco motivo) ignorados))
      (progn
        (setq ponto (trans (cdr (assoc 10 (entget bloco))) bloco 1) altura 0.0)
        (foreach ent alvos
          (setq altura (max altura (abs (rnm:valor 40 (entget ent) 0.0)))))
        (setq itens (cons (list bloco alvos ponto indice altura) itens))))
    (setq indice (1+ indice)))
  (list (reverse itens) (reverse ignorados)))

(defun rnm:ordenar (itens / alturas mediana tolerancia escala fila linha referencia saida item)
  (if itens
    (progn
      (setq alturas (mapcar '(lambda (it) (nth 4 it)) itens)
            mediana (nth (nth (fix (/ (length alturas) 2)) (vl-sort-i alturas '<)) alturas)
            escala (apply 'max (mapcar '(lambda (it) (abs (cadr (nth 2 it)))) itens)))
      ;; Uma ancora fixa por linha evita que varios pequenos desvios unam
      ;; duas linhas distintas por encadeamento. A altura vem do ATTRIB real.
      (setq tolerancia (max (* 0.5 mediana) (* 1e-12 (max 1.0 escala))))
      ;; Indice unico como desempate: referencias sobrepostas nao somem.
      (setq fila (vl-sort itens
        '(lambda (a b)
           (if (= (cadr (nth 2 a)) (cadr (nth 2 b)))
             (< (nth 3 a) (nth 3 b))
             (> (cadr (nth 2 a)) (cadr (nth 2 b)))))))
      (while fila
        (setq referencia (cadr (nth 2 (car fila))) linha nil)
        (while (and fila (<= (- referencia (cadr (nth 2 (car fila)))) tolerancia))
          (setq linha (cons (car fila) linha) fila (cdr fila)))
        (setq linha (vl-sort linha
          '(lambda (a b)
             (if (= (car (nth 2 a)) (car (nth 2 b)))
               (if (= (cadr (nth 2 a)) (cadr (nth 2 b)))
                 (< (nth 3 a) (nth 3 b))
                 (> (cadr (nth 2 a)) (cadr (nth 2 b))))
               (< (car (nth 2 a)) (car (nth 2 b)))))))
        (foreach item linha (setq saida (cons item saida))))
      (reverse saida))))

(defun rnm:numero (prefixo numero / texto)
  (setq texto (itoa numero))
  (if (< numero 10) (setq texto (strcat "0" texto)))
  (strcat prefixo texto))

;;; Plano: ((atributo bloco dados-originais novo-valor) ...).
(defun rnm:plano (itens prefixo / numero item ent plano)
  (setq numero 1)
  (foreach item itens
    (foreach ent (cadr item)
      (setq plano (cons (list ent (car item) (entget ent) (rnm:numero prefixo numero)) plano)))
    (setq numero (1+ numero)))
  (reverse plano))

(defun rnm:restaurar (plano / item resultado ok)
  (setq ok T)
  (foreach item plano
    (setq resultado (vl-catch-all-apply 'entmod (list (nth 2 item))))
    (if (or (vl-catch-all-error-p resultado) (null resultado)) (setq ok nil))
    (vl-catch-all-apply 'entupd (list (cadr item))))
  ok)

(defun rnm:aplicar (plano / item dados resultado ok)
  (setq ok T)
  (while (and plano ok)
    (setq item (car plano) dados (nth 2 item)
          resultado (if (entget (car item))
            (vl-catch-all-apply 'entmod
              (list (subst (cons 1 (nth 3 item)) (assoc 1 dados) dados)))
            nil))
    (if (or (vl-catch-all-error-p resultado) (null resultado)
            (not (equal (nth 3 item) (cdr (assoc 1 (entget (car item)))))))
      (setq ok nil)
      (progn
        (setq resultado (vl-catch-all-apply 'entupd (list (cadr item))))
        (if (vl-catch-all-error-p resultado) (setq ok nil))))
    (setq plano (cdr plano)))
  ok)

(defun rnm:processar (selecao prefixo / *error* coleta itens ignorados item plano undoativo escrevendo ok)
  (defun *error* (mensagem / restaurado)
    (if escrevendo (setq restaurado (rnm:restaurar plano)))
    (if undoativo (progn (command-s "_.UNDO" "_End") (setq undoativo nil)))
    (if (and mensagem (not (wcmatch (strcase mensagem) "*CANCEL*,*QUIT*,*EXIT*,*BREAK*")))
      (princ (strcat "\nErro: " mensagem)))
    (if escrevendo
      (princ (if restaurado "\nLote cancelado; valores anteriores restaurados."
                            "\nOperacao interrompida. Use U para desfazer o lote.")))
    (princ))
  (setq coleta (rnm:coletar selecao)
        itens (rnm:ordenar (car coleta)) ignorados (cadr coleta))
  (foreach item ignorados
    (princ (strcat "\nIgnorado [" (cdr (assoc 5 (entget (car item)))) "]: " (cadr item) ".")))
  (if itens
    (progn
      ;; O lote inteiro e preparado antes de alterar o primeiro atributo.
      (setq plano (rnm:plano itens prefixo))
      (command-s "_.UNDO" "_Begin")
      (setq undoativo T escrevendo T)
      (setq ok (rnm:aplicar plano))
      (if (not ok)
        (princ (if (rnm:restaurar plano)
                 "\nFalha na gravacao; valores anteriores restaurados."
                 "\nFalha na gravacao/restauracao. Use U para desfazer o lote.")))
      (setq escrevendo nil)
      (command-s "_.UNDO" "_End")
      (setq undoativo nil)
      (if ok
        (princ (strcat "\n" (itoa (length itens)) " bloco(s) renumerado(s): "
                       (rnm:numero prefixo 1) " a " (rnm:numero prefixo (length itens))
                       ". Ignorados: " (itoa (length ignorados)) ". Use U para desfazer."))))
    (princ "\nNenhum bloco com NOME editavel na selecao."))
  (list ok (if ok (length itens) 0) (length ignorados)))

(defun c:RENOMHVAC (/ selecao prefixo)
  (cond
    ((= 1 (getvar "BLOCKEDITOR"))
     (princ "\nFeche o BEDIT. Execute RENOMHVAC nas referencias inseridas no desenho."))
    (T
     (princ "\nSelecione os blocos para renumerar o atributo NOME: ")
     (setq selecao (ssget "_I" '((0 . "INSERT"))))
     (if (null selecao) (setq selecao (ssget '((0 . "INSERT")))))
     (if selecao
       (progn
         (setq prefixo (vl-string-trim " \t" (getstring T "\nPrefixo <UE->: ")))
         (if (= prefixo "") (setq prefixo "UE-"))
         (rnm:processar selecao prefixo))
       (princ "\nNenhum bloco selecionado."))))
  (princ))

(princ "\nRENOMHVAC v1 carregada. Digite RENOMHVAC para numerar o atributo NOME.")
(princ)
