;;; ==========================================================================
;;; Sistema de Numeração Sequencial de Blocos no AutoCAD
;;; ==========================================================================
;;; Descrição: Cria textos (MTEXT, tamanho 0.4) com prefixo "R" e sequencial
;;;            de dois dígitos (ex: R01, R02...) para os blocos selecionados.
;;;            A numeração segue a posição dos blocos: de cima para baixo e
;;;            da esquerda para a direita.
;;;            Se algum bloco for apagado e o comando REGEN for executado,
;;;            os rótulos são atualizados e reordenados automaticamente.
;;;
;;; Comandos:
;;;   RNB      - Inicia a numeração selecionando os blocos modelo.
;;;   RNBCLEAR - Limpa todas as numerações e o registro da memória do desenho.
;;; ==========================================================================

(vl-load-com)

;; ==========================================================================
;; CONFIGURAÇÃO E CONSTANTES
;; ==========================================================================
(setq *renumber-text-height* 0.4)      ; Altura do texto MTEXT
(setq *renumber-text-offset* 0.2)      ; Distância do texto acima do bounding box do bloco

;; ==========================================================================
;; REGISTRO E OBTENÇÃO DOS DADOS NO DICIONÁRIO DO DESENHO (Persistência)
;; ==========================================================================
(defun put-stored-block-names (names)
  (vlax-ldata-put "BLOCK_RENUMBER_DICT" "BLOCK_NAMES" names)
)

(defun get-stored-block-names ()
  (vlax-ldata-get "BLOCK_RENUMBER_DICT" "BLOCK_NAMES")
)

;; ==========================================================================
;; ASSOCIAÇÃO XDATA (Vincula o texto MTEXT ao cabo/handle do bloco correspondente)
;; ==========================================================================
(defun set-block-link (text-ename block-handle)
  (regapp "BLOCK_RENUMBER_APP")
  (entmod
    (append (entget text-ename)
      (list
        (list -3
          (list "BLOCK_RENUMBER_APP"
            (cons 1005 block-handle)
          )
        )
      )
    )
  )
)

(defun get-block-link (text-ename / xdata app-data)
  (setq xdata (assoc -3 (entget text-ename '("BLOCK_RENUMBER_APP"))))
  (if xdata
    (progn
      (setq app-data (car (cdr xdata)))
      (cdr (assoc 1005 (cdr app-data)))
    )
    nil
  )
)

;; ==========================================================================
;; BUSCA O CENTRO SUPERIOR DO BLOCO COM BASE NO BOUNDING BOX
;; ==========================================================================
(defun get-block-top-center (obj / minpt maxpt min-list max-list x y z)
  (if (not (vl-catch-all-error-p
             (vl-catch-all-apply 'vla-GetBoundingBox (list obj 'minpt 'maxpt))
           ))
    (progn
      (setq min-list (vlax-safearray->list (vlax-variant-value minpt)))
      (setq max-list (vlax-safearray->list (vlax-variant-value maxpt)))
      (list
        (/ (+ (car min-list) (car max-list)) 2.0) ; Centro em X
        (cadr max-list)                           ; Topo em Y
        (caddr min-list)                          ; Z (base)
      )
    )
    ;; Fallback: usa o ponto de inserção padrão caso o bounding box falhe
    (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint obj)))
  )
)

;; Retorna a altura total do bloco para cálculo dinâmico da tolerância de linha
(defun get-block-height (obj / minpt maxpt min-list max-list)
  (if (not (vl-catch-all-error-p
             (vl-catch-all-apply 'vla-GetBoundingBox (list obj 'minpt 'maxpt))
           ))
    (progn
      (setq min-list (vlax-safearray->list (vlax-variant-value minpt)))
      (setq max-list (vlax-safearray->list (vlax-variant-value maxpt)))
      (- (cadr max-list) (cadr min-list))
    )
    0.0
  )
)

;; ==========================================================================
;; FORMATAÇÃO SEQUENCIAL (R01, R02...)
;; ==========================================================================
(defun format-sequence-number (num)
  (if (< num 10)
    (strcat "R0" (itoa num))
    (strcat "R" (itoa num))
  )
)

;; ==========================================================================
;; ORDENAÇÃO DE BLOCOS (De cima para baixo, da esquerda para a direita)
;; ==========================================================================
(defun sort-blocks (blocks / block-list first-obj height tol rows current-row row-y sorted-blocks sorted-row pos)
  (if blocks
    (progn
      ;; Calcula a tolerância de linha dinâmica (80% da altura do bloco)
      (setq first-obj (car blocks))
      (setq height (get-block-height first-obj))
      (if (> height 0.0)
        (setq tol (* 0.8 height))
        (setq tol 0.6)
      )
      
      ;; Monta a lista com os objetos e suas coordenadas de inserção (X e Y)
      (setq block-list nil)
      (foreach obj blocks
        (setq pos (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint obj))))
        (setq block-list (cons (list obj (car pos) (cadr pos)) block-list))
      )
      
      ;; Ordena primariamente por Y decrescente (do topo para o rodapé)
      (setq block-list
        (vl-sort block-list
          '(lambda (a b)
             (> (caddr a) (caddr b))
           )
        )
      )
      
      ;; Agrupa em "linhas" com base na tolerância
      (setq rows nil)
      (setq current-row nil)
      (setq row-y nil)
      (foreach item block-list
        (setq y (caddr item))
        (if (not current-row)
          (progn
            (setq current-row (list item))
            (setq row-y y)
          )
          (if (< (abs (- row-y y)) tol)
            (setq current-row (cons item current-row))
            (progn
              (setq rows (cons current-row rows))
              (setq current-row (list item))
              (setq row-y y)
            )
          )
        )
      )
      (if current-row
        (setq rows (cons current-row rows))
      )
      (setq rows (reverse rows))
      
      ;; Ordena cada linha individualmente por X crescente (esquerda para a direita)
      (setq sorted-blocks nil)
      (foreach row rows
        (setq sorted-row
          (vl-sort row
            '(lambda (a b)
               (< (cadr a) (cadr b))
             )
          )
        )
        (foreach item sorted-row
          (setq sorted-blocks (cons (car item) sorted-blocks))
        )
      )
      (reverse sorted-blocks)
    )
    nil
  )
)

;; ==========================================================================
;; CRIAÇÃO E ATUALIZAÇÃO DO MTEXT
;; ==========================================================================
(defun create-new-label (block-obj text-str / pos layer dxf-list label-ename)
  (setq pos (get-block-top-center block-obj))
  ;; Eleva o ponto de inserção para ficar ligeiramente acima do topo do bloco
  (setq pos (list (car pos) (+ (cadr pos) *renumber-text-offset*) (caddr pos)))
  (setq layer (vla-get-Layer block-obj))
  
  (setq dxf-list
    (list
      '(0 . "MTEXT")
      '(100 . "AcDbEntity")
      (cons 8 layer)
      '(100 . "AcDbMText")
      (cons 10 pos)
      (cons 40 *renumber-text-height*)
      '(41 . 0.0)                          ; Largura fixa 0 (evita quebra)
      '(71 . 8)                            ; Alinhamento: 8 = Bottom Center (ancora na base do texto)
      (cons 1 text-str)
      '(7 . "Standard")                    ; Estilo padrão
    )
  )
  
  (entmake dxf-list)
  (setq label-ename (entlast))
  (set-block-link label-ename (vla-get-Handle block-obj))
  label-ename
)

(defun update-label-entity (label-ename block-obj text-str / pos layer dxf-list)
  (setq pos (get-block-top-center block-obj))
  (setq pos (list (car pos) (+ (cadr pos) *renumber-text-offset*) (caddr pos)))
  (setq layer (vla-get-Layer block-obj))
  
  (setq dxf-list (entget label-ename))
  (setq dxf-list (subst (cons 10 pos) (assoc 10 dxf-list) dxf-list))
  (setq dxf-list (subst (cons 1 text-str) (assoc 1 dxf-list) dxf-list))
  (setq dxf-list (subst (cons 8 layer) (assoc 8 dxf-list) dxf-list))
  
  (entmod dxf-list)
  (entupd label-ename)
)

;; Obtém todos os rótulos de numeração existentes no desenho
(defun get-existing-labels (/ ss i ent handle label-map)
  (setq label-map nil)
  (regapp "BLOCK_RENUMBER_APP")
  (if (setq ss (ssget "X" '((0 . "MTEXT,TEXT") (-3 ("BLOCK_RENUMBER_APP")))))
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (setq handle (get-block-link ent))
        (if handle
          (setq label-map (cons (cons handle ent) label-map))
        )
        (setq i (1+ i))
      )
    )
  )
  label-map
)

;; ==========================================================================
;; NÚCLEO DE ATUALIZAÇÃO E RENUMERAÇÃO AUTOMÁTICA
;; ==========================================================================
(defun update-all-block-labels (/ registered-names ss i ent obj b-name blocks-by-name 
                                  active-handles label-map valid-label-map b-handle 
                                  label-ent b-list sorted-blocks idx label-pair)
  (setq registered-names (get-stored-block-names))
  (if registered-names
    (progn
      (setq registered-names (mapcar 'strcase registered-names))
      (setq label-map (get-existing-labels))
      
      ;; Seleciona todos os blocos no desenho
      (setq blocks-by-name nil)
      (if (setq ss (ssget "X" '((0 . "INSERT"))))
        (progn
          (setq i 0)
          (while (< i (sslength ss))
            (setq ent (ssname ss i))
            (setq obj (vlax-ename->vla-object ent))
            (setq b-name (strcase (vla-get-EffectiveName obj)))
            (if (member b-name registered-names)
              (progn
                (setq existing-group (assoc b-name blocks-by-name))
                (if existing-group
                  (setq blocks-by-name (subst (cons b-name (cons obj (cdr existing-group))) existing-group blocks-by-name))
                  (setq blocks-by-name (cons (list b-name obj) blocks-by-name))
                )
              )
            )
            (setq i (1+ i))
          )
        )
      )
      
      ;; Mapeia handles dos blocos que estão ativos no desenho
      (setq active-handles nil)
      (foreach group blocks-by-name
        (foreach obj (cdr group)
          (setq active-handles (cons (vla-get-Handle obj) active-handles))
        )
      )
      
      ;; Valida rótulos existentes (deleta órfãos ou duplicados)
      (setq valid-label-map nil)
      (foreach pair label-map
        (setq b-handle (car pair))
        (setq label-ent (cdr pair))
        (if (and (member b-handle active-handles)
                 (entget label-ent)
                 (not (assoc b-handle valid-label-map)))
          (setq valid-label-map (cons pair valid-label-map))
          (if (entget label-ent) (entdel label-ent))
        )
      )
      
      ;; Processa cada grupo de blocos e ordena/re-numera
      (foreach group blocks-by-name
        (setq b-name (car group))
        (setq b-list (cdr group))
        
        ;; Executa ordenação posicional
        (setq sorted-blocks (sort-blocks b-list))
        
        (setq idx 1)
        (foreach b-obj sorted-blocks
          (setq b-handle (vla-get-Handle b-obj))
          (setq text-str (format-sequence-number idx))
          
          (setq label-pair (assoc b-handle valid-label-map))
          (if label-pair
            (setq label-ent (cdr label-pair))
            (progn
              (setq label-ent (create-new-label b-obj text-str))
              (setq valid-label-map (cons (cons b-handle label-ent) valid-label-map))
            )
          )
          
          (update-label-entity label-ent b-obj text-str)
          (setq idx (1+ idx))
        )
      )
    )
  )
  (princ)
)

;; ==========================================================================
;; REATOR DO CAD (Garante a renumeração automática ao dar REGEN)
;; ==========================================================================
(defun renumber-command-ended (reactor params / cmd-name)
  (setq cmd-name (car params))
  (if (or (= cmd-name "REGEN") (= cmd-name "REGENALL"))
    (update-all-block-labels)
  )
)

(defun init-renumber-reactor ()
  ;; Remove reator duplicado antes de criar um novo
  (foreach r-list (vlr-reactors :vlr-command-reactor)
    (foreach r (cdr r-list)
      (if (= (vlr-data r) "BLOCK_RENUMBER_REACTOR")
        (vlr-remove r)
      )
    )
  )
  (vlr-command-reactor "BLOCK_RENUMBER_REACTOR"
    '(
      (:vlr-commandEnded . renumber-command-ended)
    )
  )
)

;; ==========================================================================
;; COMANDO 1: RNB (Registrar e Numerar Blocos)
;; ==========================================================================
(defun c:rnb ( / ss i ent obj name registered-names msg)
  (princ "\nSelecione os blocos para numeracao sequencial: ")
  (if (setq ss (ssget '((0 . "INSERT"))))
    (progn
      (setq registered-names nil)
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (setq obj (vlax-ename->vla-object ent))
        (setq name (vla-get-EffectiveName obj))
        (if (not (member name registered-names))
          (setq registered-names (cons name registered-names))
        )
        (setq i (1+ i))
      )
      
      ;; Grava no dicionário do desenho
      (put-stored-block-names registered-names)
      
      ;; Inicializa o reator
      (init-renumber-reactor)
      
      ;; Executa a renumeração
      (update-all-block-labels)
      
      (princ "\n=======================================================")
      (princ "\nBlocos registrados e numerados com sucesso:")
      (setq msg "")
      (foreach name registered-names
        (setq msg (strcat msg "  * " name "\n"))
      )
      (princ (strcat "\n" msg))
      (princ "\nSe você apagar algum bloco e rodar REGEN, a numeração se autoajustará!")
      (princ "\n=======================================================")
    )
    (princ "\nNenhum bloco válido selecionado.")
  )
  (princ)
)

;; ==========================================================================
;; COMANDO 2: RNBCLEAR (Limpar rótulos e memória)
;; ==========================================================================
(defun c:rnbclear ( / label-map pair label-ent)
  (setq label-map (get-existing-labels))
  (foreach pair label-map
    (setq label-ent (cdr pair))
    (if (entget label-ent)
      (entdel label-ent)
    )
  )
  (vlax-ldata-delete "BLOCK_RENUMBER_DICT" "BLOCK_NAMES")
  
  ;; Remove o reator
  (foreach r-list (vlr-reactors :vlr-command-reactor)
    (foreach r (cdr r-list)
      (if (= (vlr-data r) "BLOCK_RENUMBER_REACTOR")
        (vlr-remove r)
      )
    )
  )
  
  (princ "\nTodos os rotulos foram removidos e o reator desativado.")
  (princ)
)

;; Inicializa o reator e faz o sync inicial ao carregar a lisp
(init-renumber-reactor)
(update-all-block-labels)

(princ "\n=======================================================")
(princ "\n  ROTINA DE NUMERAÇÃO SEQUENCIAL CARREGADA COM SUCESSO!")
(princ "\n  Comandos:")
(princ "\n    RNB      - Selecionar blocos e numerar (R01, R02...)")
(princ "\n    RNBCLEAR - Excluir rótulos e limpar registro da memória")
(princ "\n=======================================================")
(princ)
