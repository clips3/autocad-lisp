;;; PP_QUANTITATIVO_TUBOS.lsp
;;; Soma comprimentos por cor/diametro e cria uma tabela com conexoes em T.
;;; Compativel com AutoCAD para Windows (Visual LISP / ActiveX).

(vl-load-com)

(setq *qt-version* "1.2")

;; Configuracoes que podem ser alteradas antes de carregar o arquivo.
(if (not (boundp '*qt-color-map*))
  (setq *qt-color-map* nil))
(if (not (boundp '*qt-default-color-map*))
  (setq *qt-default-color-map*
    '(
      ;; Cores exatas da legenda fornecida pelo usuario.
      ("RGB:0,128,255" . 16.0)  ; azul
      ("RGB:255,0,255" . 20.0)  ; magenta
      ("RGB:192,129,129" . 26.0) ; rosa
      ("RGB:0,135,137" . 32.0)  ; verde-petroleo
      ;; Equivalentes basicos caso azul/magenta tenham sido usados como ACI.
      ("ACI:5" . 16.0)
      ("ACI:6" . 20.0))))
(if (not (boundp '*qt-length-to-meter*))
  (setq *qt-length-to-meter* 0.001)) ; desenho em mm -> metros
(if (not (boundp '*qt-text-height*))
  (setq *qt-text-height* 2.5))
(if (not (boundp '*qt-node-tolerance*))
  (setq *qt-node-tolerance* 0.0001))
(if (not (boundp '*qt-item-name*))
  (setq *qt-item-name* "Tubo multicamada"))

(defun qt-vla-color-key (object / color red green blue result)
  ;; Converte inclusive uma cor ACI para o RGB que o AutoCAD exibe.
  (if (and object
           (not
             (vl-catch-all-error-p
               (setq color
                 (vl-catch-all-apply 'vla-get-TrueColor (list object))))))
    (progn
      (setq red (vl-catch-all-apply 'vla-get-Red (list color))
            green (vl-catch-all-apply 'vla-get-Green (list color))
            blue (vl-catch-all-apply 'vla-get-Blue (list color)))
      (if (and (not (vl-catch-all-error-p red))
               (not (vl-catch-all-error-p green))
               (not (vl-catch-all-error-p blue)))
        (setq result
          (strcat
            "RGB:" (itoa red) "," (itoa green) "," (itoa blue))))
      (if color (vlax-release-object color))))
  result)

(defun qt-entity-vla-color-key (ename / object result)
  (if (not
        (vl-catch-all-error-p
          (setq object
            (vl-catch-all-apply 'vlax-ename->vla-object (list ename)))))
    (progn
      (setq result (qt-vla-color-key object))
      (if object (vlax-release-object object))))
  result)

(defun qt-layer-vla-color-key (layer-name / app document layers layer result)
  (if (and
        (not
          (vl-catch-all-error-p
            (setq app (vl-catch-all-apply 'vlax-get-acad-object nil))))
        app
        (not
          (vl-catch-all-error-p
            (setq document
              (vl-catch-all-apply 'vla-get-ActiveDocument (list app)))))
        document
        (not
          (vl-catch-all-error-p
            (setq layers
              (vl-catch-all-apply 'vla-get-Layers (list document)))))
        layers
        (not
          (vl-catch-all-error-p
            (setq layer
              (vl-catch-all-apply 'vla-Item (list layers layer-name))))))
    (setq result (qt-vla-color-key layer)))
  (if layer (vlax-release-object layer))
  (if layers (vlax-release-object layers))
  result)

(defun qt-replace-all (new old txt / pos)
  (while (setq pos (vl-string-search old txt))
    (setq txt
      (strcat
        (substr txt 1 pos)
        new
        (substr txt (+ pos (strlen old) 1)))))
  txt)

(defun qt-trim-zeroes (txt)
  (if (vl-string-search "." txt)
    (progn
      (while (= (substr txt (strlen txt) 1) "0")
        (setq txt (substr txt 1 (1- (strlen txt)))))
      (if (= (substr txt (strlen txt) 1) ".")
        (setq txt (substr txt 1 (1- (strlen txt)))))))
  txt)

(defun qt-format-diameter (value)
  (qt-replace-all "," "." (qt-trim-zeroes (rtos value 2 3))))

(defun qt-group-thousands (txt / n first result pos)
  (setq n (strlen txt)
        first (rem n 3))
  (if (= first 0) (setq first 3))
  (setq result (substr txt 1 first)
        pos (+ first 1))
  (while (<= pos n)
    (setq result (strcat result "." (substr txt pos 3))
          pos (+ pos 3)))
  result)

(defun qt-format-number-br (value decimals / raw dot integer fraction)
  (setq raw (rtos value 2 decimals)
        dot (vl-string-search "." raw))
  (if dot
    (setq integer (substr raw 1 dot)
          fraction (substr raw (+ dot 2)))
    (setq integer raw
          fraction nil))
  (strcat
    (qt-group-thousands integer)
    (if fraction (strcat "," fraction) "")))

(defun qt-rgb-key (value / red green blue)
  (setq red   (logand 255 (lsh value -16))
        green (logand 255 (lsh value -8))
        blue  (logand 255 value))
  (strcat "RGB:" (itoa red) "," (itoa green) "," (itoa blue)))

(defun qt-effective-color-key (ename / data aci true layer-data aci-key rgb-key)
  (setq data (entget ename)
        true (cdr (assoc 420 data))
        aci (cdr (assoc 62 data)))
  (cond
    (true (qt-rgb-key true))
    ((and aci (> aci 0) (< aci 256))
      (setq aci-key (strcat "ACI:" (itoa aci)))
      ;; Mantem os ACI explicitamente cadastrados. Os demais sao convertidos
      ;; ao RGB efetivamente exibido para permitir comparacao por proximidade.
      (if (assoc aci-key *qt-default-color-map*)
        aci-key
        (progn
          (setq rgb-key (qt-entity-vla-color-key ename))
          (if rgb-key rgb-key aci-key))))
    ((or (null aci) (= aci 256))
      (setq layer-data (tblsearch "LAYER" (cdr (assoc 8 data))))
      (cond
        ((cdr (assoc 420 layer-data))
          (qt-rgb-key (cdr (assoc 420 layer-data))))
        ((cdr (assoc 62 layer-data))
          (setq aci-key
            (strcat "ACI:" (itoa (abs (cdr (assoc 62 layer-data))))))
          (if (assoc aci-key *qt-default-color-map*)
            aci-key
            (progn
              (setq rgb-key
                (qt-layer-vla-color-key (cdr (assoc 8 data))))
              (if rgb-key rgb-key aci-key))))
        (T "ACI:7")))
    (T "ACI:7"))) ; PorBloco fora de um bloco

(defun qt-color-description (key / n name)
  (if (= (substr key 1 4) "ACI:")
    (progn
      (setq n (atoi (substr key 5))
            name
              (cond
                ((= n 1) "vermelho")
                ((= n 2) "amarelo")
                ((= n 3) "verde")
                ((= n 4) "ciano")
                ((= n 5) "azul")
                ((= n 6) "magenta")
                ((= n 7) "branco/preto")
                (T nil)))
      (if name
        (strcat key " (" name ")")
        key))
    key))

(defun qt-map-set (key diameter)
  (setq *qt-color-map*
    (cons
      (cons key diameter)
      (vl-remove-if
        '(lambda (pair) (= (car pair) key))
        *qt-color-map*))))

(defun qt-rgb-key-values (key / text)
  (if (= (substr key 1 4) "RGB:")
    (progn
      (setq text (vl-string-translate "," " " (substr key 5)))
      (read (strcat "(" text ")")))))

(defun qt-rgb-distance-squared (a b)
  (+
    (* (- (car a) (car b)) (- (car a) (car b)))
    (* (- (cadr a) (cadr b)) (- (cadr a) (cadr b)))
    (* (- (caddr a) (caddr b)) (- (caddr a) (caddr b)))))

(defun qt-nearest-default-pair (key / rgb item item-rgb distance best best-distance)
  (if (setq rgb (qt-rgb-key-values key))
    (foreach item *qt-default-color-map*
      (if (setq item-rgb (qt-rgb-key-values (car item)))
        (progn
          (setq distance (qt-rgb-distance-squared rgb item-rgb))
          (if (or (null best-distance) (< distance best-distance))
            (setq best item best-distance distance))))))
  ;; Tolera pequenas diferencas de paleta/arredondamento: ate 30 niveis RGB.
  (if (and best-distance (<= best-distance 900)) best nil))

(defun qt-map-pair (key / pair)
  ;; Um mapeamento manual sempre prevalece sobre a legenda automatica.
  (setq pair (assoc key *qt-color-map*))
  (if (null pair)
    (setq pair (assoc key *qt-default-color-map*)))
  (if (null pair)
    (setq pair (qt-nearest-default-pair key)))
  pair)

(defun qt-map-get (key / pair)
  (if (setq pair (qt-map-pair key)) (cdr pair) nil))

(defun qt-map-known-p (key)
  (if (qt-map-pair key) T nil))

(defun qt-unique (values / result value)
  (foreach value values
    (if (not (member value result))
      (setq result (cons value result))))
  (reverse result))

(defun qt-read-diameter (key / answer value done)
  (while (not done)
    (setq answer
      (getstring T
        (strcat
          "\nDiametro em mm para " (qt-color-description key)
          " (0 = ignorar): ")))
    (setq answer (vl-string-translate "," "." answer))
    (if (and (/= answer "")
             (setq value (distof answer 2))
             (>= value 0.0))
      (setq done T)
      (prompt "\nInforme um numero maior ou igual a zero.")))
  value)

(defun qt-ensure-mappings (keys / key)
  (foreach key keys
    (if (not (qt-map-known-p key))
      (qt-map-set key (qt-read-diameter key)))))

(defun qt-curve-length (ename / result)
  (setq result
    (vl-catch-all-apply
      'vlax-curve-getDistAtParam
      (list ename (vlax-curve-getEndParam ename))))
  (if (vl-catch-all-error-p result) 0.0 result))

;; Registro de entidade:
;; (ename objeto-com chave-cor diametro comprimento indice)
(defun qt-make-records (selection / i ename key diameter result)
  (setq i 0)
  (repeat (sslength selection)
    (setq ename (ssname selection i)
          key (qt-effective-color-key ename)
          diameter (qt-map-get key))
    (if (and diameter (> diameter 0.0))
      (setq result
        (cons
          (list
            ename
            (vlax-ename->vla-object ename)
            key
            diameter
            (qt-curve-length ename)
            (length result))
          result)))
    (setq i (1+ i)))
  (reverse result))

(defun qt-rec-ename (record) (nth 0 record))
(defun qt-rec-object (record) (nth 1 record))
(defun qt-rec-diameter (record) (nth 3 record))
(defun qt-rec-length (record) (nth 4 record))

(defun qt-variant-points (object-a object-b / variant array raw result)
  (setq variant
    (vl-catch-all-apply
      'vla-IntersectWith
      (list object-a object-b 0))) ; acExtendNone
  (if (not (vl-catch-all-error-p variant))
    (progn
      (setq array
        (vl-catch-all-apply 'vlax-variant-value (list variant)))
      (if (not (vl-catch-all-error-p array))
        (progn
          (setq raw
            (vl-catch-all-apply 'vlax-safearray->list (list array)))
          (if (not (vl-catch-all-error-p raw))
            (while (>= (length raw) 3)
              (setq result
                (cons (list (car raw) (cadr raw) (caddr raw)) result)
                    raw (cdddr raw))))))))
  (reverse result))

(defun qt-add-id (id ids)
  (if (member id ids) ids (cons id ids)))

(defun qt-node-add (nodes point id-a id-b tolerance / result node found ids)
  (foreach node nodes
    (if (and (not found) (<= (distance point (car node)) tolerance))
      (progn
        (setq ids (qt-add-id id-b (qt-add-id id-a (cadr node)))
              result (cons (list (car node) ids) result)
              found T))
      (setq result (cons node result))))
  (if (not found)
    (setq result (cons (list point (list id-a id-b)) result)))
  (reverse result))

(defun qt-find-nodes (records tolerance / count i j points point nodes)
  (setq count (length records)
        i 0)
  (while (< i count)
    (setq j (1+ i))
    (while (< j count)
      (setq points
        (qt-variant-points
          (qt-rec-object (nth i records))
          (qt-rec-object (nth j records))))
      (foreach point points
        (setq nodes (qt-node-add nodes point i j tolerance)))
      (setq j (1+ j)))
    (setq i (1+ i)))
  nodes)

(defun qt-closed-p (ename / value)
  (setq value (vl-catch-all-apply 'vlax-curve-isClosed (list ename)))
  (and (not (vl-catch-all-error-p value)) value))

(defun qt-endpoint-p (record point tolerance / ename start end)
  (setq ename (qt-rec-ename record))
  (if (qt-closed-p ename)
    nil
    (progn
      (setq start (vlax-curve-getStartPoint ename)
            end (vlax-curve-getEndPoint ename))
      (or (<= (distance point start) tolerance)
          (<= (distance point end) tolerance)))))

(defun qt-vscale (factor vector)
  (mapcar '(lambda (x) (* factor x)) vector))

(defun qt-vdot (a b)
  (apply '+ (mapcar '* a b)))

(defun qt-vlength (vector)
  (sqrt (qt-vdot vector vector)))

(defun qt-vunit (vector / size)
  (setq size (qt-vlength vector))
  (if (> size 1e-12)
    (qt-vscale (/ 1.0 size) vector)
    '(1.0 0.0 0.0)))

(defun qt-direction-at-node (record point tolerance / ename closest param deriv end)
  (setq ename (qt-rec-ename record)
        closest (vlax-curve-getClosestPointTo ename point)
        param (vlax-curve-getParamAtPoint ename closest)
        deriv (vlax-curve-getFirstDeriv ename param))
  ;; Em uma extremidade final, inverte a derivada para ela apontar para fora do no.
  (if (and (not (qt-closed-p ename))
           (setq end (vlax-curve-getEndPoint ename))
           (<= (distance point end) tolerance))
    (setq deriv (qt-vscale -1.0 deriv)))
  (qt-vunit deriv))

(defun qt-better-main-record (id-a id-b records)
  ;; Nos cruzamentos de duas curvas continuas, a maior bitola prevalece;
  ;; em empate, prevalece a entidade de maior comprimento.
  (cond
    ((> (qt-rec-diameter (nth id-a records))
        (qt-rec-diameter (nth id-b records))) id-a)
    ((< (qt-rec-diameter (nth id-a records))
        (qt-rec-diameter (nth id-b records))) id-b)
    ((>= (qt-rec-length (nth id-a records))
         (qt-rec-length (nth id-b records))) id-a)
    (T id-b)))

(defun qt-best-branch (ids main-id records point tolerance / main-dir id score best best-score)
  (setq main-dir
    (qt-direction-at-node (nth main-id records) point tolerance))
  (foreach id ids
    (if (/= id main-id)
      (progn
        ;; Menor paralelismo = melhor candidato a ramal.
        (setq score
          (abs
            (qt-vdot
              main-dir
              (qt-direction-at-node (nth id records) point tolerance))))
        (if (or (null best-score) (< score best-score))
          (setq best id best-score score)))))
  best)

(defun qt-best-collinear-pair (ids records point tolerance / a rest b score best best-score)
  (foreach a ids
    (setq rest (cdr (member a ids)))
    (foreach b rest
      ;; Direcoes opostas produzem produto escalar proximo de -1.
      (setq score
        (-
          (qt-vdot
            (qt-direction-at-node (nth a records) point tolerance)
            (qt-direction-at-node (nth b records) point tolerance))))
      (if (or (null best-score) (> score best-score))
        (setq best (list a b) best-score score))))
  best)

(defun qt-remove-ids (ids remove-ids)
  (vl-remove-if '(lambda (id) (member id remove-ids)) ids))

(defun qt-node-triple (node records tolerance / point ids interiors id main branch pair remaining)
  (setq point (car node)
        ids (cadr node))
  (foreach id ids
    (if (not (qt-endpoint-p (nth id records) point tolerance))
      (setq interiors (cons id interiors))))
  (cond
    ;; Uma curva atravessa o no: ela e o trecho principal.
    ((= (length interiors) 1)
      (setq main (car interiors)
            branch (qt-best-branch ids main records point tolerance))
      (if branch
        (list
          (qt-rec-diameter (nth main records))
          (qt-rec-diameter (nth branch records))
          (qt-rec-diameter (nth main records)))))
    ;; Cruzamento entre duas ou mais curvas continuas: conta como um te.
    ((> (length interiors) 1)
      (setq main (car interiors))
      (foreach id (cdr interiors)
        (setq main (qt-better-main-record main id records)))
      (setq branch (qt-best-branch ids main records point tolerance))
      (if branch
        (list
          (qt-rec-diameter (nth main records))
          (qt-rec-diameter (nth branch records))
          (qt-rec-diameter (nth main records)))))
    ;; Tres ou mais segmentos terminando no mesmo ponto: busca o par colinear.
    ((>= (length ids) 3)
      (setq pair (qt-best-collinear-pair ids records point tolerance)
            remaining (qt-remove-ids ids pair)
            branch (car remaining))
      (if (and pair branch)
        (list
          (qt-rec-diameter (nth (car pair) records))
          (qt-rec-diameter (nth branch records))
          (qt-rec-diameter (nth (cadr pair) records)))))
    ;; Duas entidades unidas apenas pelas pontas sao uma emenda/cotovelo, nao um te.
    (T nil)))

(defun qt-nearly-equal (a b)
  (< (abs (- a b)) 1e-8))

;; Registro de conexao:
;; (chave tipo diametro-principal-1 diametro-ramal diametro-principal-2 quantidade)
(defun qt-connection-add (connections triple / d1 branch d3 high low type key result item found)
  (setq d1 (car triple)
        branch (cadr triple)
        d3 (caddr triple)
        high (max d1 d3)
        low (min d1 d3)
        type
          (cond
            ((and (qt-nearly-equal high branch)
                  (qt-nearly-equal branch low)) "Te simples")
            ((qt-nearly-equal high low) "Te de reducao")
            (T "Te de reducao assimetrico"))
        key
          (strcat
            (rtos high 2 6) "|"
            (rtos branch 2 6) "|"
            (rtos low 2 6)))
  (foreach item connections
    (if (= key (car item))
      (progn
        (setq result
          (cons
            (list key type high branch low (1+ (nth 5 item)))
            result)
              found T))
      (setq result (cons item result))))
  (if (not found)
    (setq result (cons (list key type high branch low 1) result)))
  (reverse result))

(defun qt-connection-rank (type)
  (cond
    ((= type "Te simples") 1)
    ((= type "Te de reducao") 2)
    (T 3)))

(defun qt-connection-less-p (a b / rank-a rank-b)
  (setq rank-a (qt-connection-rank (nth 1 a))
        rank-b (qt-connection-rank (nth 1 b)))
  (if (= rank-a rank-b)
    (> (nth 2 a) (nth 2 b))
    (< rank-a rank-b)))

(defun qt-build-connections (nodes records tolerance / node triple result)
  (foreach node nodes
    (if (setq triple (qt-node-triple node records tolerance))
      (setq result (qt-connection-add result triple))))
  (vl-sort result 'qt-connection-less-p))

;; Registro de tubo: (diametro comprimento-em-unidades-do-desenho)
(defun qt-pipe-add (pipes diameter length / result item found)
  (foreach item pipes
    (if (qt-nearly-equal diameter (car item))
      (progn
        (setq result (cons (list diameter (+ length (cadr item))) result)
              found T))
      (setq result (cons item result))))
  (if (not found)
    (setq result (cons (list diameter length) result)))
  (reverse result))

(defun qt-build-pipes (records / record result)
  (foreach record records
    (setq result
      (qt-pipe-add result (qt-rec-diameter record) (qt-rec-length record))))
  (vl-sort result '(lambda (a b) (< (car a) (car b)))))

(defun qt-current-space (/ document)
  (setq document (vla-get-ActiveDocument (vlax-get-acad-object)))
  (if (= (getvar "CVPORT") 1)
    (vla-get-PaperSpace document)
    (vla-get-ModelSpace document)))

(defun qt-safe-call (function arguments)
  (vl-catch-all-apply function arguments))

(defun qt-table-set-text (table row column text)
  (vla-SetText table row column text))

(defun qt-table-merge-title (table row text)
  (vla-MergeCells table row row 0 2)
  (qt-table-set-text table row 0 text))

(defun qt-create-table (point pipes connections factor text-height / pipe-count connection-count rows row table pipe item total total-connections)
  (setq pipe-count (length pipes)
        connection-count (max 1 (length connections))
        rows (+ 8 pipe-count connection-count)
        table
          (vla-AddTable
            (qt-current-space)
            (vlax-3d-point (trans point 1 0))
            rows
            3
            (* text-height 2.0)
            (* text-height 20.0)))
  (vla-put-RegenerateTableSuppressed table :vlax-true)
  (qt-safe-call 'vla-SetColumnWidth (list table 0 (* text-height 26.0)))
  (qt-safe-call 'vla-SetColumnWidth (list table 1 (* text-height 24.0)))
  (qt-safe-call 'vla-SetColumnWidth (list table 2 (* text-height 18.0)))
  (qt-safe-call 'vla-SetTextHeight (list table 1 text-height)) ; dados
  (qt-safe-call 'vla-SetTextHeight (list table 2 (* text-height 1.25))) ; titulo
  (qt-safe-call 'vla-SetTextHeight (list table 4 text-height)) ; cabecalho
  (qt-safe-call 'vla-SetAlignment (list table 1 5))
  (qt-safe-call 'vla-SetAlignment (list table 2 5))
  (qt-safe-call 'vla-SetAlignment (list table 4 5))

  (setq row 0)
  (qt-table-merge-title table row "QUANTITATIVO DE TUBOS E CONEXOES EM T")
  (setq row (1+ row))
  (qt-table-merge-title table row "TUBOS")
  (setq row (1+ row))
  (qt-table-set-text table row 0 "Item")
  (qt-table-set-text table row 1 "Diametro")
  (qt-table-set-text table row 2 "Comprimento (m)")
  (setq row (1+ row)
        total 0.0)
  (foreach pipe pipes
    (qt-table-set-text table row 0 *qt-item-name*)
    (qt-table-set-text table row 1 (strcat (qt-format-diameter (car pipe)) " mm"))
    (qt-table-set-text table row 2 (qt-format-number-br (* (cadr pipe) factor) 2))
    (setq total (+ total (* (cadr pipe) factor))
          row (1+ row)))
  (qt-table-set-text table row 0 "Total")
  (vla-MergeCells table row row 0 1)
  (qt-table-set-text table row 2 (qt-format-number-br total 2))

  (setq row (1+ row))
  (qt-table-merge-title table row "CONEXOES EM T")
  (setq row (1+ row))
  (qt-table-merge-title
    table row
    "Notacao: trecho principal x ramal central x trecho principal.")
  (setq row (1+ row))
  (qt-table-set-text table row 0 "Tipo")
  (qt-table-set-text table row 1 "Especificacao (mm)")
  (qt-table-set-text table row 2 "Quantidade")
  (setq row (1+ row)
        total-connections 0)
  (if connections
    (foreach item connections
      (qt-table-set-text table row 0 (nth 1 item))
      (qt-table-set-text table row 1
        (strcat
          (qt-format-diameter (nth 2 item)) " x "
          (qt-format-diameter (nth 3 item)) " x "
          (qt-format-diameter (nth 4 item))))
      (qt-table-set-text table row 2 (itoa (nth 5 item)))
      (setq total-connections (+ total-connections (nth 5 item))
            row (1+ row)))
    (progn
      (vla-MergeCells table row row 0 2)
      (qt-table-set-text table row 0 "Nenhuma conexao em T detectada")
      (setq row (1+ row))))
  (qt-table-set-text table row 0 "Total de conexoes")
  (vla-MergeCells table row row 0 1)
  (qt-table-set-text table row 2 (itoa total-connections))

  (vla-put-RegenerateTableSuppressed table :vlax-false)
  (vla-Update table)
  table)

(defun qt-get-positive-real (message current / value)
  (setq value
    (getreal
      (strcat message " <" (qt-trim-zeroes (rtos current 2 6)) ">: ")))
  (if (and value (> value 0.0)) value current))

(defun c:QTUBOSMAP (/ selection ename key current answer value)
  (vl-load-com)
  (prompt "\nSelecione uma linha de exemplo da cor a configurar: ")
  (if (setq selection (ssget "_:S" '((0 . "LINE,LWPOLYLINE,POLYLINE,ARC,SPLINE,ELLIPSE"))))
    (progn
      (setq ename (ssname selection 0)
            key (qt-effective-color-key ename)
            current (qt-map-get key))
      (setq answer
        (getstring T
          (strcat
            "\nDiametro em mm para " (qt-color-description key)
            (if current
              (strcat " <" (qt-format-diameter current) ">")
              "")
            " (0 = ignorar): ")))
      (setq answer (vl-string-translate "," "." answer))
      (cond
        ((and (= answer "") current) (setq value current))
        ((and (/= answer "") (setq value (distof answer 2)) (>= value 0.0)) nil)
        (T (setq value nil)))
      (if value
        (progn
          (qt-map-set key value)
          (prompt
            (strcat
              "\nMapeamento salvo nesta sessao: "
              (qt-color-description key) " = "
              (qt-format-diameter value) " mm.")))
        (prompt "\nValor invalido; nenhuma alteracao foi feita.")))
    (prompt "\nNenhuma entidade valida selecionada."))
  (princ))

(defun c:QTUBOSLIMPAR ()
  (setq *qt-color-map* nil)
  (prompt
    "\nMapeamentos manuais apagados. A legenda automatica continua ativa.")
  (princ))

(defun c:QTCOR (/ selection ename key diameter)
  (vl-load-com)
  (prompt "\nSelecione uma linha para diagnosticar sua cor: ")
  (if (setq selection
        (ssget "_:S" '((0 . "LINE,LWPOLYLINE,POLYLINE,ARC,SPLINE,ELLIPSE"))))
    (progn
      (setq ename (ssname selection 0)
            key (qt-effective-color-key ename)
            diameter (qt-map-get key))
      (prompt
        (strcat
          "\nCor efetiva: " key
          (if diameter
            (strcat " -> reconhecida como " (qt-format-diameter diameter) " mm.")
            " -> ainda nao reconhecida."))))
    (prompt "\nNenhuma entidade valida selecionada."))
  (princ))

(defun c:QTUBOS (/ *error* old-cmdecho selection keys i records pipes nodes connections point table)
  (vl-load-com)
  (setq old-cmdecho (getvar "CMDECHO"))
  (defun *error* (message)
    (setvar "CMDECHO" old-cmdecho)
    (if (and message
             (/= message "Function cancelled")
             (/= message "quit / exit abort"))
      (prompt (strcat "\nErro no QTUBOS: " message)))
    (princ))
  (setvar "CMDECHO" 0)
  (prompt
    "\nSelecione as linhas/polilinhas dos tubos (comprimentos e cruzamentos): ")
  (if (setq selection
        (ssget '((0 . "LINE,LWPOLYLINE,POLYLINE,ARC,SPLINE,ELLIPSE"))))
    (progn
      (setq i 0)
      (repeat (sslength selection)
        (setq keys
          (cons (qt-effective-color-key (ssname selection i)) keys)
              i (1+ i)))
      (setq keys (qt-unique keys))
      (qt-ensure-mappings keys)
      (setq records (qt-make-records selection))
      (if records
        (progn
          (setq *qt-length-to-meter*
            (qt-get-positive-real
              "\nFator da unidade do desenho para metros"
              *qt-length-to-meter*))
          (setq *qt-text-height*
            (qt-get-positive-real
              "\nAltura do texto da tabela"
              *qt-text-height*))
          (prompt "\nAnalisando intersecoes...")
          (setq pipes (qt-build-pipes records)
                nodes (qt-find-nodes records *qt-node-tolerance*)
                connections
                  (qt-build-connections nodes records *qt-node-tolerance*))
          (if (setq point (getpoint "\nPonto de insercao da tabela: "))
            (progn
              (setq table
                (qt-create-table
                  point pipes connections
                  *qt-length-to-meter* *qt-text-height*))
              (prompt
                (strcat
                  "\nTabela criada: " (itoa (length pipes))
                  " bitola(s) e " (itoa (length connections))
                  " especificacao(oes) de te.")))))
        (prompt "\nTodas as cores foram marcadas para ignorar.")))
    (prompt "\nNada foi selecionado."))
  (setvar "CMDECHO" old-cmdecho)
  (princ))

(prompt
  (strcat
    "\nQuantitativoTubos v" *qt-version*
    " carregado. Use QTUBOS (QTCOR diagnostica uma cor)."))
(princ)
