(vl-load-com)

;;; Variaveis globais (persistem na sessao)
(if (not *pipe-curva*) (setq *pipe-curva* 0.1))
(if (not *pipe-tamanho*) (setq *pipe-tamanho* 0.1))
;; *pipe-linha-central* removida, agora e sempre LIGADA

;;; ==========================================================================
;;; FUNCOES AUXILIARES
;;; ==========================================================================

;;; Cria um layer se ele nao existir
(defun pipe:ensure-layer (layName)
  (if (not (tblsearch "LAYER" layName))
    (command "_.LAYER" "_M" layName "")
  )
)

;;; Limpa linhas temporarias
(defun pipe:delete_temp (temp_lines) 
  (if temp_lines 
    (foreach e temp_lines (entdel e))
  )
)

;;; Adiciona XData (BB_PIPE) a uma entidade de forma segura via ActiveX
(defun pipe:add-xdata (ent / obj dataType dataValue)
  (if (and ent (entget ent))
    (progn
      (if (not (tblsearch "APPID" "BB_PIPE"))
        (regapp "BB_PIPE")
      )
      (setq obj (vlax-ename->vla-object ent))
      (setq dataType (vlax-make-safearray vlax-vbInteger '(0 . 1)))
      (vlax-safearray-put-element dataType 0 1001)
      (vlax-safearray-put-element dataType 1 1000)
      
      (setq dataValue (vlax-make-safearray vlax-vbVariant '(0 . 1)))
      (vlax-safearray-put-element dataValue 0 "BB_PIPE")
      (vlax-safearray-put-element dataValue 1 "PIPE_OBJ")
      
      (vl-catch-all-apply 'vla-SetXData (list obj dataType dataValue))
    )
  )
)

;;; Retorna o vetor unitario entre dois pontos
(defun pipe:get-unit-vector (p1 p2 / dist)
  (setq dist (distance p1 p2))
  (if (> dist 1e-8)
    (list (/ (- (car p2) (car p1)) dist)
          (/ (- (cadr p2) (cadr p1)) dist)
          0.0
    )
    '(0.0 0.0 0.0)
  )
)

;;; Desenha uma linha central continua DashDot
(defun pipe:draw-continuous-centerline (pts / 2d_pts ent-list last-ent)
  (if (and pts (> (length pts) 1))
    (progn
      (setq 2d_pts (mapcar '(lambda (p) (list (car p) (cadr p))) pts))
      (setq ent-list (append 
                       (list 
                         '(0 . "LWPOLYLINE")
                         '(100 . "AcDbEntity")
                         '(100 . "AcDbPolyline")
                         (cons 90 (length pts))
                         '(70 . 0)
                       )
                       (apply 'append 
                              (mapcar '(lambda (p) (list (cons 10 p))) 2d_pts)
                       )
                     )
      )
      (if (entmake ent-list)
        (progn
          (setq last-ent (entlast))
          (pipe:ensure-layer "PP_LINHA CENTRAL")
          (vla-put-Layer (vlax-ename->vla-object last-ent) "PP_LINHA CENTRAL")
          (if (not (tblsearch "LTYPE" "DASHDOT")) 
            (vl-cmdf "_.LINETYPE" "_L" "DASHDOT" "acadiso.lin" "")
          )
          (vla-put-Linetype (vlax-ename->vla-object last-ent) "DASHDOT")
          (vla-put-LinetypeScale (vlax-ename->vla-object last-ent) 0.5)
          (pipe:add-xdata last-ent)
          last-ent
        )
      )
    )
  )
)

;;; Cria Hatch Solido para um boundary
(defun pipe:create-hatch (boundary-ent / model-space hatch loop) 
  (if (and boundary-ent (entget boundary-ent)) 
    (progn 
      (setq model-space (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object))))

      (if 
        (not 
          (vl-catch-all-error-p 
            (setq hatch (vl-catch-all-apply 'vla-AddHatch 
                                            (list model-space 
                                                  acHatchPatternTypePreDefined
                                                  "SOLID"
                                                  :vlax-true
                                            )
                        )
            )
          )
        )
        (progn 
          (setq loop (vlax-make-safearray vlax-vbObject '(0 . 0)))
          (vlax-safearray-put-element loop 0 (vlax-ename->vla-object boundary-ent))

          (vla-AppendOuterLoop hatch loop)
          (vla-Evaluate hatch)

          ;; Define cor para ByLayer (256) e transparencia para 80%
          (vla-put-Color hatch 256)
          (vl-catch-all-apply 'vla-put-EntityTransparency (list hatch 80))

          (pipe:add-xdata (entlast))
          (entlast) ;; Retorna a entidade hatch criada
        )
        (princ "\nErro ao criar objeto hatch.")
      )
    )
  )
)

;;; Ajusta Draw Order: Wipeout no fundo, Hatch acima, Linhas no topo
(defun pipe:set-draw-order (ent-list hatch-ent wipeout-ent / ss ss-all old-cmdecho) 
  (setq old-cmdecho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)

  ;; 1. Wipeout no fundo absoluto
  (if (and wipeout-ent (entget wipeout-ent)) 
    (progn 
      (setq ss (ssadd))
      (ssadd wipeout-ent ss)
      (vl-cmdf "_.DRAWORDER" ss "" "_B")
      (while (> (getvar "CMDACTIVE") 0) (command ""))
    )
  )

  ;; 2. Linhas acima da hatch
  (if (and ent-list hatch-ent (entget hatch-ent)) 
    (progn 
      (setq ss (ssadd))
      (foreach e ent-list (if (and e (entget e)) (ssadd e ss)))
      (if (> (sslength ss) 0) 
        (progn 
          (vl-cmdf "_.DRAWORDER" ss "" "_A" hatch-ent)
          (while (> (getvar "CMDACTIVE") 0) (command ""))
        )
      )
    )
  )

  ;; 3. Traz todo o conjunto para o front absoluto
  (setq ss-all (ssadd))
  (if (and wipeout-ent (entget wipeout-ent)) (ssadd wipeout-ent ss-all))
  (if (and hatch-ent (entget hatch-ent)) (ssadd hatch-ent ss-all))
  (foreach e ent-list 
    (if (and e (entget e)) (ssadd e ss-all))
  )
  (if (> (sslength ss-all) 0) 
    (progn 
      (vl-cmdf "_.DRAWORDER" ss-all "" "_F")
      (while (> (getvar "CMDACTIVE") 0) (command ""))
    )
  )

  ;; 4. Traz especificamente o primeiro item da lista (geralmente a central) para o topo absoluto
  (if (and ent-list (car ent-list) (entget (car ent-list)))
    (progn
      (setq ss (ssadd))
      (ssadd (car ent-list) ss)
      (vl-cmdf "_.DRAWORDER" ss "" "_F")
      (while (> (getvar "CMDACTIVE") 0) (command ""))
    )
  )

  (setvar "CMDECHO" old-cmdecho)
)

;;; ==========================================================================
;;; TESSELLIZACAO DE ARCOS
;;; ==========================================================================

;;; Converte arco (p1, p2, bulge) em lista de pontos retos
(defun pipe:tessellate-arc (p1 p2 bulge num-segs / chord-len sagitta radius theta 
                            center start-ang delta-ang pts i ang pt
                           ) 
  (if (or (not bulge) (= bulge 0.0) (< (abs bulge) 0.0001)) 
    (list p1) ;; Sem arco, retorna apenas p1
    (progn 
      (setq chord-len (distance p1 p2))
      (setq sagitta (* (abs bulge) (/ chord-len 2.0)))
      (setq radius (/ 
                     (+ (* (/ chord-len 2.0) (/ chord-len 2.0)) 
                        (* sagitta sagitta)
                     )
                     (* 2.0 sagitta)
                   )
      )
      (setq theta (* 4.0 (atan (abs bulge))))

      ;; Centro do arco
      (setq center (polar 
                     (polar p1 (angle p1 p2) (/ chord-len 2.0))
                     (+ (angle p1 p2) (if (> bulge 0) (/ pi 2.0) (/ pi -2.0)))
                     (- radius sagitta)
                   )
      )

      ;; Angulos
      (setq start-ang (angle center p1))
      (setq delta-ang (/ theta num-segs))
      (if (< bulge 0) (setq delta-ang (- delta-ang)))

      ;; Gera pontos intermediarios
      (setq pts (list p1))
      (setq i 1)
      (while (< i num-segs) 
        (setq ang (+ start-ang (* i delta-ang)))
        (setq pt (polar center ang radius))
        (setq pts (append pts (list (list (car pt) (cadr pt)))))
        (setq i (1+ i))
      )
      pts
    )
  )
)

;;; Extrai pontos da polilinha tessellizando arcos
(defun pipe:get-poly-points-tessellated (ent num-segs-per-arc / data pts i p1 p2 
                                         bulge seg-pts
                                        ) 
  (setq data (pipe:get-poly-data ent))
  (setq pts nil)
  (if (and data (> (length data) 0)) 
    (progn 
      (setq i 0)
      (while (< i (length data)) 
        (setq p1 (car (nth i data)))
        (setq bulge (cadr (nth i data)))

        (if (< i (1- (length data))) 
          (setq p2 (car (nth (1+ i) data)))
          ;; Se fechada, conecta ao primeiro
          (if (= (cdr (assoc 70 (entget ent))) 1) 
            (setq p2 (car (nth 0 data)))
            (setq p2 nil)
          )
        )

        (if (and p2 bulge (/= bulge 0.0) (>= (abs bulge) 0.0001)) 
          (progn 
            (setq seg-pts (pipe:tessellate-arc p1 p2 bulge num-segs-per-arc))
            (setq pts (append pts seg-pts))
          )
          (setq pts (append pts (list p1)))
        )
        (setq i (1+ i))
      )
    )
  )
  pts
)

;;; Cria Wipeout a partir do envelope (tessellizando arcos)
(defun pipe:create-wipeout (boundary-ent / pts ss wipeout old-cmdecho) 
  (if (and boundary-ent (entget boundary-ent)) 
    (progn 
      ;; Obtem pontos tessellizados (8 segmentos por arco)
      (setq pts (pipe:get-poly-points-tessellated boundary-ent 8))

      (if (and pts (> (length pts) 2)) 
        (progn 
          (setq old-cmdecho (getvar "CMDECHO"))
          (setvar "CMDECHO" 0)

          ;; Remove pontos duplicados ou extremamente proximos que fazem o Wipeout falhar
          (setq pts 
            (progn
              (setq res (list (car pts)))
              (foreach p (cdr pts)
                (if (> (distance p (car res)) 1e-4)
                  (setq res (cons p res))
                )
              )
              (reverse res)
            )
          )

          ;; Cria o Wipeout ponto a ponto para driblar o erro "The polyline must be closed..."
          ;; que ocorre quando o AutoCAD rejeita polilinhas com pequenas auto-intersecoes.
          (vl-catch-all-apply 
            '(lambda () 
               (command "_.WIPEOUT")
               (foreach p pts 
                 (command p)
               )
               (command "") ;; Enter para fechar e finalizar o poligono do Wipeout
             )
          )

          (setq wipeout (entlast))

          ;; Verifica se realmente criou um wipeout
          (if (and wipeout (= (cdr (assoc 0 (entget wipeout))) "WIPEOUT")) 
            (progn 
              (pipe:add-xdata wipeout)
              (setvar "CMDECHO" old-cmdecho)
              wipeout
            )
            (progn 
              (setvar "CMDECHO" old-cmdecho)
              nil
            )
          )
        )
        nil
      )
    )
    nil
  )
)

;;; Extrai dados da polilinha: lista de ((x y z) bulge)
(defun pipe:get-poly-data (ent / data result pt bulge) 
  (setq result nil
        pt     nil
        bulge  0.0
  )
  (if (and ent (setq data (entget ent))) 
    (progn 
      (foreach item data 
        (cond 
          ((= (car item) 10)
           (if pt (setq result (cons (list pt bulge) result)))
           (setq pt (cdr item))
           (setq bulge 0.0)
          )
          ((= (car item) 42)
           (setq bulge (cdr item))
          )
        )
      )
      (if pt (setq result (cons (list pt bulge) result)))
    )
  )
  (reverse result)
)

;;; Inverte dados da polilinha para fechar o poligono
(defun pipe:reverse-poly-data (data / result len i pt bulge) 
  (setq len (length data))
  (setq result nil
        i      0
  )
  (while (< i len) 
    (setq pt (car (nth (- len 1 i) data)))
    (if (< (- len 2 i) 0) 
      (setq bulge 0.0)
      (setq bulge (- (cadr (nth (- len 2 i) data)))) ;; Inverte sinal do bulge
    )
    (setq result (cons (list pt bulge) result))
    (setq i (1+ i))
  )
  (reverse result)
)

;;; Mapeia Global Width e angulo de deflexao para o nome do bloco do joelho
(defun pipe:get-elbow-block-name (width deflection / w_mm deg type prefix) 
  (setq w_mm (fix (+ 0.5 (* width 1000))))
  (setq deg (chr 176)) ;; Codigo ASCII para o simbolo de grau
  
  ;; Determina se e 45 ou 90 (com tolerancia)
  (setq type (cond 
               ((< (abs (- deflection (/ pi 2.0))) 0.15) "90") ;; ~90 graus
               ((< (abs (- deflection (/ pi 4.0))) 0.15) "45") ;; ~45 graus
               (T nil)
             )
  )
  
  (if type
    (progn
      (setq prefix (if (= w_mm 40) "" "#")) ;; 40 nao tem # no nome
      (strcat prefix "Joelho" type deg "_" (itoa w_mm) "_PVC_F")
    )
    nil
  )
)

;;; Mapeia Global Width e angulo de deflexao para o nome do bloco de Junção/Tê
(defun pipe:get-junction-block-name (w-main w-branch deflection / w_m w_b type prefix)
  (setq w_m (fix (+ 0.5 (* w-main 1000))))
  (setq w_b (fix (+ 0.5 (* w-branch 1000))))
  
  (setq type (cond 
               ((< (abs (- deflection (/ pi 2.0))) 0.35) "Te")
               ((< (abs (- deflection (/ pi 4.0))) 0.35) (strcat "Jun" (chr 231) (chr 227) "o"))
               (T nil)
             )
  )
  
  (if type
    (progn
      (setq prefix (if (= w_m 40) "" "#"))
      (strcat prefix type "_" (itoa w_m) "_" (itoa w_b) "_PVC_F")
    )
    nil
  )
)

;;; Verifica se um ponto pt esta sobre o segmento p1-p2
(defun pipe:point-on-segment-p (pt p1 p2 / d1 d2 d)
  (setq d1 (distance pt p1))
  (setq d2 (distance pt p2))
  (setq d (distance p1 p2))
  (if (and (< (abs (- (+ d1 d2) d)) 1e-4)
           (> d1 1e-4)
           (> d2 1e-4))
    T
    nil
  )
)

;;; Quebra os segmentos que se cruzam, encostam em T, ou cruzam com blocos de corte
(defun pipe:split-segments (segs corte-list / new-segs split-occurred i j seg-a seg-b pt pt-list id-counter cur-id p1 p2 w s1 s2 corte-pts)
  (setq split-occurred T)
  (setq new-segs segs)
  (setq id-counter 10000) ;; para gerar novos ids em splits
  (setq corte-pts (if corte-list (mapcar 'car corte-list) nil))
  (while split-occurred
    (setq split-occurred nil)
    (setq i 0)
    (while (and (< i (length new-segs)) (not split-occurred))
      (setq seg-a (nth i new-segs))
      
      ;; 1. Verifica split com pontos de corte
      (if corte-pts
        (foreach pt corte-pts
          (if (and (not split-occurred) (pipe:point-on-segment-p pt (cadr seg-a) (caddr seg-a)))
            (progn
              (setq split-occurred T)
              (setq new-segs (vl-remove seg-a new-segs))
              (setq cur-id (car seg-a) p1 (cadr seg-a) p2 (caddr seg-a) w (cadddr seg-a) s1 (nth 4 seg-a) s2 (nth 5 seg-a))
              (setq new-segs (append new-segs 
                                     (list (list cur-id p1 pt w s1 0.0)
                                           (list (setq id-counter (1+ id-counter)) pt p2 w 0.0 s2))))
            )
          )
        )
      )

      ;; 2. Verifica split com outros segmentos (apenas se nao dividiu por corte)
      (if (not split-occurred)
        (progn
          (setq j 0)
          (while (and (< j (length new-segs)) (not split-occurred))
            (if (/= i j)
              (progn
                (setq seg-b (nth j new-segs))
                (setq pt-list (list (cadr seg-b) (caddr seg-b)))
                (foreach pt pt-list
                  (if (and (not split-occurred) (pipe:point-on-segment-p pt (cadr seg-a) (caddr seg-a)))
                    (progn
                      (setq split-occurred T)
                      (setq new-segs (vl-remove seg-a new-segs))
                      (setq cur-id (car seg-a) p1 (cadr seg-a) p2 (caddr seg-a) w (cadddr seg-a) s1 (nth 4 seg-a) s2 (nth 5 seg-a))
                      (setq new-segs (append new-segs 
                                             (list (list cur-id p1 pt w s1 0.0)
                                                   (list (setq id-counter (1+ id-counter)) pt p2 w 0.0 s2))))
                    )
                  )
                )
              )
            )
            (setq j (1+ j))
          )
        )
      )
      (setq i (1+ i))
    )
  )
  new-segs
)

;;; Constroi a lista de nos (Nodes). Cada no eh: (ponto (lista_de_segmentos))
(defun pipe:build-nodes (segs / nodes pt found node)
  (setq nodes nil)
  (foreach seg segs
    (foreach pt (list (cadr seg) (caddr seg))
      (setq found nil)
      (foreach node nodes
        (if (< (distance pt (car node)) 1e-4)
          (setq found node)
        )
      )
      (if found
        ;; se ja tem o segmento registrado no no, ignora
        (if (not (vl-position seg (cadr found)))
          (setq nodes (subst (list (car found) (cons seg (cadr found))) found nodes))
        )
        (setq nodes (cons (list pt (list seg)) nodes))
      )
    )
  )
  nodes
)

;;; Obtem o angulo saindo do no pt ao longo do segmento seg
(defun pipe:get-leaving-vector (seg pt / p1 p2)
  (setq p1 (cadr seg) p2 (caddr seg))
  (if (< (distance p1 pt) 1e-4)
    (angle p1 p2)
    (angle p2 p1)
  )
)

;;; Atualiza o valor de encurtamento de um segmento
(defun pipe:update-shorten (segs seg-id pt amount / new-segs p1 p2)
  (setq new-segs nil)
  (foreach s segs
    (if (= (car s) seg-id)
      (progn
        (setq p1 (cadr s) p2 (caddr s))
        (if (< (distance p1 pt) 1e-4)
          (setq new-segs (cons (list (car s) p1 p2 (cadddr s) (max (nth 4 s) amount) (nth 5 s)) new-segs))
          (setq new-segs (cons (list (car s) p1 p2 (cadddr s) (nth 4 s) (max (nth 5 s) amount)) new-segs))
        )
      )
      (setq new-segs (cons s new-segs))
    )
  )
  (reverse new-segs)
)

;;; Rastreia todos os blocos de corte e retorna lista: ((pt . "Sobe") (pt . "Desce") ...)
(defun pipe:get-corte-blocks (/ ss i ent obj eff-name pt props val lst)
  (setq lst nil)
  (if (setq ss (ssget "X" '((0 . "INSERT"))))
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (setq obj (vlax-ename->vla-object ent))
        (setq eff-name (if (vlax-property-available-p obj 'EffectiveName)
                         (vla-get-EffectiveName obj)
                         (vla-get-Name obj)
                       )
        )
        (if (wcmatch (strcase eff-name) "*CORTE_PVC*")
          (progn
            (setq pt (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint obj))))
            (setq pt (list (car pt) (cadr pt) 0.0))
            (setq val nil)
            (if (= (vla-get-IsDynamicBlock obj) :vlax-true)
              (progn
                (setq props (vlax-invoke obj 'GetDynamicBlockProperties))
                (foreach p props
                  (if (= (strcase (vla-get-PropertyName p)) "SETA")
                    (setq val (vlax-get p 'Value))
                  )
                )
              )
            )
            (if val
              (setq lst (cons (list pt val ent) lst))
            )
          )
        )
        (setq i (1+ i))
      )
    )
  )
  lst
)

;;; Processa os nos do grafo, identifica conexoes e insere os blocos
(defun pipe:process-nodes (nodes segs corte-list / new-segs pt conn-segs degree a1 a2 a3 d deflection w block-name ins-pt rot d12 d13 d23 min-d main1 main2 branch a_m1 a_m2 a_b w_main w_branch d_branch block-angle best-rot best-scale test-a diff corte-val corte-ent w_mm prefix suffix)
  (setq new-segs segs)
  (foreach node nodes
    (setq pt (car node))
    (setq conn-segs (cadr node))
    (setq degree (length conn-segs))
    
    (cond
      ;; --- PONTA (Grau 1) ---
      ((= degree 1)
       (setq corte-val nil corte-ent nil)
       (foreach c corte-list
         (if (< (distance pt (car c)) 1e-4)
           (setq corte-val (cadr c) corte-ent (caddr c))
         )
       )
       (if (and corte-val 
                (or (= (strcase corte-val) "SOBE") (= (strcase corte-val) "CIMA")
                    (= (strcase corte-val) "DESCE") (= (strcase corte-val) "BAIXO")))
         (progn
           (setq a1 (pipe:get-leaving-vector (nth 0 conn-segs) pt))
           (setq w (cadddr (nth 0 conn-segs)))
           (setq w_mm (fix (+ 0.5 (* w 1000))))
           (setq prefix (if (= w_mm 40) "" "#"))
           (setq suffix (if (or (= (strcase corte-val) "SOBE") (= (strcase corte-val) "CIMA")) "T" "B"))
           (setq block-name (strcat prefix "Joelho90" (chr 176) "_" (itoa w_mm) "_PVC_" suffix))
           
           (if (tblsearch "BLOCK" block-name)
             (progn
               (setq ins-pt (list (car pt) (cadr pt) 0.0))
               (if (entmake (list '(0 . "INSERT") (cons 2 block-name) (cons 10 ins-pt) '(41 . 1.0) '(42 . 1.0) '(43 . 1.0) (cons 50 a1)))
                 (progn
                   (entupd (entlast))
                   (pipe:add-xdata (entlast))
                   (setq *pipe-new-blocks* (cons (entlast) *pipe-new-blocks*))
                   (if (or (= (strcase corte-val) "SOBE") (= (strcase corte-val) "CIMA"))
                     (setq *pipe-corte-sobe-blocks* (cons corte-ent *pipe-corte-sobe-blocks*))
                   )
                 )
               )
               ;; Encurta o tubo
               (setq new-segs (pipe:update-shorten new-segs (car (nth 0 conn-segs)) pt (/ w 2.0)))
             )
             (princ (strcat "\nBloco vertical nao encontrado: " block-name))
           )
         )
       )
      )
      
      ;; --- QUINA (Joelho) ou TE DE CORTE ---
      ((= degree 2)
       (setq corte-val nil corte-ent nil)
       (foreach c corte-list
         (if (< (distance pt (car c)) 1e-4)
           (setq corte-val (cadr c) corte-ent (caddr c))
         )
       )

       (setq a1 (pipe:get-leaving-vector (nth 0 conn-segs) pt))
       (setq a2 (pipe:get-leaving-vector (nth 1 conn-segs) pt))
       (setq d (- a2 a1))
       (while (> d pi) (setq d (- d (* 2 pi))))
       (while (<= d (- pi)) (setq d (+ d (* 2 pi))))
       (setq deflection (abs (- (abs d) pi)))
       (setq w (max (cadddr (nth 0 conn-segs)) (cadddr (nth 1 conn-segs))))

       (if (and corte-val 
                (or (= (strcase corte-val) "SOBE") (= (strcase corte-val) "CIMA")
                    (= (strcase corte-val) "DESCE") (= (strcase corte-val) "BAIXO")))
         (progn
           ;; Eh um Te vertical no meio da linha
           (setq w_mm (fix (+ 0.5 (* w 1000))))
           (setq prefix (if (= w_mm 40) "" "#"))
           (setq suffix (if (or (= (strcase corte-val) "SOBE") (= (strcase corte-val) "CIMA")) "T" "B"))
           (setq block-name (strcat prefix "Te_" (itoa w_mm) "_" (itoa w_mm) "_PVC_" suffix))
           
           (if (tblsearch "BLOCK" block-name)
             (progn
               (setq ins-pt (list (car pt) (cadr pt) 0.0))
               (setq rot a1)
               (if (entmake (list '(0 . "INSERT") (cons 2 block-name) (cons 10 ins-pt) '(41 . 1.0) '(42 . 1.0) '(43 . 1.0) (cons 50 rot)))
                 (progn
                   (entupd (entlast))
                   (pipe:add-xdata (entlast))
                   (setq *pipe-new-blocks* (cons (entlast) *pipe-new-blocks*))
                   (if (or (= (strcase corte-val) "SOBE") (= (strcase corte-val) "CIMA"))
                     (setq *pipe-corte-sobe-blocks* (cons corte-ent *pipe-corte-sobe-blocks*))
                   )
                 )
               )
               (setq new-segs (pipe:update-shorten new-segs (car (nth 0 conn-segs)) pt (/ w 2.0)))
               (setq new-segs (pipe:update-shorten new-segs (car (nth 1 conn-segs)) pt (/ w 2.0)))
             )
             (princ (strcat "\nBloco Te vertical nao encontrado: " block-name))
           )
         )
         (progn
           ;; Comportamento normal de Joelho se nao houver corte
           (if (> deflection 0.1) ; Eh uma curva significativa
             (progn
               (setq block-name (pipe:get-elbow-block-name w deflection))
               (if (and block-name (tblsearch "BLOCK" block-name))
                 (progn
                   (setq ins-pt (list (car pt) (cadr pt) 0.0))
                   (if (> d 0) (setq rot a1) (setq rot a2))
                   (if (entmake (list '(0 . "INSERT") (cons 2 block-name) (cons 10 ins-pt) '(41 . 1.0) '(42 . 1.0) '(43 . 1.0) (cons 50 rot)))
                     (progn
                       (entupd (entlast))
                       (pipe:add-xdata (entlast))
                       (setq *pipe-new-blocks* (cons (entlast) *pipe-new-blocks*))
                     )
                   )
                   ;; Marca os dois segmentos para encurtar
                   (setq new-segs (pipe:update-shorten new-segs (car (nth 0 conn-segs)) pt (/ w 2.0)))
                   (setq new-segs (pipe:update-shorten new-segs (car (nth 1 conn-segs)) pt (/ w 2.0)))
                 )
                 (princ (strcat "\nBloco nao encontrado: " (if block-name block-name "NIL")))
               )
             )
             (progn
               ;; --- REDUCAO ---
               (setq w1 (cadddr (nth 0 conn-segs)))
               (setq w2 (cadddr (nth 1 conn-segs)))
               (if (> (abs (- w1 w2)) 1e-4) ; Verifica diferenca de largura
                 (progn
                   ;; Identifica tubo maior e menor
                   (if (> w1 w2)
                     (setq w_large w1 w_small w2 a_large a1 a_small a2 seg_large (nth 0 conn-segs) seg_small (nth 1 conn-segs))
                     (setq w_large w2 w_small w1 a_large a2 a_small a1 seg_large (nth 1 conn-segs) seg_small (nth 0 conn-segs))
                   )
                   
                   (setq w_large_mm (fix (+ 0.5 (* w_large 1000))))
                   (setq w_small_mm (fix (+ 0.5 (* w_small 1000))))
                   (setq prefix (if (= w_large_mm 40) "" "#"))
                   
                   ;; Formata o nome do bloco usando ASCII para contornar problemas de acentuacao ("Redução")
                   (setq block-name (strcat prefix "Redu" (chr 231) (chr 227) "o_" (itoa w_large_mm) "_" (itoa w_small_mm) "_PVC_F"))
                   
                   (if (tblsearch "BLOCK" block-name)
                     (progn
                       (setq ins-pt (list (car pt) (cadr pt) 0.0))
                       
                       ;; Assumimos que o bloco 0 graus aponta para o tubo MENOR. 
                       ;; (Se ele apontar para o MAIOR, mude rot para a_large).
                       (setq rot a_small)
                       
                       (if (entmake (list '(0 . "INSERT") (cons 2 block-name) (cons 10 ins-pt) '(41 . 1.0) '(42 . 1.0) '(43 . 1.0) (cons 50 rot)))
                         (progn
                           (entupd (entlast))
                           (pipe:add-xdata (entlast))
                           (setq *pipe-new-blocks* (cons (entlast) *pipe-new-blocks*))
                         )
                       )
                       ;; Encurta os tubos
                       (setq new-segs (pipe:update-shorten new-segs (car seg_large) pt (/ w_large 2.0)))
                       (setq new-segs (pipe:update-shorten new-segs (car seg_small) pt (/ w_small 2.0)))
                     )
                     (princ (strcat "\nBloco de reducao nao encontrado: " block-name))
                   )
                 )
               )
             )
           )
         )
       )
      )
      
      ;; --- JUNCAO / TE ---
      ((= degree 3)
       (setq a1 (pipe:get-leaving-vector (nth 0 conn-segs) pt))
       (setq a2 (pipe:get-leaving-vector (nth 1 conn-segs) pt))
       (setq a3 (pipe:get-leaving-vector (nth 2 conn-segs) pt))
       
       (setq d12 (abs (- (abs (- a1 a2)) pi)))
       (setq d13 (abs (- (abs (- a1 a3)) pi)))
       (setq d23 (abs (- (abs (- a2 a3)) pi)))
       (setq min-d (min d12 d13 d23))
       
       (if (< min-d 0.35) ; Encontrou o trecho principal (reto) - maior tolerancia
         (progn
           (cond
             ((= min-d d12) (setq main1 (nth 0 conn-segs) main2 (nth 1 conn-segs) branch (nth 2 conn-segs)))
             ((= min-d d13) (setq main1 (nth 0 conn-segs) main2 (nth 2 conn-segs) branch (nth 1 conn-segs)))
             ((= min-d d23) (setq main1 (nth 1 conn-segs) main2 (nth 2 conn-segs) branch (nth 0 conn-segs)))
           )
           (setq a_m1 (pipe:get-leaving-vector main1 pt))
           (setq a_m2 (pipe:get-leaving-vector main2 pt))
           (setq a_b (pipe:get-leaving-vector branch pt))
           
           (setq w_main (max (cadddr main1) (cadddr main2)))
           (setq w_branch (cadddr branch))
           
           ;; Calcula a deflexao real do ramal
           (setq d_branch (- a_b a_m1))
           (while (> d_branch pi) (setq d_branch (- d_branch (* 2 pi))))
           (while (<= d_branch (- pi)) (setq d_branch (+ d_branch (* 2 pi))))
           (setq deflection (abs d_branch))
           (if (> deflection (/ pi 2)) (setq deflection (- pi deflection)))
           
           (setq block-name (pipe:get-junction-block-name w_main w_branch deflection))
           (if (and block-name (tblsearch "BLOCK" block-name))
             (progn
               (setq ins-pt (list (car pt) (cadr pt) 0.0))
               (setq block-angle (if (< (abs (- deflection (/ pi 4))) 0.35) (* 3.0 (/ pi 4.0)) (/ pi 2)))
               (setq best-rot nil best-scale nil)
               
               (foreach r (list a_m1 a_m2)
                 (foreach sy (list 1.0 -1.0)
                   (setq test-a (+ r (* sy block-angle)))
                   (while (> test-a pi) (setq test-a (- test-a (* 2 pi))))
                   (while (<= test-a (- pi)) (setq test-a (+ test-a (* 2 pi))))
                   
                   (setq diff (abs (- test-a a_b)))
                   (while (> diff pi) (setq diff (- diff (* 2 pi))))
                   (if (< (abs diff) 0.35) ; Tolerancia maior para encaixar o bloco
                     (setq best-rot r best-scale sy)
                   )
                 )
               )
               
               (if (not best-rot) (setq best-rot a_m1 best-scale 1.0)) ; Forca insercao mesmo se o angulo nao for perfeito
               
               (if (entmake (list '(0 . "INSERT") (cons 2 block-name) (cons 10 ins-pt) '(41 . 1.0) (cons 42 best-scale) '(43 . 1.0) (cons 50 best-rot)))
                 (progn
                   (entupd (entlast))
                   (pipe:add-xdata (entlast))
                   (setq *pipe-new-blocks* (cons (entlast) *pipe-new-blocks*))
                 )
               )
               
               ;; Encurta os tubos
               (setq new-segs (pipe:update-shorten new-segs (car main1) pt (/ w_branch 2.0)))
               (setq new-segs (pipe:update-shorten new-segs (car main2) pt (/ w_branch 2.0)))
               (setq new-segs (pipe:update-shorten new-segs (car branch) pt (/ w_main 2.0)))
             )
             (princ (strcat "\nBloco nao encontrado ou angulo fora do padrao: " (if block-name block-name "NIL")))
           )
         )
       )
      )
    )
  )
  new-segs
)

;;; Gera as geometrias de tubo e linha central
(defun pipe:generate-pipes (segs / p1 p2 w s1 s2 uv pt1 pt2 cl-ent top-ss)
  (setq top-ss (ssadd))
  (foreach seg segs
    (setq p1 (cadr seg) p2 (caddr seg) w (cadddr seg) s1 (nth 4 seg) s2 (nth 5 seg))
    (setq uv (pipe:get-unit-vector p1 p2))
    
    ;; Desenha a linha central continua desse trecho ANTES de encurtar
    (setq cl-ent (pipe:draw-continuous-centerline (list p1 p2)))
    (if cl-ent (ssadd cl-ent top-ss))

    ;; Aplica os recuos
    (setq pt1 (list (+ (car p1) (* (car uv) s1)) (+ (cadr p1) (* (cadr uv) s1)) (if (caddr p1) (caddr p1) 0.0)))
    (setq pt2 (list (- (car p2) (* (car uv) s2)) (- (cadr p2) (* (cadr uv) s2)) (if (caddr p2) (caddr p2) 0.0)))
    
    (if (> (distance pt1 pt2) 1e-4)
      (pipe:create_poly (list pt1 pt2) 0.0 w nil nil)
    )
  )
  ;; Traz todas as linhas centrais desse sistema para a frente
  (if (> (sslength top-ss) 0)
    (progn
      (vl-cmdf "_.DRAWORDER" top-ss "" "_F")
      (while (> (getvar "CMDACTIVE") 0) (command ""))
    )
  )
)

;;; Cria a polyline final, aplica fillet e gera envelope fechado com hatch e wipeout
(defun pipe:create_poly (points radius tamanho temp_lines manter-central / 2d_pts ent-list last-ent 
                         old-fillet vla-obj half-width off1 off2 ent1 ent2 data1 data2 
                         all-data result-env res1 res2 hatch-ent wipeout-ent
                        ) 
  (pipe:delete_temp temp_lines)

  (if (> (length points) 1) 
    (progn 
      ;; 1. Cria Polilinha Central
      (setq 2d_pts (mapcar '(lambda (p) (list (car p) (cadr p))) points))

      (setq ent-list (append 
                       (list 
                         '(0 . "LWPOLYLINE")
                         '(100 . "AcDbEntity")
                         '(100 . "AcDbPolyline")
                         (cons 90 (length points))
                         '(70 . 0) ;; 0 = Aberta
                       )
                       (apply 'append 
                              (mapcar '(lambda (p) (list (cons 10 p))) 2d_pts)
                       )
                     )
      )

      (if (entmake ent-list) 
        (progn 
          (setq last-ent (entlast))
          
          ;; Aplica linetype DASHDOT na central e move para a layer PP_LINHA CENTRAL
          (pipe:ensure-layer "PP_LINHA CENTRAL")
          (vla-put-Layer (vlax-ename->vla-object last-ent) "PP_LINHA CENTRAL")

          (if (not (tblsearch "LTYPE" "DASHDOT")) 
            (vl-cmdf "_.LINETYPE" "_L" "DASHDOT" "acadiso.lin" "")
          )
          (vla-put-Linetype (vlax-ename->vla-object last-ent) "DASHDOT")
          (vla-put-LinetypeScale (vlax-ename->vla-object last-ent) 0.5)

          ;; 2. Aplica Fillet na Central
          (if (and radius (> radius 0.0)) 
            (progn 
              (setq old-fillet (getvar "FILLETRAD"))
              (setvar "FILLETRAD" radius)
              (command "_.FILLET" "_P" last-ent)
              (setvar "FILLETRAD" old-fillet)
            )
          )

          ;; 3. Gera Envelope Fechado (Offsets)
          (if (and tamanho (> tamanho 0.0)) 
            (progn 
              (setq vla-obj (vlax-ename->vla-object last-ent))
              (setq half-width (/ tamanho 2.0))
              (setq off1 nil
                    off2 nil
              )

              ;; Gera Offsets
              (if 
                (not 
                  (vl-catch-all-error-p 
                    (setq res1 (vl-catch-all-apply 'vla-Offset 
                                                   (list vla-obj half-width)
                               )
                    )
                  )
                )
                (setq off1 (vlax-safearray-get-element (vlax-variant-value res1) 0))
              )
              (if 
                (not 
                  (vl-catch-all-error-p 
                    (setq res2 (vl-catch-all-apply 'vla-Offset 
                                                   (list vla-obj (- half-width))
                               )
                    )
                  )
                )
                (setq off2 (vlax-safearray-get-element (vlax-variant-value res2) 0))
              )

              (if (and off1 off2) 
                (progn 
                  (setq ent1 (vlax-vla-object->ename off1))
                  (setq ent2 (vlax-vla-object->ename off2))

                  ;; Extrai dados
                  (setq data1 (pipe:get-poly-data ent1))
                  (setq data2 (pipe:get-poly-data ent2))

                  ;; Deleta linhas temporarias de offset
                  (vla-Delete off1)
                  (vla-Delete off2)

                  (if (and data1 data2) 
                    (progn 
                      ;; Combina: Data1 + Inverso(Data2) + Fecha no inicio
                      (setq all-data (append data1 (pipe:reverse-poly-data data2)))

                      ;; Cria Envelope Fechado
                      (setq ent-list (append 
                                       (list 
                                         '(0 . "LWPOLYLINE")
                                         '(100 . "AcDbEntity")
                                         '(100 . "AcDbPolyline")
                                         (cons 90 (length all-data))
                                         '(70 . 1) ;; 1 = Fechada
                                       )
                                       (apply 'append 
                                              (mapcar 
                                                '(lambda (item) 
                                                   (list 
                                                     (cons 10 (car item))
                                                     (cons 42 (cadr item))
                                                   )
                                                 )
                                                all-data
                                              )
                                       )
                                     )
                      )

                      (if (entmake ent-list) 
                        (progn 
                          (setq result-env (entlast))
                          (pipe:add-xdata result-env)

                          ;; 4. Cria Hachura Solida
                          (setq hatch-ent (pipe:create-hatch result-env))

                          ;; 5. Cria Wipeout (tesselliza arcos) - SEMPRE LIGADO
                          (setq wipeout-ent (pipe:create-wipeout result-env))

                          ;; 6. Ajusta Draw Order
                          (pipe:set-draw-order 
                            (if manter-central (list last-ent result-env) (list result-env))
                            hatch-ent
                            wipeout-ent
                          )

                          ;; 7. Deleta a central se nao for para manter
                          (if (not manter-central)
                            (if (and last-ent (entget last-ent)) (entdel last-ent))
                          )
                        )
                        (princ "\nErro ao criar envelope.")
                      )
                    )
                  )
                )
                (princ "\nErro ao gerar offsets (geometria complexa?).")
              )
            )
          )
        )
        (princ "\nErro ao criar polilinha.")
      )
    )
    (princ "\nNecessario pelo menos 2 pontos.")
  )
)

;;; ==========================================================================
;;; COMANDO PI
;;; ==========================================================================
(defun c:PI (/ pts pt p1 temp_lines loop loop_start input tmp ss i ent poly-data width block-name current-curva current-tamanho vla-ent w doc old-cmdecho nodes corte-list all-segs id-counter base-ents j) 
  (vl-load-com)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (princ "\nIniciando comando PI - PIPE...")

  ;; 1. Verifica selecao previa (Pickfirst) de Polilinhas
  (if (setq ss (ssget "_I" '((0 . "LWPOLYLINE"))))
    (progn 
      (vla-StartUndoMark doc)
      (setq old-cmdecho (getvar "CMDECHO"))
      (setvar "CMDECHO" 0) 
      (setq i 0)
      (setq all-segs nil)
      (setq id-counter 0)
      (setq base-ents nil)
      (repeat (sslength ss) 
        (setq ent (ssname ss i))
        (setq base-ents (cons ent base-ents))
        (setq poly-data (pipe:get-poly-data ent))
        ;; Extrai apenas os pontos (car de cada item)
        (setq pts (mapcar 'car poly-data))

        ;; Pega o global width da polilinha via ActiveX para maior confiabilidade
        (setq width 0.0)
        (setq vla-ent (vlax-ename->vla-object ent))
        (if (not (vl-catch-all-error-p (setq w (vl-catch-all-apply 'vla-get-ConstantWidth (list vla-ent)))))
          (setq width w)
          ;; Tenta fallback pro dxf
          (if (setq w (cdr (assoc 43 (entget ent)))) (setq width w))
        )

        ;; Debug prints
        (setq j 0)
        (while (< j (1- (length pts)))
          (setq all-segs (cons (list (setq id-counter (1+ id-counter)) (nth j pts) (nth (1+ j) pts) width 0.0 0.0) all-segs))
          (setq j (1+ j))
        )
        (setq i (1+ i))
      )

      ;; 2. Pipeline de Grafos
      (princ "\nConstruindo rede de tubulacoes...")
      (setq corte-list (pipe:get-corte-blocks))
      (setq all-segs (pipe:split-segments all-segs corte-list))
      (setq nodes (pipe:build-nodes all-segs))
      (setq *pipe-new-blocks* nil)
      (setq *pipe-corte-sobe-blocks* nil)
      (setq all-segs (pipe:process-nodes nodes all-segs corte-list))
      (pipe:generate-pipes all-segs)

      ;; Bring blocks to front
      (if *pipe-new-blocks*
        (progn
          (setq ss-blocks (ssadd))
          (foreach blk *pipe-new-blocks* (ssadd blk ss-blocks))
          (vl-cmdf "_.DRAWORDER" ss-blocks "" "_F")
          (while (> (getvar "CMDACTIVE") 0) (command ""))
        )
      )

      ;; Bring "Sobe" reference blocks to the very front
      (if *pipe-corte-sobe-blocks*
        (progn
          (setq ss-sobe (ssadd))
          (foreach blk *pipe-corte-sobe-blocks* (ssadd blk ss-sobe))
          (vl-cmdf "_.DRAWORDER" ss-sobe "" "_F")
          (while (> (getvar "CMDACTIVE") 0) (command ""))
        )
      )

      ;; 3. Move polilinhas originais para PP_GUIAS e manda para tras
      (if base-ents
        (progn
          ;; Cria layer PP_GUIAS se nao existir (cor 3 = Verde)
          (if (not (tblsearch "LAYER" "PP_GUIAS"))
            (progn
              (vl-cmdf "_.LAYER" "_N" "PP_GUIAS" "_C" "3" "PP_GUIAS" "")
              (while (> (getvar "CMDACTIVE") 0) (command ""))
            )
          )
          
          (setq ss-guias (ssadd))
          (foreach ent base-ents
            (if (and ent (entget ent))
              (progn
                ;; Muda a layer
                (setq ent-data (entget ent))
                (setq ent-data (subst (cons 8 "PP_GUIAS") (assoc 8 ent-data) ent-data))
                (entmod ent-data)
                (ssadd ent ss-guias)
              )
            )
          )
          
          ;; Manda para tras
          (if (> (sslength ss-guias) 0)
            (progn
              (vl-cmdf "_.DRAWORDER" ss-guias "" "_B")
              (while (> (getvar "CMDACTIVE") 0) (command ""))
            )
          )
        )
      )

      ;; Remove os "grips" (pontos azuis de selecao) PRIMEIRO para destravar a tela
      (sssetfirst nil nil)
      ;; Atualiza a interface de video garantidamente
      (command "_.REDRAW")
      (setvar "CMDECHO" old-cmdecho)
      (vla-EndUndoMark doc)
      (princ (strcat "\nProcessadas " (itoa (sslength ss)) " polilinhas integradas no sistema."))
      (princ)
    )
    (progn 
      ;; 2. Caso nao haja selecao previa, inicia modo INTERATIVO
      (setq pts        nil
            temp_lines nil
            loop_start T
            p1         nil
      )


      ;; Loop inicial para capturar primeiro ponto ou opcoes
      (while loop_start 
        (initget "Curva Tamanho")
        (setq input (getpoint 
                      (strcat "\nEspecifique o primeiro ponto ou [Curva/Tamanho] <C=" 
                              (rtos *pipe-curva*)
                              " T="
                              (rtos *pipe-tamanho*)
                              ">: "
                      )
                    )
        )

        (cond 
          ;; Opcao Curva
          ((= input "Curva")
           (initget 4) ;; Nao permite negativo
           (setq tmp (getdist (strcat "\nRaio de curva <" (rtos *pipe-curva*) ">: ")))
           (if tmp (setq *pipe-curva* tmp))
          )

          ;; Opcao Tamanho
          ((= input "Tamanho")
           (initget 6) ;; Nao permite zero ou negativo
           (setq tmp (getdist 
                       (strcat "\nTamanho total (largura) <" 
                               (rtos *pipe-tamanho*)
                               ">: "
                       )
                     )
           )
           (if tmp (setq *pipe-tamanho* tmp))
          )

          ;; Ponto especificado
          ((listp input)
           (setq p1 input)
           (setq loop_start nil)
          )

          ;; Enter ou Cancelamento
          (T
           (setq loop_start nil)
          )
        )
      )

      (if p1 
        (progn 
          (vla-StartUndoMark doc)
          (setq pts (list p1))
          (setq loop T)

          ;; Loop para os proximos pontos
          (while loop 
            (setq pt (getpoint p1 "\nProximo ponto (ou Enter para finalizar): "))

            (if pt 
              (progn 
                ;; Desenha linha temporaria visual
                (entmake (list '(0 . "LINE") (cons 10 p1) (cons 11 pt)))
                (setq temp_lines (cons (entlast) temp_lines))

                ;; Adiciona a lista de pontos
                (setq pts (cons pt pts))

                ;; Atualiza ponto base
                (setq p1 pt)
              )
              ;; Se user der Enter (pt = nil), encerra loop
              (setq loop nil)
            )
          )

          ;; Finaliza criando a polyline, aplicando fillets e criando envelope
          (pipe:create_poly (reverse pts) *pipe-curva* *pipe-tamanho* temp_lines T)
          (vla-EndUndoMark doc)
        )
        (princ "\nCancelado.")
      )
    )
  )

  (princ)
)

(princ "\nComando PI carregado. Digite PI para iniciar.")
