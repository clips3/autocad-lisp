;;; ==========================================================================
;;; Comando: AP (Area Points - BB_AREAPOINTS)
;;; Descrição: Permite ao usuário desenhar uma área fechada (Polyline) e em
;;;            seguida pede uma quantidade de pontos. A lisp utiliza Ray Casting
;;;            e K-Means Clusterização para posicionar matematicamente os pontos
;;;            nos centros "naturais" mais convenientes da forma complexa desenhada,
;;;            suportando polígonos côncavos em "C", "L" ou formas arqueadas.
;;; ==========================================================================

(vl-load-com)

;;; Ray Casting: Avalia se um ponto está dentro de uma coleção de vértices sequenciais
(defun pt-in-poly-p (pt polyPts / p1 p2 count n x y p1x p1y p2x p2y) 
  (setq count 0)
  (setq x (car pt)
        y (cadr pt)
  )
  (setq n (length polyPts))
  (setq p1 (nth (1- n) polyPts))
  (foreach p2 polyPts 
    (setq p1x (car p1)
          p1y (cadr p1)
    )
    (setq p2x (car p2)
          p2y (cadr p2)
    )
    ;; Verifica interceptação do raio no eixo Y
    (if 
      (or (and (<= p1y y) (> p2y y)) 
          (and (> p1y y) (<= p2y y))
      )
      ;; Interpola eq. da reta para checar intercept X à direita
      (if (< x (+ p1x (* (/ (- y p1y) (- p2y p1y)) (- p2x p1x)))) 
        (setq count (1+ count))
      )
    )
    (setq p1 p2)
  )
  ;; Se quantidade de interceptações é ímpar, o ponto é interno
  (= (rem count 2) 1)
)

;;; Transformação: Discretiza o objeto geométrico em ~150 arestas retas virtuais
;;; (garantindo que curvas, arcos, etc funcionem sem estragar o Ray Casting)
(defun discretize-poly (ent / obj len step dist pts pt params) 
  (setq obj (vlax-ename->vla-object ent))
  (if 
    (vl-catch-all-error-p 
      (setq len (vl-catch-all-apply 'vlax-curve-getDistAtParam 
                                    (list obj (vlax-curve-getEndParam obj))
                )
      )
    )
    nil ; Falhou ao ler curva
    (progn 
      (setq step (/ len 150.0))
      (setq dist 0.0)
      (setq pts nil)
      (while (< dist len) 
        (setq pt (vlax-curve-getPointAtDist obj dist))
        (setq pts (cons (list (car pt) (cadr pt)) pts))
        (setq dist (+ dist step))
      )
      ;; Inclui estritamente o endpoint para selar as perdas decimais
      (setq pt (vlax-curve-getEndPoint obj))
      (setq pts (cons (list (car pt) (cadr pt)) pts))
      (reverse pts)
    )
  )
)

;;; Helper de performance (Distância quadrada)
(defun dist-sq (p1 p2 / dx dy) 
  (setq dx (- (car p1) (car p2))
        dy (- (cadr p1) (cadr p2))
  )
  (+ (* dx dx) (* dy dy))
)

;;; Matemática Celular: Cria as fronteiras (Diagrama de Voronoi) intersecionadas perfeitamente na forma
(defun draw-voronoi-lines (centroids polyPts / N edges p1 p2 i j c1 c2 M Vx Vy vlen 
                           tList eg ip tval k c3 M3 Vx3 Vy3 p3 p4 uniqueT lastT nT idx 
                           tm pmid mindT ok ptA ptB
                          ) 
  (setq N (length centroids))
  (if (<= N 1) 
    (princ)
    (progn 
      ;; Pré-calcula as arestas do polígono baseadas na nuvem limítrofe
      (setq edges nil)
      (setq p1 (nth (1- (length polyPts)) polyPts))
      (foreach p2 polyPts 
        (setq edges (cons (list p1 p2) edges))
        (setq p1 p2)
      )

      (setq i 0)
      (while (< i (1- N)) 
        (setq c1 (nth i centroids))
        (setq j (1+ i))
        (while (< j N) 
          (setq c2 (nth j centroids))
          ;; M é o mid-point da ponte entre 2 centróides
          (setq M (list (/ (+ (car c1) (car c2)) 2.0) 
                        (/ (+ (cadr c1) (cadr c2)) 2.0)
                  )
          )
          ;; O Vetor perpendicular (direção exata da Bissetriz Divisória)
          (setq Vx (- (cadr c1) (cadr c2)))
          (setq Vy (- (car c2) (car c1)))

          (setq vlen (sqrt (+ (* Vx Vx) (* Vy Vy))))
          (if (> vlen 1e-6) 
            (progn 
              (setq Vx (/ Vx vlen)
                    Vy (/ Vy vlen)
              )
              (setq p1 M
                    p2 (list (+ (car M) Vx) (+ (cadr M) Vy))
              )

              (setq tList nil)

              ;; 1) Interseção bruta com todas as arestas do Polígono (para prender a linha dentro da casa)
              (foreach eg edges 
                ;; Calcula interseção paramétrica infinita vs infinita
                (setq ip (inters p1 p2 (car eg) (cadr eg) nil))
                (if ip 
                  ;; Confirma se o nó caiu rigorosamente no trecho finito do muro
                  (if 
                    (equal (+ (distance (car eg) ip) (distance ip (cadr eg))) 
                           (distance (car eg) (cadr eg))
                           1e-4
                    )
                    (progn 
                      (setq tval (+ (* (- (car ip) (car M)) Vx) 
                                    (* (- (cadr ip) (cadr M)) Vy)
                                 )
                      )
                      (setq tList (cons tval tList))
                    )
                  )
                )
              )

              ;; 2) Choque Dinâmico com outras Bissetrizes (As Quinas Multiplas de Voronoi)
              (setq k 0)
              (while (< k N) 
                (if (and (/= k i) (/= k j)) 
                  (progn 
                    (setq c3 (nth k centroids))
                    (setq M3 (list (/ (+ (car c1) (car c3)) 2.0) 
                                   (/ (+ (cadr c1) (cadr c3)) 2.0)
                             )
                    )
                    (setq Vx3 (- (cadr c1) (cadr c3)))
                    (setq Vy3 (- (car c3) (car c1)))
                    (setq p3 M3
                          p4 (list (+ (car M3) Vx3) (+ (cadr M3) Vy3))
                    )

                    (setq ip (inters p1 p2 p3 p4 nil))
                    (if ip 
                      (progn 
                        (setq tval (+ (* (- (car ip) (car M)) Vx) 
                                      (* (- (cadr ip) (cadr M)) Vy)
                                   )
                        )
                        (setq tList (cons tval tList))
                      )
                    )
                  )
                )
                (setq k (1+ k))
              )

              ;; Organiza a teia de retas paramétricas e cimenta pedaços
              (if tList 
                (progn 
                  (setq tList (vl-sort tList '<))
                  (setq lastT (car tList))
                  (setq uniqueT (list lastT))
                  (foreach tv (cdr tList) 
                    (if (> (- tv lastT) 1e-3) 
                      (progn 
                        (setq uniqueT (cons tv uniqueT))
                        (setq lastT tv)
                      )
                    )
                  )
                  (setq uniqueT (reverse uniqueT))

                  ;; Recria as cordas válidas em blocos renderizáveis
                  (setq nT (length uniqueT))
                  (if (>= nT 2) 
                    (progn 
                      (setq idx 0)
                      (while (< idx (1- nT)) 
                        ;; Amostra Ponto Mediano para testar pureza do corte
                        (setq tm (/ 
                                   (+ (nth idx uniqueT) (nth (1+ idx) uniqueT))
                                   2.0
                                 )
                        )
                        (setq pmid (list (+ (car M) (* tm Vx)) 
                                         (+ (cadr M) (* tm Vy))
                                   )
                        )

                        ;; Validação Fina: Tá dentro da casa e pertence aos 2 donos exatos?
                        (if (pt-in-poly-p pmid polyPts) 
                          (progn 
                            (setq mindT (dist-sq pmid c1))
                            (setq ok T)
                            (setq k 0)
                            (while (and ok (< k N)) 
                              (if (and (/= k i) (/= k j)) 
                                (if 
                                  (< (dist-sq pmid (nth k centroids)) 
                                     (- mindT 1e-3)
                                  )
                                  (setq ok nil)
                                )
                              )
                              (setq k (1+ k))
                            )

                            (if ok 
                              (progn 
                                (setq ptA (list 
                                            (+ (car M) (* (nth idx uniqueT) Vx))
                                            (+ (cadr M) (* (nth idx uniqueT) Vy))
                                          )
                                )
                                (setq ptB (list 
                                            (+ (car M) 
                                               (* (nth (1+ idx) uniqueT) Vx)
                                            )
                                            (+ (cadr M) 
                                               (* (nth (1+ idx) uniqueT) Vy)
                                            )
                                          )
                                )
                                ;; Pinta rigorosamente ByLayer (Layer 0)
                                (entmake 
                                  (list '(0 . "LINE") 
                                        '(8 . "0")
                                        (cons 10 ptA)
                                        (cons 11 ptB)
                                  )
                                )
                              )
                            )
                          )
                        )
                        (setq idx (1+ idx))
                      )
                    )
                  )
                )
              )
            )
          )
          (setq j (1+ j))
        )
        (setq i (1+ i))
      )
    )
  )
)

;;; Comando Principal
(defun c:AP (/ oldCmd oldEnt newEnt ent minPt maxPt p1 p2 W H polyPts stepX stepY px 
             py internalPts finalPts validPts numPts len centroids newCentroids iter 
             clusters c avgX avgY closestPt minD d pt i idx closestCentroidIdx 
             clusterInfo clusterPts polyArea totalVal ptVal txtStr txtH txtPt oldLayer 
             oldColor curEnt blkName ss
            ) 

  (defun *error* (msg) 
    (if oldCmd (setvar "CMDECHO" oldCmd))
    (if oldLayer (setvar "CLAYER" oldLayer))
    (if oldColor (setvar "CECOLOR" oldColor))
    (if (not (wcmatch (strcase msg t) "*BREAK*,*CANCEL*,*EXIT*")) 
      (princ (strcat "\nErro: " msg))
    )
    (princ)
  )

  (setq oldCmd (getvar "CMDECHO"))
  (setq oldLayer (getvar "CLAYER"))
  (setq oldColor (getvar "CECOLOR"))
  (setvar "CMDECHO" 0)
  (setvar "CLAYER" "0")
  (setvar "CECOLOR" "BYLAYER")

  (princ "\nDesenhe a área desejada (Clique sequencialmente para formar o polígono). Pressione ENTER ao finalizar.")
  (setq oldEnt (entlast))

  (vl-cmdf "_.PLINE")
  (while (> (getvar "CMDACTIVE") 0) 
    (vl-cmdf PAUSE)
  )

  (setq newEnt (entlast))

  (if (not (equal oldEnt newEnt)) 
    (progn 
      (setq ent newEnt)
      ;; Fecha para evitar leaking do raycasting
      (if (and ent (= (cdr (assoc 0 (entget ent))) "LWPOLYLINE")) 
        (vl-cmdf "_.PEDIT" ent "_C" "")
      )

      ;; Obtém número inteiro alvo
      (setq numPts   nil
            validPts nil
      )
      (while (not validPts) 
        (setq numPts (getint "\nDigite o número de pontos a distribuir (ou de ENTER para cancelar): "))
        (if numPts 
          (if (> numPts 0) 
            (setq validPts T)
            (princ "\nO número deve ser maior que zero!")
          )
          (setq validPts 'CANCEL)
        )
      )

      (if (eq validPts T) 
        (progn 
          (princ "\nExecutando I.A. Geométrica K-Means...")

          (vla-getboundingbox (vlax-ename->vla-object ent) 'minPt 'maxPt)
          (setq p1 (vlax-safearray->list minPt))
          (setq p2 (vlax-safearray->list maxPt))
          (setq W (- (car p2) (car p1)))
          (setq H (- (cadr p2) (cadr p1)))

          (setq polyPts (discretize-poly ent))

          (if (not polyPts) 
            (princ "\nErro: A área desenhada não pôde ser interpretada numericamente.")
            (progn 
              ;; 1. Estabelece a nuvem de discretização (Sampling grid) ~3600 pontos
              (setq stepX (/ W 60.0))
              (setq stepY (/ H 60.0))
              (if (= stepX 0.0) (setq stepX 0.001))
              (if (= stepY 0.0) (setq stepY 0.001))

              (setq internalPts nil)
              (setq px (+ (car p1) (/ stepX 2.0)))
              (while (< px (car p2)) 
                (setq py (+ (cadr p1) (/ stepY 2.0)))
                (while (< py (cadr p2)) 
                  ;; Retém as posições no interior do contorno com o RayCasting
                  (if (pt-in-poly-p (list px py) polyPts) 
                    (setq internalPts (cons (list px py) internalPts))
                  )
                  (setq py (+ py stepY))
                )
                (setq px (+ px stepX))
              )

              (if (< (length internalPts) numPts) 
                (princ "\nÁrea da forma é fina/incompleta demais para o grid geométrico atual.")
                (progn 
                  ;; Se for 1 ponto único, descobre o Centro de Massa em vez de clusterizar
                  (if (= numPts 1) 
                    (progn 
                      (setq avgX 0.0
                            avgY 0.0
                      )
                      (foreach pt internalPts 
                        (setq avgX (+ avgX (car pt))
                              avgY (+ avgY (cadr pt))
                        )
                      )
                      (setq avgX (/ avgX (length internalPts))
                            avgY (/ avgY (length internalPts))
                      )

                      ;; Ao invés de inserir na Média (que pode estar num "buraco" de concavidade),
                      ;; selecionamos a amostra interior salva que seja mais próxima dessa média matemática
                      (setq minD      1e99
                            closestPt nil
                      )
                      (foreach pt internalPts 
                        (if (< (setq d (dist-sq pt (list avgX avgY))) minD) 
                          (setq minD      d
                                closestPt pt
                          )
                        )
                      )
                      (setq finalPts (list closestPt))
                    )
                    (progn 
                      ;; >2 Pontos: Configura e Executa Logica Iterativa K-Means Llyod
                      (setq len (length internalPts))

                      ;; Inicializa sementes de clusters pulando identicamente sobre o dataset interior
                      (setq centroids nil)
                      (setq i 0)
                      (while (< i numPts) 
                        (setq idx (fix (* len (/ (+ i 0.5) numPts))))
                        (setq centroids (cons (nth idx internalPts) centroids))
                        (setq i (1+ i))
                      )
                      (setq centroids (reverse centroids))

                      ;; 15 gerações são perfeitas para estabilizar K-Means espacial em 3k pontos
                      (setq iter 0)
                      (while (< iter 15) 
                        (setq clusters nil)
                        (setq c 0)
                        (while (< c numPts) 
                          (setq clusters (cons (cons c nil) clusters))
                          (setq c (1+ c))
                        )

                        ;; Designa os vizinhos
                        (foreach pt internalPts 
                          (setq minD               1e99
                                closestCentroidIdx 0
                                c                  0
                          )
                          (foreach cent centroids 
                            (if (< (setq d (dist-sq pt cent)) minD) 
                              (setq minD               d
                                    closestCentroidIdx c
                              )
                            )
                            (setq c (1+ c))
                          )
                          ;; Grava no dicionario do cluster correspondente com cons pra O(1) update
                          (setq clusterInfo (assoc closestCentroidIdx clusters))
                          (setq clusters (subst 
                                           (cons closestCentroidIdx 
                                                 (cons pt (cdr clusterInfo))
                                           )
                                           clusterInfo
                                           clusters
                                         )
                          )
                        )

                        ;; Recalcula nova massa vetorial
                        (setq newCentroids nil)
                        (setq c 0)
                        (while (< c numPts) 
                          (setq clusterPts (cdr (assoc c clusters)))
                          (if clusterPts 
                            (progn 
                              (setq avgX 0.0
                                    avgY 0.0
                              )
                              (foreach pt clusterPts 
                                (setq avgX (+ avgX (car pt))
                                      avgY (+ avgY (cadr pt))
                                )
                              )
                              (setq avgX (/ avgX (length clusterPts))
                                    avgY (/ avgY (length clusterPts))
                              )

                              (setq minD      1e99
                                    closestPt nil
                              )
                              (foreach pt clusterPts 
                                (if (< (setq d (dist-sq pt (list avgX avgY))) minD) 
                                  (setq minD      d
                                        closestPt pt
                                  )
                                )
                              )
                              (setq newCentroids (cons closestPt newCentroids))
                            )
                            ;; Se um cluster engravidou outro ou ficou sem grid, não muda
                            (setq newCentroids (cons (nth c centroids) 
                                                     newCentroids
                                               )
                            )
                          )
                          (setq c (1+ c))
                        )
                        (setq centroids (reverse newCentroids))
                        (setq iter (1+ iter))
                      )
                      (setq finalPts centroids)
                    )
                  )

                  ;; Adiciona no ambiente do CAD
                  (if (<= (getvar "PDMODE") 1) 
                    (setvar "PDMODE" 3)
                  )

                  ;; Cálculo Baseado em Área
                  (setq polyArea (vla-get-Area (vlax-ename->vla-object ent)))
                  (setq totalVal (* polyArea 850.0))
                  (setq ptVal (/ totalVal numPts))
                  (setq txtStr (rtos ptVal 2 0)) ;; Formata numero pra literal sem decimais
                  (setq txtH (/ (min W H) 25.0)) ;; Escala de altura segura baseada no ambiente

                  (foreach pt finalPts 
                    (vl-cmdf "_.POINT" "_NON" pt)

                    ;; Offset lateral proporcional para colar o texto
                    (setq txtPt (list (+ (car pt) (* txtH 0.5)) 
                                      (+ (cadr pt) (* txtH 0.5))
                                )
                    )

                    ;; Cria instanciamento cru do Objeto Texto pra evitar prompts no AutoCAD
                    (entmake 
                      (list 
                        '(0 . "TEXT")
                        '(8 . "0")
                        '(7 . "Standard") ;; O Style solicitado
                        (cons 1 txtStr)
                        (cons 10 txtPt)
                        (cons 40 txtH)
                        '(50 . 0.0)
                      )
                    )
                  )

                  ;; Traça a fronteira final em limitando células de área
                  (draw-voronoi-lines finalPts polyPts)

                  ;; Reúne todos as novas criações pra fechar num Bloco unificado limpo
                  (setq ss (ssadd))
                  (setq curEnt (if oldEnt (entnext oldEnt) (entnext)))
                  (while curEnt 
                    (ssadd curEnt ss)
                    (setq curEnt (entnext curEnt))
                  )
                  (if (> (sslength ss) 0) 
                    (progn 
                      (setq blkName (strcat "BB_" 
                                            (vl-string-subst "_" 
                                                             "."
                                                             (rtos (getvar "CDATE") 
                                                                   2
                                                                   8
                                                             )
                                            )
                                    )
                      )
                      (vl-cmdf "_.-BLOCK" blkName "_NON" p1 ss "")
                      (vl-cmdf "_.-INSERT" blkName "_NON" p1 "1" "1" "0")
                    )
                  )

                  (princ 
                    (strcat "\nSucesso! Bloco Célula gerado contendo " 
                            (itoa numPts)
                            " pontos e áreas."
                    )
                  )
                )
              )
            )
          )
        )
        (princ "\nComando cancelado.")
      )
    )
    (princ "\nNenhuma polyline rastreada.")
  )

  (setvar "CMDECHO" oldCmd)
  (setvar "CLAYER" oldLayer)
  (setvar "CECOLOR" oldColor)
  (princ)
)

(princ "\nArquivo PP_AREAPOINTS.lsp (v2.0) carregado. Digite AP para rodar.")
(princ)
