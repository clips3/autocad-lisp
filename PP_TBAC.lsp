;; ==============================================================================
;; Nome: PP_TBAC.lsp
;; Comando: TBAC
;; Descrição: Seleciona blocos no desenho, filtra pelos nomes predefinidos.
;;            Para cada bloco filtrado, extrai NOME e BTU, determina o MODELO
;;            da TB - AC conforme tabela de referência, insere o bloco "TB - AC"
;;            preenchendo atributos e definindo o estado de visibilidade MODELO.
;; ==============================================================================

(defun c:TBAC (/ ss i ent obj blkName validNames filteredSs pt extractedName 
               extractedBtuString extractedBlkName modelo newBlk tag pos j currentEnt 
               currentObj dataList currentData get-num get-prefix get-modelo prop 
               extractedAmbiente layoutList firstLayout oldOsmode minPt maxPt minX 
               minY maxX maxY ll ur halfSpaceModels yOffset qtd oldAttreq oldAttdia 
               foundItem oldCmdecho extractedSeparacao currentSeparacao sep sepBlk 
               sepBlockName tmpBlk
              ) 
  (vl-load-com)

  ;; Desativa o eco de comandos na interface para o script rodar consideravelmente mais rápido e limpo
  (setq oldCmdecho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)

  ;; Verifica se o usuário está no Model Space antes de iniciar, senão, muda para lá
  (if (/= (strcase (getvar "CTAB")) "MODEL") 
    (progn 
      (setvar "CTAB" "Model")
      (princ "\nMudando automaticamente para a aba Model...")
    )
  )

  ;; Função auxiliar para extrair apenas o número de uma string (ex: "T1" -> 1, "T11" -> 11)
  (defun get-num (s / idx c res) 
    (setq res "")
    (setq idx 1)
    (while (<= idx (strlen s)) 
      (setq c (substr s idx 1))
      (if (and (>= (ascii c) 48) (<= (ascii c) 57)) 
        (setq res (strcat res c))
      )
      (setq idx (1+ idx))
    )
    (if (= res "") 0 (atoi res))
  )

  ;; Função auxiliar para extrair o prefixo alfabético de uma string (ex: "A.0" -> "A", "T1" -> "T")
  (defun get-prefix (s / idx c res) 
    (setq res "")
    (setq idx 1)
    (while (<= idx (strlen s)) 
      (setq c (substr s idx 1))
      (if 
        (or (and (>= (ascii c) 65) (<= (ascii c) 90)) 
            (and (>= (ascii c) 97) (<= (ascii c) 122))
        )
        (setq res (strcat res (strcase c)))
      )
      (setq idx (1+ idx))
    )
    res
  )

  ;; Função auxiliar para determinar o MODELO da TB - AC com base no nome do bloco e BTU
  (defun get-modelo (bName btuStr / key modeloMap btuNum btuKey) 
    ;; Tabela de referência: ("NOME_DO_BLOCO|BTU" . "MODELO")
    (setq modeloMap '(("SKY AIR SPLIT - DUTO|18" . "FDMQA18A5VL")
                      ("SKY AIR SPLIT - DUTO|24" . "FDMQA24A5VL")
                      ("SKY AIR SPLIT - DUTO|30" . "FBQ30AVL")
                      ("SKY AIR SPLIT - DUTO|36" . "FBQ36AVL")
                      ("SKY AIR SPLIT - DUTO|42" . "FBQ42AVL")
                      ("SKY AIR SPLIT - DUTO|48" . "FBQ48AVL")
                      ("SPLIT R-32 - HI WALL|32" . "FTXM32TVL")
                      ("SMART GOLD R-32 - HI WALL|9" . "FTKP09S5VL")
                      ("SMART GOLD R-32 - HI WALL|12" . "FTKP12S5VL")
                      ("SMART GOLD R-32 - HI WALL|18" . "FTKP18S5VL")
                      ("SMART GOLD R-32 - HI WALL|24" . "FTKP24S5VL")
                      ("LG MULTI INVERTER - EVAPORADORA|7" . "AMNW07GSJA0")
                      ("LG MULTI INVERTER - EVAPORADORA|9" . "AMNW09GSJA0")
                      ("LG MULTI INVERTER - EVAPORADORA|12" . "AMNW12GSJA0")
                      ("LG MULTI INVERTER - EVAPORADORA|18" . "AMNW18GSKA0")
                      ("LG MULTI INVERTER - EVAPORADORA|24" . "AMNW24GSKA0")
                      ("LG MULTI INVERTER - CONDENSADORA|18" . "A2UW18GFA2")
                      ("LG MULTI INVERTER - CONDENSADORA|24" . "A3UW24GFA2")
                      ("LG MULTI INVERTER - CONDENSADORA|30" . "A4UW30GFA2")
                      ("LG MULTI INVERTER - CONDENSADORA|36" . "A5UW36GFA2")
                      ("LG MULTI INVERTER - CONDENSADORA|48" . "A5UW48GFA1")
                      ("MIDEA AIRVOLUTION - EVAPORADORA|9" . "42AFVCI09S5")
                      ("MIDEA AIRVOLUTION - EVAPORADORA|12" . "42AFVCI12S5")
                      ("MIDEA AIRVOLUTION - EVAPORADORA|18" . "42AFVCI18S5")
                      ("MIDEA AIRVOLUTION - EVAPORADORA|22" . "42AFVCI22S5")
                      ("MIDEA AIRVOLUTION FRIO - EVAPORADORA|9" . "42EFVCA09M5")
                      ("MIDEA AIRVOLUTION FRIO - EVAPORADORA|12" . "42EFVCA12M5")
                      ("MIDEA AIRVOLUTION FRIO - EVAPORADORA|18" . "42EFVCA18M5")
                      ("MIDEA AIRVOLUTION FRIO - EVAPORADORA|22" . "42EFVCA22M5")
                      ("LG MULTI INVERTER - CASSETE 1 VIA|9" . "AMNW09GTUC0")
                      ("LG MULTI INVERTER - CASSETE 1 VIA|12" . "AMNW12GTUC0")
                      ("LG MULTI INVERTER - CASSETE 1 VIA|18" . "AMNW18GTTC0")
                      ("LG INVERTER - CASSETE - EVAPORADORA|18" . "ZTNW18GPLAA")
                      ("LG INVERTER - CASSETE - EVAPORADORA|24" . "ZTNW24GPLAA")
                      ("LG INVERTER - CASSETE - EVAPORADORA|36" . "ZTNW36GNLAA")
                      ("LG INVERTER - CASSETE - EVAPORADORA|48" . "ZTNW48GMLAA")
                      ("LG INVERTER - CASSETE - EVAPORADORA|55" . "ZTNW60GMLAA")
                      ("SKY AIR SPLIT - CASSETE|30" . "FCQ30AVL")
                      ("SKY AIR SPLIT - CASSETE|36" . "FCQ36AVL")
                      ("SKY AIR SPLIT - CASSETE|42" . "FCQ42AVL")
                      ("SKY AIR SPLIT - CASSETE|48" . "FCQ48AVL")
                      ("CARRIER CASSETE 4 VIAS INVERTER CONNECT - EVAPORADORA|24" . "40KVQD24C5")
                      ("VRV 6 - CASSETE ROUND FLOW|9" . "FXFQ25AVM")
                      ("VRV 6 - CASSETE ROUND FLOW|12" . "FXFQ32AVM")
                      ("VRV 6 - CASSETE ROUND FLOW|15" . "FXFQ40AVM")
                      ("VRV 6 - CASSETE ROUND FLOW|19" . "FXFQ50AVM")
                      ("VRV 6 - CASSETE ROUND FLOW|24" . "FXFQ63AVM")
                      ("VRV 6 - CASSETE ROUND FLOW|30" . "FXFQ80AVM")
                      ("VRV 6 - CASSETE ROUND FLOW|38" . "FXFQ100AVM")
                      ("VRV 6 - CASSETE ROUND FLOW|47" . "FXFQ125AVM")
                      ("VRV 6 - CASSETE ROUND FLOW|54" . "FXFQ140AVM")
                      ("VRV 6 - UNIDADE EXTERNA|20" . "RXYQ20BTL")
                      ("MULTI SPLIT ADVANCE - CASSETE|9" . "FFQ25KVL")
                      ("MULTI SPLIT ADVANCE - CASSETE|12" . "FFQ35KVL")
                      ("MULTI SPLIT ADVANCE - CASSETE|18" . "FFQ50KVL")
                      ("MULTI SPLIT ADVANCE - CASSETE|20" . "FFQ60KVL")
                      ("MULTI SPLIT ADVANCE - CONDENSADORA|2MXS18" . "2MXS18PMVM")
                      ("MULTI SPLIT ADVANCE - CONDENSADORA|3MXS18" . "3MXS18PMVM")
                      ("MULTI SPLIT ADVANCE - CONDENSADORA|3MXS24" . "3MXS24PMVM")
                      ("MULTI SPLIT ADVANCE - CONDENSADORA|4MXS28" . "4MXS28PMVM")
                      ("MULTI SPLIT ADVANCE - CONDENSADORA|4MXS34" . "4MXS34PMVM")
                      ("MULTI SPLIT ADVANCE - CONDENSADORA|5XMS38" . "5MXS38PMVM")
                      ("TETO XPOWER INVERTER - EVAP|30" . "42ZQVD30C5")
                      ("TETO XPOWER INVERTER - EVAP|36" . "42ZQVD36C5")
                      ("TETO XPOWER INVERTER - EVAP|48" . "42ZQVD48C5")
                      ("TETO XPOWER INVERTER - EVAP|60" . "42ZQVD60C5")
                     )
    )
    ;; Monta a chave de busca: "NOME_DO_BLOCO|BTU_INTEIRO" ou string exato
    (if (= (strcase bName) "MULTI SPLIT ADVANCE - CONDENSADORA")
      (setq key (strcat (strcase bName) "|" (strcase btuStr)))
      (progn
        (setq btuNum (fix (atof btuStr)))
        (if (>= btuNum 1000)
          (setq btuKey (itoa (fix (/ btuNum 1000.0))))
          (setq btuKey (itoa btuNum))
        )
        (setq key (strcat (strcase bName) "|" btuKey))
      )
    )

    ;; Utiliza a função nativa O(1) de lista de associação Lisp
    (cdr (assoc key modeloMap))
  )

  ;; Lista de nomes de blocos válidos
  (setq validNames '("SPLIT R-32 - HI WALL" "SMART GOLD R-32 - HI WALL" 
                     "SKY AIR SPLIT - DUTO" "LG MULTI INVERTER - EVAPORADORA" 
                     "LG MULTI INVERTER - CONDENSADORA" 
                     "MIDEA AIRVOLUTION - EVAPORADORA" 
                     "MIDEA AIRVOLUTION FRIO - EVAPORADORA"
                     "LG MULTI INVERTER - CASSETE 1 VIA" 
                     "VRV 6 - CASSETE ROUND FLOW"
                     "LG INVERTER - CASSETE - EVAPORADORA" "SKY AIR SPLIT - CASSETE"
                     "CARRIER CASSETE 4 VIAS INVERTER CONNECT - EVAPORADORA"
                     "VRV 6 - UNIDADE EXTERNA"
                     "MULTI SPLIT ADVANCE - CASSETE"
                     "MULTI SPLIT ADVANCE - CONDENSADORA"
                     "TETO XPOWER INVERTER - EVAP"
                    )
  )

  ;; Lista de modelos que utilizam metade do espaçamento vertical na tabela
  (setq halfSpaceModels '("AMNW07GSJA0" "AMNW09GSJA0" "AMNW12GSJA0" "AMNW18GSKA0" 
                          "AMNW24GSKA0" "A2UW18GFA2" "A3UW24GFA2" "A4UW30GFA2" 
                          "A5UW36GFA2" "AMNW09GTUC0" "AMNW12GTUC0" "A5UW48GFA1" 
                          "AMNW18GTTC0" "FXFQ25AVM" "FXFQ32AVM" "FXFQ40AVM" 
                          "FXFQ50AVM" "FXFQ63AVM" "FXFQ80AVM" "FXFQ100AVM" 
                          "FXFQ125AVM" "FXFQ140AVM" "RXYQ20BTL" "FFQ25KVL" 
                          "FFQ35KVL" "FFQ50KVL" "FFQ60KVL" "2MXS18PMVM" 
                          "3MXS18PMVM" "3MXS24PMVM" "4MXS28PMVM" "4MXS34PMVM" 
                          "5MXS38PMVM"
                         )
  )

  (prompt "\nSelecione os blocos... ")

  ;; Filtra apenas para permitir a seleção de blocos (INSERT)
  (if (setq ss (ssget '((0 . "INSERT")))) 
    (progn 
      ;; Cria um selection set vazio para armazenar os blocos filtrados
      (setq filteredSs (ssadd))
      (setq i 0)

      ;; Itera sobre o selection set inicial
      (while (< i (sslength ss)) 
        (setq ent (ssname ss i))
        (setq obj (vlax-ename->vla-object ent))

        ;; Pega o nome do bloco (EffectiveName para suportar blocos dinâmicos)
        (if (vlax-property-available-p obj 'EffectiveName) 
          (setq blkName (vla-get-EffectiveName obj))
          (setq blkName (vla-get-Name obj))
        )

        ;; Compara o nome do bloco (ignorando maiúsculas/minúsculas)
        (if (member (strcase blkName) (mapcar 'strcase validNames)) 
          (ssadd ent filteredSs)
        )
        (setq i (1+ i))
      )

      ;; Verifica se algum bloco passou pelo filtro
      (if (> (sslength filteredSs) 0) 
        (progn 
          (princ 
            (strcat "\n" 
                    (itoa (sslength filteredSs))
                    " blocos filtrados foram selecionados."
            )
          )

          (command "_.REGEN")

          ;; Checa se o bloco TB - AC existe antes do loop de inserção
          (if (tblsearch "BLOCK" "TB - AC") 
            (progn 
              (setq j 0)
              (setq dataList nil)

              ;; PRIMEIRO PASSO: Itera sobre cada bloco filtrado para extrair seus dados
              (while (< j (sslength filteredSs)) 
                (setq currentEnt (ssname filteredSs j))
                (setq currentObj (vlax-ename->vla-object currentEnt))
                (setq extractedName "")
                (setq extractedBtuString "")
                (setq extractedBlkName "")
                (setq extractedAmbiente "----")
                (setq extractedSeparacao "")

                ;; Pega o EffectiveName do bloco de origem
                (if (vlax-property-available-p currentObj 'EffectiveName) 
                  (setq extractedBlkName (vla-get-EffectiveName currentObj))
                  (setq extractedBlkName (vla-get-Name currentObj))
                )

                ;; Extrai os atributos "NOME" e "AMBIENTE" deste bloco específico
                (if (= (vla-get-HasAttributes currentObj) :vlax-true) 
                  (foreach att (vlax-invoke currentObj 'GetAttributes) 
                    (cond 
                      ((= (strcase (vla-get-TagString att)) "NOME")
                       (setq extractedName (vla-get-TextString att))
                       ;; Remove prefixo antes de "-", se houver (ex: "UE - T1" vira "T1")
                       (if (setq pos (vl-string-search "-" extractedName)) 
                         (setq extractedName (vl-string-left-trim " " 
                                                                  (substr extractedName 
                                                                          (+ pos 2)
                                                                  )
                                             )
                         )
                       )
                      )
                      ((= (strcase (vla-get-TagString att)) "AMBIENTE")
                       (if (/= (vla-get-TextString att) "") 
                         (setq extractedAmbiente (vla-get-TextString att))
                       )
                      )
                      ((vl-string-search "SEPARA" 
                                         (strcase (vla-get-TagString att))
                       )
                       (if (/= (vla-get-TextString att) "") 
                         (setq extractedSeparacao (vla-get-TextString att))
                       )
                      )
                    )
                  )
                )

                ;; Extrai a propriedade dinâmica "Btu" do bloco de origem
                (foreach prop (vlax-invoke currentObj 'GetDynamicBlockProperties) 
                  (if (= (strcase (vla-get-PropertyName prop)) "BTU") 
                    (setq extractedBtuString (vl-princ-to-string (vlax-get prop 'Value)))
                  )
                )

                ;; Determina o MODELO com base no nome do bloco e BTU
                (setq modelo (get-modelo extractedBlkName extractedBtuString))

                ;; Verifica se já existe um equipamento igual na lista
                (setq foundItem nil)
                (setq dataList (mapcar 
                                 '(lambda (item) 
                                    (if 
                                      (and (= (cadr item) extractedName) 
                                           (= (caddr item) modelo)
                                           (= (cadddr item) extractedAmbiente)
                                           (= (nth 5 item) extractedSeparacao)
                                      )
                                      (progn 
                                        (setq foundItem T)
                                        (list (car item) 
                                              (cadr item)
                                              (caddr item)
                                              (cadddr item)
                                              (1+ (nth 4 item))
                                              (nth 5 item)
                                        )
                                      )
                                      item
                                    )
                                  )
                                 dataList
                               )
                )

                ;; Se não encontrou equipamento igual, adiciona novo na lista com Quantidade = 1
                ;; Armazena: (índice nome modelo ambiente quantidade separacao)
                (if (not foundItem) 
                  (setq dataList (cons 
                                   (list j extractedName modelo extractedAmbiente 1 
                                         extractedSeparacao
                                   )
                                   dataList
                                 )
                  )
                )

                (setq j (1+ j))
              ) ;; Fim do while (< j (sslength filteredSs))

              ;; SEGUNDO PASSO: Ordena os dados extraídos pelo prefixo alfabético e depois pelo número
              (setq dataList (vl-sort dataList 
                                      '(lambda (a b / s1 s2 p1 p2 n1 n2) 
                                         (setq s1 (nth 5 a))
                                         (setq s2 (nth 5 b))
                                         (setq p1 (get-prefix (cadr a)))
                                         (setq p2 (get-prefix (cadr b)))
                                         (setq n1 (get-num (cadr a)))
                                         (setq n2 (get-num (cadr b)))
                                         (cond 
                                           ((/= s1 s2) (< s1 s2)) ;; Ordena primeiro pela Separação
                                           ((/= p1 p2) (< p1 p2)) ;; Prefixo diferente: ordena alfabeticamente
                                           ((/= n1 n2) (< n1 n2)) ;; Mesmo prefixo: ordena pelo número
                                           (T (< (car a) (car b))) ;; Tudo igual: mantém ordem original
                                         )
                                       )
                             )
              )

              ;; TERCEIRO PASSO: Trocar para a aba de Layout
              ;; Pega o nome do primeiro layout disponível que não seja "Model" usando propriedades ActiveX
              (setq layoutList nil)
              (vlax-for layout (vla-get-Layouts (vla-get-ActiveDocument (vlax-get-acad-object))) 
                (if (/= (strcase (vla-get-Name layout)) "MODEL") 
                  (setq layoutList (cons (vla-get-Name layout) layoutList))
                )
              )

              (if (> (length layoutList) 0) 
                (progn 
                  ;; Pega o último layout da lista (que na verdade foi o primeiro lido, para garantir um layout padrão)
                  (setq firstLayout (car (reverse layoutList)))
                  (setvar "CTAB" firstLayout)
                  (princ (strcat "\nMudando para o layout: " firstLayout))
                )
                (princ "\nAviso: Nenhum layout encontrado. Continuando no Model.")
              )

              ;; QUARTO PASSO: Pede o ponto de inserção para a tabela no Layout
              (if (setq pt (getpoint "\nEspecifique o ponto de inserção para a tabela TB - AC: ")) 
                (progn 
                  ;; Desativa o OSNAP temporariamente para que pontos não sejam "puxados" por objetos próximos na linha Y
                  (setq oldOsmode (getvar "OSMODE"))
                  (setvar "OSMODE" 0)

                  ;; Desativa as janelas de atributos (prompt e caixa de diálogo) da inserção de blocos
                  (setq oldAttreq (getvar "ATTREQ"))
                  (setq oldAttdia (getvar "ATTDIA"))
                  (setvar "ATTREQ" 0)
                  (setvar "ATTDIA" 0)

                  ;; Inicializa os limites da bounding box global antes de inserir os blocos
                  (setq minX nil
                        minY nil
                        maxX nil
                        maxY nil
                  )

                  ;; Insere o bloco de título "TB - AC TITULO" no ponto clicado
                  (if (tblsearch "BLOCK" "TB - AC TITULO") 
                    (progn 
                      (command "_.-INSERT" "TB - AC TITULO" pt)
                      (while (> (getvar "CMDACTIVE") 0) 
                        (command "")
                      )
                      ;; Atualiza bounding box global com o bloco do título
                      (vla-GetBoundingBox (vlax-ename->vla-object (entlast)) 
                                          'll
                                          'ur
                      )
                      (setq minPt (vlax-safearray->list ll))
                      (setq maxPt (vlax-safearray->list ur))
                      (setq minX (car minPt)
                            minY (cadr minPt)
                            maxX (car maxPt)
                            maxY (cadr maxPt)
                      )
                    )
                    (princ "\nAviso: O bloco \"TB - AC TITULO\" não foi encontrado. Inserindo apenas as linhas de dados.")
                  )

                  (setq currentSeparacao nil)

                  ;; Itera sobre a lista ORDENADA para inserir os blocos em cascata
                  (foreach currentData dataList 
                    (setq extractedName (cadr currentData))
                    (setq modelo (caddr currentData))
                    (setq extractedAmbiente (cadddr currentData))
                    (setq qtd (nth 4 currentData))
                    (setq sep (nth 5 currentData))

                    ;; Verifica se mudou a separação (e se ela não for nula/vazia)
                    (if (/= sep currentSeparacao) 
                      (progn 
                        (setq currentSeparacao sep)

                        ;; Tenta encontrar o bloco de separação ignorando problemas de acento (encoding)
                        (setq sepBlockName nil)
                        (setq tmpBlk (tblnext "BLOCK" T))
                        (while tmpBlk 
                          (if 
                            (vl-string-search "SEPARA" 
                                              (strcase (cdr (assoc 2 tmpBlk)))
                            )
                            (setq sepBlockName (cdr (assoc 2 tmpBlk)))
                          )
                          (setq tmpBlk (tblnext "BLOCK"))
                        )

                        (if sepBlockName 
                          (progn 
                            (command "_.-INSERT" sepBlockName pt)
                            (while (> (getvar "CMDACTIVE") 0) 
                              (command "")
                            )
                            (setq sepBlk (vlax-ename->vla-object (entlast)))

                            ;; Preenche atributos do bloco SEPARAÇÃO (se houver) com o texto da separação
                            (if (= (vla-get-HasAttributes sepBlk) :vlax-true) 
                              (foreach att (vlax-invoke sepBlk 'GetAttributes) 
                                (vla-put-TextString att currentSeparacao)
                              )
                            )

                            ;; Move o ponto de inserção para baixo no eixo Y (deslocamento pela metade para o bloco SEPARAÇÃO)
                            (setq pt (list (car pt) 
                                           (- (cadr pt) (/ 0.00816656 2.0))
                                           (caddr pt)
                                     )
                            )

                            ;; Atualiza bounding box global ignorando geometria invisivel (vla-GetBoundingBox bug workaround)
                            (if (not minX) 
                              (progn 
                                (vla-GetBoundingBox sepBlk 'll 'ur)
                                (setq minPt (vlax-safearray->list ll)
                                      maxPt (vlax-safearray->list ur)
                                )
                                (setq minX (car minPt)
                                      maxX (car maxPt)
                                      maxY (cadr maxPt)
                                )
                              )
                            )
                            (if (not minY) 
                              (setq minY (cadr pt))
                              (setq minY (min minY (cadr pt)))
                            )
                          )
                          (princ 
                            (strcat "\nAviso: Um bloco contendo SEPARA no nome não foi encontrado. Não foi possível inserir divisória para: " 
                                    sep
                            )
                          )
                        )
                      )
                    )

                    ;; Formata a quantidade para ter sempre dois dígitos (ex: 1 -> "01")
                    (if (< qtd 10) 
                      (setq qtd (strcat "0" (itoa qtd)))
                      (setq qtd (itoa qtd))
                    )

                    ;; Insere o bloco no ponto atual
                    (command "_.-INSERT" "TB - AC" pt)
                    ;; Responde "Enter" para todos os prompts
                    (while (> (getvar "CMDACTIVE") 0) 
                      (command "")
                    )

                    ;; Captura o bloco inserido
                    (setq newBlk (vlax-ename->vla-object (entlast)))

                    ;; Define o estado de visibilidade "MODELO" no bloco dinâmico TB - AC
                    (if modelo 
                      (foreach prop (vlax-invoke newBlk 'GetDynamicBlockProperties) 
                        (if (= (strcase (vla-get-PropertyName prop)) "MODELO") 
                          (vlax-put prop 'Value modelo)
                        )
                      )
                    )

                    ;; Preenche os atributos automaticamente
                    (if (= (vla-get-HasAttributes newBlk) :vlax-true) 
                      (foreach att (vlax-invoke newBlk 'GetAttributes) 
                        (setq tag (strcase (vla-get-TagString att)))
                        (cond 
                          ((or (= tag "QUANT.") (= tag "QUANT") (= tag "QUANT.-"))
                           (vla-put-TextString att qtd)
                          )
                          ((= tag "AMBIENTE")
                           (vla-put-TextString att extractedAmbiente)
                          )
                          ((or (= tag "NOME") (= tag "NOME-"))
                           (vla-put-TextString att extractedName)
                          )
                        )
                      )
                    )

                    ;; Define a distância de deslocamento vertical com base no modelo
                    (setq yOffset 0.00816656)
                    (if (member modelo halfSpaceModels) 
                      (setq yOffset (/ 0.00816656 2.0))
                    )

                    ;; Move o ponto de inserção para baixo no eixo Y
                    (setq pt (list (car pt) (- (cadr pt) yOffset) (caddr pt)))

                    ;; Atualiza o limite inferior (minY) com base na Posição Final matemática e não na caixa delimitadora
                    (if (not minX) 
                      (progn 
                        (vla-GetBoundingBox newBlk 'll 'ur)
                        (setq minPt (vlax-safearray->list ll)
                              maxPt (vlax-safearray->list ur)
                        )
                        (setq minX (car minPt)
                              maxX (car maxPt)
                              maxY (cadr maxPt)
                        )
                      )
                    )
                    (if (not minY) 
                      (setq minY (cadr pt))
                      (setq minY (min minY (cadr pt)))
                    )
                  ) ;; Fim do foreach currentData dataList

                  ;; Cria Wipeout englobando a Bounding Box total
                  (if (and minX minY maxX maxY) 
                    (progn 
                      ;; Ajuste para remover excesso no lado direito do wipeout
                      (setq maxX (- maxX 0.00091912))
                      (command "_.WIPEOUT" 
                               (list minX minY)
                               (list minX maxY)
                               (list maxX maxY)
                               (list maxX minY)
                               "C"
                      )
                      ;; Envia o Wipeout para trás de todos os outros objetos
                      (command "_.DRAWORDER" (entlast) "" "B")
                    )
                  )

                  ;; Finalização: tira a seleção dos blocos e dá um regen
                  (sssetfirst nil nil)
                  (command "_.REGEN")
                  (setvar "OSMODE" oldOsmode)
                  (setvar "ATTREQ" oldAttreq)
                  (setvar "ATTDIA" oldAttdia)
                  (setvar "CMDECHO" oldCmdecho)
                  (princ "\nComando finalizado. Tabela inserida com sucesso.")
                )
                (progn 
                  (princ "\nPonto de inserção cancelado.")
                  (setvar "CMDECHO" oldCmdecho)
                )
              )
            )
            (progn 
              (princ "\nErro: O bloco \"TB - AC\" não foi encontrado neste desenho!")
              (setvar "CMDECHO" oldCmdecho)
            )
          )
        )
        (progn 
          (princ "\nNenhum bloco correspondente aos filtros foi encontrado na seleção.")
          (setvar "CMDECHO" oldCmdecho)
        )
      )
    )
    (progn 
      (princ "\nNenhum objeto selecionado.")
      (setvar "CMDECHO" oldCmdecho)
    )
  )

  (princ)
)

(princ "\nComando TBAC carregado com sucesso. Digite TBAC para usar.")
(princ)
