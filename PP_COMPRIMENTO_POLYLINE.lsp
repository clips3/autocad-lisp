;;; Comando: CPL (Comprimento PolyLine ou Linha)
;;; Descrição: Insere o comprimento de múltiplas linhas ou polylines (inteiro ou por trechos) acima delas.

(defun c:CPL ( / opt ss textHeight i ent obj objName currentOpt len midPt param deriv ang textStr offsetPt endParam j startParam nextParam startDist endDist segLen segMidDist segMidPt )
  (vl-load-com)
  
  ;; 1. Perguntar ao usuário o modo de operação
  (initget 0 "Inteiro Trechos")
  (setq opt (getkword "\nDeseja cotar o comprimento [Inteiro/Trechos] <Inteiro>: "))
  (if (not opt) (setq opt "Inteiro"))
  
  ;; 2. Selecionar vários objetos (o filtro permite selecionar apenas linhas e polylines)
  (princ "\nSelecione as linhas e polylines: ")
  (setq ss (ssget '((0 . "LINE,LWPOLYLINE,POLYLINE"))))
  
  (if ss
    (progn
      ;; Obter a altura do texto atual configurada no AutoCAD
      (setq textHeight (getvar "TEXTSIZE"))
      (setq i 0)
      
      ;; 3. Loop em todos os objetos selecionados
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (setq obj (vlax-ename->vla-object ent))
        (setq objName (vla-get-ObjectName obj))
        
        ;; Definir modo específico para este objeto (se for linha simples, força "Inteiro")
        (setq currentOpt opt)
        (if (= objName "AcDbLine")
          (setq currentOpt "Inteiro")
        )
        
        (if (= currentOpt "Inteiro")
          ;; ==========================================
          ;; MODO 1: COMPRIMENTO INTEIRO DA LINHA/POLYLINE
          ;; ==========================================
          (progn
            (setq len (vlax-curve-getDistAtParam obj (vlax-curve-getEndParam obj)))
            
            ;; Omitir textos em elementos de tamanho quase zero
            (if (> len 0.001)
              (progn
                ;; Encontrar o ponto médio e a tangente
                (setq midPt (vlax-curve-getPointAtDist obj (/ len 2.0)))
                (setq param (vlax-curve-getParamAtDist obj (/ len 2.0)))
                (setq deriv (vlax-curve-getFirstDeriv obj param))
                (setq ang (atan (cadr deriv) (car deriv)))
                
                ;; Ajustar o ângulo para evitar texto de cabeça para baixo
                (if (> ang (/ pi 2.0)) (setq ang (- ang pi)))
                (if (< ang (/ pi -2.0)) (setq ang (+ ang pi)))
                
                ;; Calcular offset para baixo e definir texto
                (setq offsetPt (polar midPt (- ang (/ pi 2.0)) (/ textHeight 2.0)))
                (setq textStr (strcat "C=" (rtos len 2 2) "m"))
                
                ;; Inserção automática do texto no centro
                (entmake
                  (list
                    '(0 . "TEXT")
                    (cons 10 offsetPt)
                    (cons 40 textHeight)
                    (cons 1 textStr)
                    (cons 50 ang)
                    '(72 . 1)           ; Alinhamento horizontal: Center
                    (cons 11 offsetPt)
                    '(73 . 3)           ; Alinhamento vertical: Top (baixo da linha)
                  )
                )
              )
            )
          )
          
          ;; ==========================================
          ;; MODO 2: COMPRIMENTO POR TRECHOS (Apenas Polylines)
          ;; ==========================================
          (progn
            (setq endParam (fix (vlax-curve-getEndParam obj)))
            (setq j 0)
            (while (< j endParam)
              (setq startParam j)
              (setq nextParam (1+ j))
              
              (setq startDist (vlax-curve-getDistAtParam obj startParam))
              (setq endDist (vlax-curve-getDistAtParam obj nextParam))
              (setq segLen (- endDist startDist))
              
              ;; Ignorar trechos com tamanho praticamente 0
              (if (> segLen 0.001)
                (progn
                  ;; Calcular o meio deste trecho específico
                  (setq segMidDist (+ startDist (/ segLen 2.0)))
                  (setq segMidPt (vlax-curve-getPointAtDist obj segMidDist))
                  (setq param (vlax-curve-getParamAtDist obj segMidDist))
                  (setq deriv (vlax-curve-getFirstDeriv obj param))
                  (setq ang (atan (cadr deriv) (car deriv)))
                  
                  ;; Ajustar o ângulo
                  (if (> ang (/ pi 2.0)) (setq ang (- ang pi)))
                  (if (< ang (/ pi -2.0)) (setq ang (+ ang pi)))
                  
                  ;; Offset abaixo do trecho e texto
                  (setq offsetPt (polar segMidPt (- ang (/ pi 2.0)) (/ textHeight 2.0)))
                  (setq textStr (strcat "C=" (rtos segLen 2 2) "m"))
                  
                  ;; Inserir texto automaticamente
                  (entmake
                    (list
                      '(0 . "TEXT")
                      (cons 10 offsetPt)
                      (cons 40 textHeight)
                      (cons 1 textStr)
                      (cons 50 ang)
                      '(72 . 1)           ; Alinhamento horizontal: Center
                      (cons 11 offsetPt)
                      '(73 . 3)           ; Alinhamento vertical: Top (baixo da linha)
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
      (princ (strcat "\nSucesso! Textos de comprimento inseridos em " (itoa (sslength ss)) " objeto(s)."))
    )
    (princ "\nComando cancelado. Nenhum objeto válido selecionado.")
  )
  (princ)
)

(princ "\n=======================================================")
(princ "\nRotina de seleção múltipla atualizada! Digite 'CPL' para usar.")
(princ "\n=======================================================")
(princ)
