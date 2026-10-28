;;; ==========================================================================
;;; Sistema de Rotulação de Tubulações de Gás (PEX Multicamadas)
;;; ==========================================================================
;;; Autor: Antigravity AI
;;; Descrição: Inserção de texto de diâmetro e tipo de tubulação com base na layer.
;;;            Desenha textos de 0.4 em fonte Arial com máscara de fundo automática.
;;;
;;; Comandos Disponíveis:
;;;   IG  - Inserir Gás (Ponto a Ponto): Insere o texto correspondente à layer atual 
;;;         nos pontos clicados.
;;;   IGS - Inserir Gás (Selecionando): Permite clicar em uma linha/tubulação e 
;;;         inserir o rótulo no ponto médio ou em um ponto de escolha, alinhado 
;;;         paralelamente ao tubo.
;;;   IGA - Inserir Gás (Automático): Seleciona múltiplos tubos e cria os rótulos 
;;;         automaticamente no ponto médio de cada um, com rotação correta.
;;; ==========================================================================

;; CARREGA AS FUNÇÕES VISUAL LISP
(vl-load-com)

;; ==========================================================================
;; CONFIGURAÇÃO GLOBAL
;; ==========================================================================
(setq *gas-mask* T)                 ; T = Habilita máscara de fundo (background mask); nil = Desabilita
(setq *gas-text-height* 0.4)        ; Altura do texto
(setq *gas-text-offset* 0.4)        ; Distância do texto acima da linha (offset)
(setq *gas-font-style* "ARIAL-GAS")  ; Nome do estilo de texto no AutoCAD

;; ==========================================================================
;; MAPEAMENTO DE LAYERS E TEXTOS
;; ==========================================================================
;; Nota: Usamos "%%C" que é o código nativo do AutoCAD para o símbolo de diâmetro (Ø).
;; Isso garante 100% de compatibilidade em qualquer versão ou codificação do AutoCAD.
(defun get-gas-text (layer-name len / up-layer is-short)
  (setq up-layer (strcase layer-name))
  (setq is-short (and len (< len 7.0)))
  (cond
    ((or (= up-layer "PP-GÁS PEX 16MM") (= up-layer "PP-GAS PEX 16MM"))
     (if is-short "%%C16mm" "%%C16mm - Multicamadas")
    )
    ((or (= up-layer "PP-GÁS PEX 20MM") (= up-layer "PP-GAS PEX 20MM"))
     (if is-short "%%C20mm" "%%C20mm - Multicamadas")
    )
    ((or (= up-layer "PP-GÁS PEX 26MM") (= up-layer "PP-GAS PEX 26MM"))
     (if is-short "%%C26mm" "%%C26mm - Multicamadas")
    )
    ((or (= up-layer "PP-GÁS PEX 32MM") (= up-layer "PP-GAS PEX 32MM"))
     (if is-short "%%C32mm" "%%C32mm - Multicamadas")
    )
    (t nil)
  )
)

;; ==========================================================================
;; CRIAÇÃO DO ESTILO DE TEXTO ARIAL
;; ==========================================================================
(defun init-gas-style ()
  (if (not (tblsearch "STYLE" *gas-font-style*))
    (entmake
      (list
        '(0 . "STYLE")
        '(100 . "AcDbSymbolTableRecord")
        '(100 . "AcDbTextStyleTableRecord")
        (cons 2 *gas-font-style*) ; Nome do estilo
        '(70 . 0)                 ; Flags padrão
        '(40 . 0.0)               ; Altura do texto fixa = 0.0 (permite definir na criação)
        '(41 . 1.0)               ; Fator de largura (Width factor)
        '(50 . 0.0)               ; Ângulo de inclinação (Oblique angle)
        '(71 . 0)                 ; Flags de geração
        (cons 42 *gas-text-height*) ; Última altura usada
        '(3 . "Arial.ttf")        ; Nome do arquivo de fonte TTF (Arial)
        '(4 . "")                 ; Fonte grande secundária (vazio para TTF)
      )
    )
  )
  (princ)
)

;; ==========================================================================
;; AJUSTE DE ÂNGULO PARA LEGIBILIDADE (Evita texto de cabeça para baixo)
;; ==========================================================================
(defun adjust-angle-readability (ang)
  ;; Normaliza o ângulo no intervalo [0, 2*PI)
  (while (>= ang (* 2.0 pi)) (setq ang (- ang (* 2.0 pi))))
  (while (< ang 0.0) (setq ang (+ ang (* 2.0 pi))))
  ;; Se o texto estiver apontando para a esquerda (Quadrantes II e III, entre 90° e 270°),
  ;; rotaciona em 180° para torná-lo legível da esquerda para a direita e de baixo para cima.
  (if (and (> ang (/ pi 2.0)) (<= ang (/ (* 3.0 pi) 2.0)))
    (setq ang (+ ang pi))
  )
  ;; Normaliza novamente após a rotação
  (while (>= ang (* 2.0 pi)) (setq ang (- ang (* 2.0 pi))))
  ang
)

;; ==========================================================================
;; OBTENÇÃO DO PONTO MÉDIO E ÂNGULO DA CURVA
;; ==========================================================================
(defun get-midpoint-and-angle (ent / obj start-param end-param mid-param mid-pt first-deriv ang)
  (setq obj (vlax-ename->vla-object ent))
  (setq start-param (vlax-curve-getStartParam obj))
  (setq end-param (vlax-curve-getEndParam obj))
  (setq mid-param (+ start-param (/ (- end-param start-param) 2.0)))
  (setq mid-pt (vlax-curve-getPointAtParam obj mid-param))
  (setq first-deriv (vlax-curve-getFirstDeriv obj mid-param))
  (setq ang (angle '(0 0 0) first-deriv))
  (setq ang (adjust-angle-readability ang))
  (list mid-pt ang)
)

;; ==========================================================================
;; CRIAÇÃO DA ENTIDADE DE TEXTO (MTEXT com Máscara opcional)
;; ==========================================================================
(defun create-gas-text (point angle-rad text layer-name / dxf-list ins-pt)
  (init-gas-style)
  ;; Desloca o ponto de inserção perpendicularmente acima do ângulo do texto
  (if (and *gas-text-offset* (> *gas-text-offset* 0.0))
    (setq ins-pt (polar point (+ angle-rad (/ pi 2.0)) *gas-text-offset*))
    (setq ins-pt point)
  )
  (setq dxf-list
    (list
      '(0 . "MTEXT")
      '(100 . "AcDbEntity")
      (cons 8 layer-name)         ; Insere na layer correspondente
      '(100 . "AcDbMText")
      (cons 10 ins-pt)            ; Ponto de inserção (coordenadas X,Y,Z)
      (cons 40 *gas-text-height*) ; Altura do texto (0.4)
      '(41 . 0.0)                 ; Largura de referência (0.0 = sem quebra automática de linha)
      '(71 . 5)                   ; Ponto de anexação: 5 = Meio Centro (Middle Center)
      (cons 1 text)               ; Conteúdo do texto
      (cons 7 *gas-font-style*)   ; Estilo Arial
      (cons 50 angle-rad)         ; Ângulo de rotação em radianos
    )
  )
  ;; Adiciona máscara de fundo (Background Mask) se habilitada
  (if *gas-mask*
    (setq dxf-list (append dxf-list (list '(90 . 3) '(45 . 1.25))))
  )
  (entmake dxf-list)
)


;; ==========================================================================
;; COMANDO 1: IG (Inserir Gás - Ponto a Ponto)
;; ==========================================================================
(defun c:ig (/ cur-layer txt pt)
  (setq cur-layer (getvar "CLAYER"))
  (setq txt (get-gas-text cur-layer nil))
  (if (not txt)
    (progn
      (princ (strcat "\nA layer atual \"" cur-layer "\" nao e uma layer de gas cadastrada."))
      (princ "\nPor favor, mude para uma das seguintes layers para usar este comando:")
      (princ "\n  - PP-GÁS PEX 16MM\n  - PP-GÁS PEX 20MM\n  - PP-GÁS PEX 26MM\n  - PP-GÁS PEX 32MM")
    )
    (progn
      (princ (strcat "\nLayer de gas ativa: " cur-layer " -> Texto: " txt))
      ;; Loop para inserção contínua
      (while (setq pt (getpoint "\nClique no ponto para inserir o texto (ou Enter/ESC para sair): "))
        (if (create-gas-text pt 0.0 txt cur-layer)
          (princ "\nRótulo inserido.")
          (princ "\nErro ao criar o texto.")
        )
      )
    )
  )
  (princ)
)

;; ==========================================================================
;; COMANDO 2: IGS (Inserir Gás Selecionando Tubo)
;; ==========================================================================
(defun c:igs (/ ent-sel ent obj lay len txt mid-and-ang mid-pt ang pt nearest-param first-deriv nearest-pt)
  (if (setq ent-sel (entsel "\nSelecione a linha/tubulacao de gas: "))
    (progn
      (setq ent (car ent-sel))
      (setq obj (vlax-ename->vla-object ent))
      (setq lay (vla-get-Layer obj))
      (setq len (vlax-curve-getDistAtParam obj (vlax-curve-getEndParam obj)))
      (setq txt (get-gas-text lay len))
      (if (not txt)
        (progn
          (princ (strcat "\nA layer do objeto (\"" lay "\") nao e uma layer de gas cadastrada."))
          (princ "\nLayers validas: PP-GÁS PEX 16MM, 20MM, 26MM, 32MM")
        )
        (progn
          (princ (strcat "\nTubulação detectada na layer: " lay " -> Texto: " txt))
          (setq mid-and-ang (get-midpoint-and-angle ent))
          (setq mid-pt (car mid-and-ang))
          (setq ang (cadr mid-and-ang))
          
          (setq pt (getpoint (strcat "\nClique no ponto de insercao (ou Enter para posicionar no PONTO MÉDIO): ")))
          (if (not pt)
            ;; Caso pressione Enter, usa o ponto médio e ângulo calculado
            (progn
              (create-gas-text mid-pt ang txt lay)
              (princ "\nRótulo inserido no ponto médio da tubulação.")
            )
            ;; Caso clique em um ponto específico, calcula a rotação naquele ponto específico da linha
            (progn
              (setq nearest-pt (vlax-curve-getClosestPointTo obj pt))
              (setq nearest-param (vlax-curve-getParamAtPoint obj nearest-pt))
              (setq first-deriv (vlax-curve-getFirstDeriv obj nearest-param))
              (setq ang (angle '(0 0 0) first-deriv))
              (setq ang (adjust-angle-readability ang))
              ;; Usamos nearest-pt para garantir que o texto seja alinhado exatamente a partir da linha
              (create-gas-text nearest-pt ang txt lay)
              (princ "\nRótulo inserido no ponto selecionado com alinhamento correto.")
            )
          )
        )
      )
    )
  )
  (princ)
)

;; ==========================================================================
;; COMANDO 3: IGA (Inserir Gás Automático)
;; ==========================================================================
(defun c:iga (/ ss i ent obj lay len txt mid-and-ang mid-pt ang count)
  (princ "\nSelecione as linhas/tubulacoes de gas para rotulacao automatica:")
  ;; Filtra apenas lines, polilinhas, arcos e splines
  (if (setq ss (ssget '((0 . "LINE,POLYLINE,LWPOLYLINE,ARC,SPLINE"))))
    (progn
      (setq count 0)
      (setq i 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i))
        (setq obj (vlax-ename->vla-object ent))
        (setq lay (vla-get-Layer obj))
        (setq len (vlax-curve-getDistAtParam obj (vlax-curve-getEndParam obj)))
        (setq txt (get-gas-text lay len))
        (if txt
          (progn
            (setq mid-and-ang (get-midpoint-and-angle ent))
            (setq mid-pt (car mid-and-ang))
            (setq ang (cadr mid-and-ang))
            (if (create-gas-text mid-pt ang txt lay)
              (setq count (1+ count))
            )
          )
        )
        (setq i (1+ i))
      )
      (princ (strcat "\nConcluido! Foram criados " (itoa count) " rotulos de gas automaticos."))
    )
    (princ "\nNenhum objeto compativel selecionado.")
  )
  (princ)
)

;; Mensagem de carregamento no console do AutoCAD
(princ "\n=======================================================")
(princ "\n  ROTULADOR DE DIÂMETROS DE GÁS PEX CARREGADO COM SUCESSO!")
(princ "\n  Use um dos comandos abaixo:")
(princ "\n    IG  - Inserir texto clicando ponto a ponto (na layer ativa)")
(princ "\n    IGS - Selecionar tubo e escolher ponto (ou Enter p/ ponto médio)")
(princ "\n    IGA - Rotular automaticamente múltiplos tubos selecionados")
(princ "\n=======================================================")
(princ)
