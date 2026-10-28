;;; ==========================================================================
;;; Automação AutoCAD LISP: Controle de Transparência de Layers do projeto
;;;
;;; Comandos:
;;;   - VPA60 ou VT : Define 60% de transparência nas layers configuradas abaixo
;;;                   e 80% especificamente na layer PP_CUBETA
;;;   - VPA0  ou VO : Retira a transparência dessas layers (define para 0%)
;;;
;;; Desenvolvido para: Proplan Engenharia
;;;
;;; Retorna a relação única de layers atendidas pelos dois comandos.
;;; Para acrescentar ou remover uma layer, altere somente esta lista.
;;; ========================================================================== 

(defun VPA:GetLayerList ()
  '("PP_ARQUITETURA"
    "PP_LAYOUT"
    "PP_PROJEÇÃO"
    "PP_FORRO"
    "PP_COBERTURA"
    "PP_GARAGEM"
    "PP_VEGETAÇÃO"
    "PP_TEXTO"
    "PP_TEXTO PILAR"
    "PP_TEXTO VIGA"
    "PP_CUBETA"
    "PP_VIGA"
    "PP_PILAR"
    "PILAR - HATCH"
  )
)

;;; --- COMANDO 1: APLICAR TRANSPARÊNCIA (VPA60 / VT) ---
(defun c:VPA60 ( / layerList csvLayers csvLayers60 oldEcho )
  (vl-load-com)
  
  ;; Lista das layers alvo
  (setq layerList (VPA:GetLayerList))
  (setq csvLayers "")
  (setq csvLayers60 "")
  
  ;; Filtra apenas as layers que existem no desenho para evitar erros na linha de comando
  (foreach lay layerList
    (if (tblsearch "LAYER" lay)
      (progn
        (if (= csvLayers "")
          (setq csvLayers lay)
          (setq csvLayers (strcat csvLayers "," lay))
        )
        (if (/= (strcase lay) "PP_CUBETA")
          (if (= csvLayers60 "")
            (setq csvLayers60 lay)
            (setq csvLayers60 (strcat csvLayers60 "," lay))
          )
        )
      )
    )
  )
  
  ;; Se pelo menos uma das layers existir no desenho
  (if (/= csvLayers "")
    (progn
      ;; Salva e desativa CMDECHO para evitar poluição visual na linha de comando
      (setq oldEcho (getvar "CMDECHO"))
      (setvar "CMDECHO" 0)
      
      ;; Aplica transparência de 60% às demais layers encontradas
      (if (/= csvLayers60 "")
        (command "_.-layer" "_tr" "60" csvLayers60 "")
      )

      ;; Aplica transparência de 80% especificamente à layer PP_CUBETA
      (if (tblsearch "LAYER" "PP_CUBETA")
        (command "_.-layer" "_tr" "80" "PP_CUBETA" "")
      )
      
      ;; Restaura a variável CMDECHO
      (setvar "CMDECHO" oldEcho)
      
      ;; Garante que a exibição de transparência do AutoCAD está ligada
      (if (= (getvar "TRANSPARENCYDISPLAY") 0)
        (progn
          (setvar "TRANSPARENCYDISPLAY" 1)
          (princ "\n[Info] Variável TRANSPARENCYDISPLAY ativada para exibir as transparências na tela.")
        )
      )
      
      ;; Regenera o desenho para atualizar a visualização na tela imediatamente
      (command "_.regen")
      
      (princ (strcat "\nSucesso: transparência aplicada às layers: " csvLayers
                     " (PP_CUBETA: 80%; demais: 60%)"))
    )
    (princ "\nNenhuma das layers configuradas foi encontrada neste desenho.")
  )
  (princ)
)

;; Atalho VT, V6, T6, V60 e T60 para aplicar a transparência configurada
(defun c:VT () (c:VPA60))
(defun c:V6 () (c:VPA60))
(defun c:T6 () (c:VPA60))
(defun c:V60 () (c:VPA60))
(defun c:T60 () (c:VPA60))


;;; --- COMANDO 2: RETIRAR TRANSPARÊNCIA (VPA0 / VO) ---
(defun c:VPA0 ( / layerList csvLayers oldEcho )
  (vl-load-com)
  
  ;; Lista das layers alvo
  (setq layerList (VPA:GetLayerList))
  (setq csvLayers "")
  
  ;; Filtra apenas as layers que existem no desenho
  (foreach lay layerList
    (if (tblsearch "LAYER" lay)
      (if (= csvLayers "")
        (setq csvLayers lay)
        (setq csvLayers (strcat csvLayers "," lay))
      )
    )
  )
  
  ;; Se pelo menos uma das layers existir no desenho
  (if (/= csvLayers "")
    (progn
      ;; Salva e desativa CMDECHO
      (setq oldEcho (getvar "CMDECHO"))
      (setvar "CMDECHO" 0)
      
      ;; Define transparência como 0% (totalmente opaco)
      (command "_.-layer" "_tr" "0" csvLayers "")
      
      ;; Restaura CMDECHO
      (setvar "CMDECHO" oldEcho)
      
      ;; Regenera o desenho
      (command "_.regen")
      
      (princ (strcat "\nSucesso: Transparência removida (0%) das layers: " csvLayers))
    )
    (princ "\nNenhuma das layers configuradas foi encontrada neste desenho.")
  )
  (princ)
)

;; Atalhos VO, V0, T0 para remover a transparência (0%)
(defun c:VO () (c:VPA0))
(defun c:V0 () (c:VPA0))
(defun c:T0 () (c:VPA0))


;;; --- MENSAGEM DE BOAS-VINDAS NO CARREGAMENTO ---
(princ "\n=======================================================")
(princ "\nRotina de Controle de Transparência carregada!")
(princ "\n  > Aplicar transparência: VT, V6, T6, V60 ou T60")
(princ "\n  > Retirar (0%): VO, V0 ou T0")
(princ "\n=======================================================")
(princ)
