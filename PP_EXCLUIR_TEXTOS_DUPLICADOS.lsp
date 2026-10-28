;;; ==========================================================================
;;; Comando: ETD (Excluir Textos Duplicados)
;;; Descrição: Remove textos (TEXT e MTEXT), atributos de blocos (INSERT com atributos)
;;;            e MULTILEADERs duplicados ou sobrepostos/colidindo.
;;; Autor: Antigravity
;;; Idioma: Português (Brasil)
;;; ==========================================================================

(defun c:ETD ( / acadObj doc old-cmdecho *error* ss tol compContent lst i ent pt str temp item rest continue sub-item sub-ent sub-pt sub-str dist del-count def-tol active-layout total-height height-count h dxf avg-height debug-count )
  (vl-load-com)
  
  (setq acadObj (vlax-get-acad-object)
        doc (vla-get-ActiveDocument acadObj)
        old-cmdecho (getvar "CMDECHO")
  )
  
  ;; Tratamento de erro local para restaurar variáveis e fechar marca de desfazer (UNDO)
  (defun *error* (msg)
    (if (and msg
             (not (member msg '("Function cancelled" "quit / exit abort" "Console Break" "função cancelada"))))
      (princ (strcat "\nErro: " msg))
    )
    (if doc (vla-EndUndoMark doc))
    (setvar "CMDECHO" old-cmdecho)
    (princ "\nOperação cancelada.")
    (princ)
  )
  
  (setvar "CMDECHO" 0)
  (vla-StartUndoMark doc)
  
  ;; Função para determinar o layout ativo (resolve viewports no Paper Space)
  (defun get-active-space-layout ()
    (if (= (getvar "TILEMODE") 1)
      "Model"
      (if (> (getvar "CVPORT") 1)
        "Model"
        (getvar "CTAB")
      )
    )
  )
  
  (setq active-layout (get-active-space-layout))
  
  ;; 1. Seleção de objetos (inclui TEXT, MTEXT, INSERT para blocos de atributos, e MULTILEADER)
  (princ "\nSelecione os textos e blocos de atributos a serem analisados [ou Enter para todo o desenho]: ")
  (setq ss (ssget '((0 . "TEXT,MTEXT,INSERT,MULTILEADER"))))
  (if (not ss)
    (progn
      (princ (strcat "\nNenhuma seleção manual feita. Selecionando todos os objetos no espaço '" active-layout "'..."))
      (setq ss (ssget "_X" (list '(0 . "TEXT,MTEXT,INSERT,MULTILEADER") (cons 410 active-layout))))
    )
  )
  
  (if ss
    (progn
      ;; 2. Parâmetros de tolerância e comparação
      ;; Calcula a altura média dos textos reais (TEXT/MTEXT) selecionados
      (setq total-height 0.0)
      (setq height-count 0)
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (setq dxf (entget ent))
        (setq h (cdr (assoc 40 dxf)))
        (if (and h (> h 0.0))
          (progn
            (setq total-height (+ total-height h))
            (setq height-count (1+ height-count))
          )
        )
        (setq i (1+ i))
      )
      (if (> height-count 0)
        (setq avg-height (/ total-height height-count))
        (setq avg-height (getvar "TEXTSIZE"))
      )
      
      ;; Sugere uma tolerância de 120% da altura média para pegar textos deslocados/sombras
      (setq def-tol (* avg-height 1.2))
      (if (< def-tol 0.001) (setq def-tol 0.1))
      
      (princ (strcat "\n-> Altura média dos textos detectados: " (rtos avg-height 2 4)))
      (setq tol (getdist (strcat "\nDigite a distância de tolerância para proximidade <" (rtos def-tol 2 4) ">: ")))
      (if (not tol) (setq tol def-tol))
      
      (initget "Sim Não Yes No")
      (setq compContent (getkword "\nComparar conteúdo do texto? (Sim = apenas idênticos / Não = qualquer texto sobreposto) [Sim/Não] <Sim>: "))
      (if (or (not compContent) (= compContent "Sim") (= compContent "Yes"))
        (setq compContent T)
        (setq compContent nil)
      )
      
      ;; 3. Funções auxiliares para leitura e limpeza de textos
      
      ;; Função para obter ponto de inserção padrão (código 10)
      (defun get-text-pt (ent)
        (cdr (assoc 10 (entget ent)))
      )
      
      ;; Função para obter o ponto central da Bounding Box do texto/bloco
      (defun get-text-center (ent / obj minpt maxpt min-list max-list err)
        (setq obj (vlax-ename->vla-object ent))
        (setq err (vl-catch-all-apply '(lambda () (vla-getboundingbox obj 'minpt 'maxpt))))
        (if (and (not (vl-catch-all-error-p err)) minpt maxpt)
          (progn
            (setq min-list (vlax-safearray->list minpt))
            (setq max-list (vlax-safearray->list maxpt))
            (list
              (* 0.5 (+ (car min-list) (car max-list)))
              (* 0.5 (+ (cadr min-list) (cadr max-list)))
              (* 0.5 (+ (caddr min-list) (caddr max-list)))
            )
          )
          (get-text-pt ent)
        )
      )
      
      ;; Função para extrair atributos de blocos (INSERT)
      (defun get-block-attributes-string (ent / sub text dxf val)
        (setq sub (entnext ent))
        (setq text "")
        (while (and sub (= (cdr (assoc 0 (setq dxf (entget sub)))) "ATTRIB"))
          (setq val (cdr (assoc 1 dxf)))
          (if val
            (setq text (strcat text " " val))
          )
          (setq sub (entnext sub))
        )
        text
      )
      
      ;; Função para obter a string de texto do elemento
      (defun get-text-string (ent / dxf type text vlaObj err)
        (setq dxf (entget ent))
        (setq type (cdr (assoc 0 dxf)))
        (cond
          ((= type "MTEXT")
           (setq text "")
           (foreach item dxf
             (if (or (= (car item) 3) (= (car item) 1))
               (setq text (strcat text (cdr item)))
             )
           )
           text
          )
          ((= type "TEXT")
           (cdr (assoc 1 dxf))
          )
          ((= type "INSERT")
           (if (= (cdr (assoc 66 dxf)) 1)
             (get-block-attributes-string ent)
             nil
           )
          )
          ((= type "MULTILEADER")
           (setq vlaObj (vlax-ename->vla-object ent))
           (setq err (vl-catch-all-apply '(lambda () (vlax-get vlaObj 'TextString))))
           (if (vl-catch-all-error-p err)
             ""
             err
           )
          )
          (t nil)
        )
      )
      
      ;; Função para remover formatação de MTEXT (ex: \A1;, \C7;, \H2;, \P, brackets {}, etc.)
      (defun strip-mtext-formatting (str / len res i char next-char temp-i found-semi)
        (setq str (vl-string-translate "{}" "  " str))
        (setq len (strlen str))
        (setq res "")
        (setq i 1)
        (while (<= i len)
          (setq char (substr str i 1))
          (if (= char "\\")
            (progn
              (setq next-char (substr str (1+ i) 1))
              (cond
                ((member next-char '("P" "p" "L" "l" "O" "o" "A" "a" "X" "x" "~"))
                 (setq i (+ i 2))
                )
                ((member next-char '("C" "c" "H" "h" "F" "f" "S" "s" "T" "t" "Q" "q" "W" "w" "U" "u"))
                 (setq temp-i (+ i 2))
                 (setq found-semi nil)
                 (while (and (<= temp-i len) (not found-semi))
                   (if (= (substr str temp-i 1) ";")
                     (setq found-semi temp-i)
                     (setq temp-i (1+ temp-i))
                   )
                 )
                 (if found-semi
                   (setq i (1+ found-semi))
                   (setq i (+ i 2))
                 )
                )
                (t
                 (setq res (strcat res next-char))
                 (setq i (+ i 2))
                )
              )
            )
            (progn
              (setq res (strcat res char))
              (setq i (1+ i))
            )
          )
        )
        (vl-string-trim " \t\r\n" res)
      )
      
      ;; Substitui sequências unicode comuns do AutoCAD (ex: \U+00D3 por O, etc.)
      (defun replace-unicode-escapes (str / unicode-map)
        (setq unicode-map
          '(("\\U+00C1" . "A") ("\\U+00E1" . "A")
            ("\\U+00C0" . "A") ("\\U+00E0" . "A")
            ("\\U+00C2" . "A") ("\\U+00E2" . "A")
            ("\\U+00C3" . "A") ("\\U+00E3" . "A")
            ("\\U+00C9" . "E") ("\\U+00E9" . "E")
            ("\\U+00CA" . "E") ("\\U+00EA" . "E")
            ("\\U+00CD" . "I") ("\\U+00ED" . "I")
            ("\\U+00D3" . "O") ("\\U+00F3" . "O")
            ("\\U+00D4" . "O") ("\\U+00F4" . "O")
            ("\\U+00D5" . "O") ("\\U+00F5" . "O")
            ("\\U+00DA" . "U") ("\\U+00FA" . "U")
            ("\\U+00DC" . "U") ("\\U+00FC" . "U")
            ("\\U+00C7" . "C") ("\\U+00E7" . "C")
            ("\\U+00B2" . "2")
           )
        )
        (foreach pair unicode-map
          (setq str (vl-string-subst (cdr pair) (car pair) str))
          (while (vl-string-search (car pair) str)
            (setq str (vl-string-subst (cdr pair) (car pair) str))
          )
        )
        str
      )
      
      ;; Remove acentos normais em português para comparação
      (defun strip-accents (str / from to i char pos res)
        (setq from "ÁÀÂÃÉÊÍÓÔÕÚÜÇáàâãéêíóôõúüç")
        (setq to   "AAAAEEIOOOUUCAAAAEEIOOOUUC")
        (setq res "")
        (setq i 1)
        (while (<= i (strlen str))
          (setq char (substr str i 1))
          (setq pos (vl-string-search char from))
          (if pos
            (setq res (strcat res (substr to (1+ pos) 1)))
            (setq res (strcat res char))
          )
          (setq i (1+ i))
        )
        res
      )
      
      ;; Normaliza múltiplos espaços consecutivos para um único espaço
      (defun normalize-spaces (str / res i len char last-char)
        (setq len (strlen str))
        (setq res "")
        (setq i 1)
        (setq last-char "")
        (while (<= i len)
          (setq char (substr str i 1))
          (if (not (and (= char " ") (= last-char " ")))
            (setq res (strcat res char))
          )
          (setq last-char char)
          (setq i (1+ i))
        )
        res
      )
      
      ;; Função para limpar texto para comparação (maiúsculas, sem acentos, sem unicode-escapes, sem espaços múltiplos)
      (defun clean-text (str)
        (vl-string-trim " \t\r\n" 
          (normalize-spaces 
            (strip-accents 
              (replace-unicode-escapes 
                (strcase (strip-mtext-formatting str))
              )
            )
          )
        )
      )
      
      ;; 4. Carregar elementos em uma lista estruturada
      (princ "\nProcessando coordenadas e conteúdo dos textos/atributos...")
      (setq lst nil)
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (setq str (get-text-string ent))
        (if (and str (/= str ""))
          (progn
            (setq pt (get-text-center ent))
            (if pt
              (setq lst (cons (list ent pt (clean-text str)) lst))
            )
          )
        )
        (setq i (1+ i))
      )
      
      (princ (strcat "\nCarregado(s) " (itoa (length lst)) " texto(s)/bloco(s) válido(s) na memória."))
      (princ "\nOrdenando elementos por coordenada X...")
      
      ;; 5. Ordenação global por coordenada X para varredura rápida
      (setq lst (vl-sort lst 
        '(lambda (a b)
           (if (/= (car (cadr a)) (car (cadr b)))
             (< (car (cadr a)) (car (cadr b)))
             (< (cadr (cadr a)) (cadr (cadr b)))
           )
         )
      ))
      
      (princ "\nBuscando e removendo duplicados...")
      
      ;; 6. Varredura e exclusão dos textos sobrepostos
      (setq del-count 0)
      (setq debug-count 0)
      (setq temp lst)
      (while temp
        (setq item (car temp))
        (setq ent (car item))
        (setq pt (cadr item))
        (setq str (caddr item))
        
        ;; Se a entidade principal ainda existe (não foi deletada)
        (if (entget ent)
          (progn
            (setq rest (cdr temp))
            (setq continue T)
            (while (and rest continue)
              (setq sub-item (car rest))
              (setq sub-ent (car sub-item))
              (setq sub-pt (cadr sub-item))
              (setq sub-str (caddr sub-item))
              
              ;; Como a lista está ordenada por X, se a diferença no eixo X for maior que a tolerância,
              ;; podemos parar a busca para o texto atual.
              (if (> (- (car sub-pt) (car pt)) tol)
                (setq continue nil)
                (progn
                  ;; Se o sub-elemento ainda existe
                  (if (entget sub-ent)
                    (progn
                      ;; Distância 2D (ignora coordenada Z)
                      (setq dist (distance (list (car pt) (cadr pt) 0.0)
                                           (list (car sub-pt) (cadr sub-pt) 0.0)))
                      (if (<= dist tol)
                        (progn
                          ;; Se devemos comparar conteúdo
                          (if compContent
                            (if (= str sub-str)
                              ;; Conteúdo igual: deletar
                              (if (not (vl-catch-all-error-p (vl-catch-all-apply 'entdel (list sub-ent))))
                                (setq del-count (1+ del-count))
                              )
                              ;; Conteúdo diferente no mesmo ponto: logar para diagnóstico (limite de 15 logs)
                              (if (< debug-count 15)
                                (progn
                                  (princ (strcat "\n[Debug] Objetos próximos com conteúdo diferente: \"" str "\" vs \"" sub-str "\""))
                                  (setq debug-count (1+ debug-count))
                                )
                              )
                            )
                            ;; Sem comparar conteúdo: deletar direto
                            (if (not (vl-catch-all-error-p (vl-catch-all-apply 'entdel (list sub-ent))))
                              (setq del-count (1+ del-count))
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
              (setq rest (cdr rest))
            )
          )
        )
        (setq temp (cdr temp))
      )
      
      ;; 7. Mensagem final
      (princ (strcat "\nSucesso! Exclusão concluída. " (itoa del-count) " objeto(s) duplicado(s) removido(s)."))
      (if (and compContent (> debug-count 0))
        (princ "\nNota: Dicas de depuração ([Debug]) foram listadas acima se objetos próximos tinham conteúdos diferentes.")
      )
    )
    (princ (strcat "\nNenhum texto, mtext ou bloco com atributos encontrado no espaço '" active-layout "' ou na seleção."))
  )
  
  (vla-EndUndoMark doc)
  (setvar "CMDECHO" old-cmdecho)
  (princ)
)

(princ "\n=========================================================================")
(princ "\nRotina 'Excluir Textos Duplicados' carregada. Digite 'ETD' para executar.")
(princ "\n=========================================================================")
(princ)
