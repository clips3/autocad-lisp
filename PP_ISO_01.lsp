; --- Gira um GRUPO de objetos 45° (H/AH) pelo CENTRO automático ---
(defun c:GIRARCENTRO (/ *error* ss i len en vla-obj dir ang_deg obj_min obj_max 
                      obj_min_list obj_max_list minpt maxpt bp err
                     ) 

  ; Carrega as funções ActiveX/VLA, se necessário
  (vl-load-com)

  ; --- 1. Obter Direção da Rotação ---
  (initget "Anti-horario Horario")
  (setq dir (getkword "\nSelecione a direção da rotação [Anti-horario/Horario] <Anti-horario>: "))
  (if (not dir) (setq dir "Anti-horario"))

  (if (= dir "Horario") 
    (setq ang_deg -45.0) ; Ângulo negativo para Horário
    (setq ang_deg 45.0) ; Ângulo positivo para Anti-horário
  )

  ; --- 2. Selecionar Objetos ---
  (prompt "\nSelecione os objetos para girar pelo centro do grupo: ")
  (setq ss (ssget))

  ; --- 3. Processar Seleção ---
  (if ss 
    (progn 
      (setq len   (sslength ss)
            i     0 ; Inicializa os pontos mínimos e máximos com valores "infinitos"
            minpt (list 1e99 1e99 1e99)
            maxpt (list -1e99 -1e99 -1e99)
      )

      ; --- 4. Encontrar a Bounding Box (Caixa Delimitadora) do Grupo ---
      (while (< i len) 
        (setq en (ssname ss i))
        (setq vla-obj (vlax-ename->vla-object en))

        ; Tenta obter a bounding box do objeto
        ; Usa 'vl-catch' para evitar erros em objetos sem geometria (ex: atributos)
        (setq err (vl-catch-all-error-p 
                    (vl-catch-all-apply 
                      'vla-getboundingbox
                      (list vla-obj 'obj_min 'obj_max)
                    )
                  )
        )

        (if (not err)  ; Se obteve a bounding box com sucesso
          (progn 
            (setq obj_min_list (vlax-safearray->list obj_min))
            (setq obj_max_list (vlax-safearray->list obj_max))

            ; Atualiza os pontos mínimos e máximos GLOBAIS
            (setq minpt (mapcar 'min minpt obj_min_list))
            (setq maxpt (mapcar 'max maxpt obj_max_list))
          )
        )
        (setq i (1+ i))
      ) ; fim while

      ; --- 5. Calcular o Centro e Girar ---
      ; Verifica se 'minpt' foi alterado (se encontrou geometria válida)
      (if (< (car minpt) 1e98) 
        (progn 
          ; Calcula o ponto base (bp) como o centro da Bounding Box total
          (setq bp (mapcar '(lambda (a b) (/ (+ a b) 2.0)) minpt maxpt))

          ; Executa a rotação
          (setvar "CMDECHO" 0)
          (command "_.ROTATE" ss "" "_non" bp ang_deg)
          (setvar "CMDECHO" 1)

          (prompt 
            (strcat "\n" 
                    (itoa len)
                    " objetos girados "
                    dir
                    " ao redor do centro do grupo."
            )
          )
        )
        (prompt "\nNão foi possível calcular o centro. Nenhuma geometria válida encontrada.")
      )
    )
    (prompt "\nNenhum objeto selecionado.")
  )
  (princ) ; Saída limpa
)

(prompt "\nRotina LISP 'GIRARCENTRO' carregada. Digite GIRARCENTRO para executar.")
(princ)