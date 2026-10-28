;;; ============================================================
;;; DET.lsp - Detalhe com EXTRIM
;;; Comando: DET
;;; Descrição: Permite ao usuário desenhar um retângulo,
;;;            copiar objetos internos e aplicar XCLIP.
;;; ============================================================

(vl-load-com)

(defun c:DET (/ pt1 pt2 pt3 pt4 osm cmdecho ss_objs pt_dest ent_rect blkname last_ent 
              ent ss_blk ent_rect_new blk_ref clayer_old obj_rect
             ) 
  ;; Salvar variáveis de sistema
  (setq osm (getvar "OSMODE"))
  (setq cmdecho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)

  ;; --- Etapa 1: Desenhar o retângulo ---
  (setvar "OSMODE" 0)
  (setq pt1 (getpoint "\nClique no primeiro canto do retângulo: "))
  (if pt1 
    (progn 
      (setq pt2 (getcorner pt1 "\nClique no canto oposto do retângulo: "))
      (if pt2 
        (progn 
          ;; Calcular os 4 cantos do retângulo (para desenhar polyline)
          (setq pt3 (list (car pt2) (cadr pt1) 0.0))
          (setq pt4 (list (car pt1) (cadr pt2) 0.0))
          (setq pt1 (list (car pt1) (cadr pt1) 0.0))
          (setq pt2 (list (car pt2) (cadr pt2) 0.0))

          ;; Garantir que a layer PP_REGIAO existe
          (if (not (tblsearch "LAYER" "PP_REGIÃO")) 
            (command "_.LAYER" "_N" "PP_REGIÃO" "")
          )

          ;; Salvar layer atual e mudar para PP_REGIAO
          (setq clayer_old (getvar "CLAYER"))
          (setvar "CLAYER" "PP_REGIÃO")

          ;; Criar a polyline retangular (fechada) - será criada na layer PP_REGIAO
          (command "_.PLINE" pt1 "_W" "0.01" "0.01" pt3 pt2 pt4 "_C")
          (setq ent_rect (entlast))

          ;; Restaurar layer anterior
          (setvar "CLAYER" clayer_old)

          ;; --- Etapa 2: Selecionar Objetos e Copiar ---
          (setq ss_objs (ssget "_C" pt1 pt2))
          (if (not ss_objs) (setq ss_objs (ssadd)))

          ;; Remover o próprio retângulo da seleção de objetos (será tratado separadamente)
          (if (ssmemb ent_rect ss_objs) (ssdel ent_rect ss_objs))

          ;; Restaurar OSMODE para o usuário escolher o destino visualmente
          (setvar "OSMODE" osm)

          (setq pt_dest (getpoint pt1 "\nClique onde deseja colar o detalhe: "))

          (if pt_dest 
            (progn 
              (setvar "OSMODE" 0)

              ;; 1. Copiar Objetos (sem o retângulo) para o destino
              (setq last_ent (entlast)) ;; Marcar último objeto antes da cópia
              (if (> (sslength ss_objs) 0) 
                (command "_.COPY" ss_objs "" pt1 pt_dest)
              )

              ;; Coletar novos objetos criados em um Selection Set (ss_blk)
              (setq ss_blk (ssadd))
              (setq ent (entnext last_ent))
              (while ent 
                (ssadd ent ss_blk)
                (setq ent (entnext ent))
              )

              ;; 2. Copiar o Retângulo separadamente (para ter referência fácil)
              (command "_.COPY" ent_rect "" pt1 pt_dest)
              (setq ent_rect_new (entlast))

              ;; 3. Criar Bloco dos objetos copiados (se houver objetos)
              (if (> (sslength ss_blk) 0) 
                (progn 
                  ;; Nome único para o bloco baseado na data/hora
                  (setq blkname (strcat "DET_" (rtos (getvar "CDATE") 2 8)))
                  ;; O comando BLOCK consome os objetos do ss_blk
                  (command "_.BLOCK" blkname pt_dest ss_blk "")

                  ;; Inserir o bloco criado
                  (command "_.INSERT" blkname pt_dest "1" "1" "0")
                  (setq blk_ref (entlast))

                  ;; Garantir que o bloco fique na layer 0
                  (command "_.CHPROP" blk_ref "" "_LA" "0" "")

                  ;; 4. Aplicar XCLIP (CLIP) usando o retângulo copiado como fronteira
                  (command "_.XCLIP" blk_ref "" "_N" "_S" ent_rect_new)

                  ;; 5. Criar copia temporaria sem largura para o wipeout
                  (command "_.COPY" ent_rect_new "" "0,0" "0,0")
                  (setq temp_pline (entlast))
                  (command "_.PEDIT" temp_pline "_W" "0" "")
                  ;; Criar wipeout a partir da copia temporaria (deletar a copia)
                  (command "_.WIPEOUT" "_P" temp_pline "_Y")
                  (setq wipeout_ent (entlast))

                  ;; 6. Draw order com hierarquia: wipeout (tras) < bloco (meio) < polyline (frente)
                  (command "_.DRAWORDER" wipeout_ent "" "_F")
                  (command "_.DRAWORDER" blk_ref "" "_F")
                  (command "_.DRAWORDER" ent_rect_new "" "_F")
                )
              )
            )
          )

          ;; Restaurar variáveis
          (setvar "OSMODE" osm)
          (setvar "CMDECHO" cmdecho)
        ) ; progn pt2
      ) ; if pt2
    ) ; progn pt1
  ) ; if pt1

  (princ)
) ; defun

(princ "\nComando DET carregado. Digite DET para executar.")
(princ)
