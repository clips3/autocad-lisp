;;; ==========================================================================
;;; Comando: S2H (Solid To Hatch - BB_SOLIDTOHATCH)
;;; Descrição: Transforma entidades 2D do tipo SOLID em HACHURAS (Hatch) de
;;;            padrão sólido, mantendo a camada (layer) e a cor original.
;;; ==========================================================================

(vl-load-com)

(defun c:S2H ( / ss i ent edata ext p1 p2 p3 p4 pl newhatch vlaSolid vlaHatch oldCmd oldOsmode *error* convertCount)
  
  ;; Função de tratamento de erro para garantir a restauração das variáveis de sistema
  (defun *error* (msg)
    (if oldCmd (setvar "CMDECHO" oldCmd))
    (if oldOsmode (setvar "OSMODE" oldOsmode))
    (if (not (wcmatch (strcase msg t) "*BREAK*,*CANCEL*,*EXIT*"))
      (princ (strcat "\nErro: " msg))
    )
    (princ)
  )

  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))

  (setq oldCmd (getvar "CMDECHO"))
  (setq oldOsmode (getvar "OSMODE"))
  (setvar "CMDECHO" 0)
  (setvar "OSMODE" 0)
  
  (princ "\nSelecione os elementos 2D SOLID para converter em Hachura: ")
  ;; Filtra apenas seleções do tipo SOLID
  (if (setq ss (ssget '((0 . "SOLID"))))
    (progn
      (setq convertCount 0)
      (setq i 0)
      
      ;; Inicia um grupo de desfazer (Undo) para reverter tudo em um passo se necessário
      (vla-StartUndoMark doc)
      
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (setq edata (entget ent))
        (setq vlaSolid (vlax-ename->vla-object ent))
        
        ;; Pega o dono (ModelSpace, PaperSpace ou Block) seguro via DXF 330
        (setq space (vlax-ename->vla-object (cdr (assoc 330 edata))))
        
        ;; Pega os pontos em OCS do SOLID
        (setq p1_dxf (cdr (assoc 10 edata)))
        (setq p2_dxf (cdr (assoc 11 edata)))
        (setq p3_dxf (cdr (assoc 12 edata)))
        (setq p4_dxf (cdr (assoc 13 edata)))
        
        (setq elev (caddr p1_dxf))
        
        ;; Constrói a array de pontos 2D para a polilinha (corrigindo o cruzamento de P3 e P4)
        (if (equal p3_dxf p4_dxf 1e-6)
          (progn
            (setq ptsArray (vlax-make-safearray vlax-vbDouble '(0 . 5)))
            (vlax-safearray-fill ptsArray (list (car p1_dxf) (cadr p1_dxf)  (car p2_dxf) (cadr p2_dxf)  (car p3_dxf) (cadr p3_dxf)))
          )
          (progn
            (setq ptsArray (vlax-make-safearray vlax-vbDouble '(0 . 7)))
            (vlax-safearray-fill ptsArray (list (car p1_dxf) (cadr p1_dxf)  (car p2_dxf) (cadr p2_dxf)  (car p4_dxf) (cadr p4_dxf)  (car p3_dxf) (cadr p3_dxf)))
          )
        )
        
        ;; Cria a polilinha leve e Hachura Sólida em memória via ActiveX (Muito mais rápido que comandos)
        (setq plineObj (vl-catch-all-apply 'vla-AddLightWeightPolyline (list space ptsArray)))
        
        (if (not (vl-catch-all-error-p plineObj))
          (progn
            (vla-put-Closed plineObj :vlax-true)
            (vla-put-Elevation plineObj elev)
            (vl-catch-all-apply 'vla-put-Normal (list plineObj (vla-get-Normal vlaSolid)))
            
            (setq hatchObj (vl-catch-all-apply 'vla-AddHatch (list space 1 "SOLID" :vlax-false)))
            
            (if (not (vl-catch-all-error-p hatchObj))
              (progn
                ;; Anexa a polilinha como limite da hachura
                (setq loopArray (vlax-make-safearray vlax-vbObject '(0 . 0)))
                (vlax-safearray-put-element loopArray 0 plineObj)
                
                (vl-catch-all-apply 'vla-AppendOuterLoop (list hatchObj loopArray))
                (vl-catch-all-apply 'vla-put-Elevation (list hatchObj elev))
                (vl-catch-all-apply 'vla-put-Normal (list hatchObj (vla-get-Normal vlaSolid)))
                (vl-catch-all-apply 'vla-Evaluate (list hatchObj))
                
                ;; Copia todas propriedades visuais necessárias (ActiveX)
                (vla-put-Layer hatchObj (vla-get-Layer vlaSolid))
                (vl-catch-all-apply 'vla-put-TrueColor (list hatchObj (vla-get-TrueColor vlaSolid)))
                (vl-catch-all-apply 'vla-put-Linetype (list hatchObj (vla-get-Linetype vlaSolid)))
                (vl-catch-all-apply 'vla-put-LinetypeScale (list hatchObj (vla-get-LinetypeScale vlaSolid)))
                (vl-catch-all-apply 'vla-put-Lineweight (list hatchObj (vla-get-Lineweight vlaSolid)))
                
                ;; Apaga objetos originais
                (vl-catch-all-apply 'vla-Delete (list vlaSolid))
                (setq convertCount (1+ convertCount))
              )
            )
            ;; Apaga o perímetro temporário (pline)
            (vl-catch-all-apply 'vla-Delete (list plineObj))
          )
        )
        
        (setq i (1+ i))
      )
      
      (vla-EndUndoMark doc)
      
      (princ (strcat "\n" (itoa convertCount) " sólidos " (if (= convertCount 1) "foi convertido" "foram convertidos") " de forma ultrarrápida (ActiveX) em hachuras."))
    )
    (princ "\nNenhum elemento SOLID foi selecionado.")
  )
  
  ;; Restaura configurações de sistema originais
  (setvar "CMDECHO" oldCmd)
  (setvar "OSMODE" oldOsmode)
  (princ)
)

(princ "\nArquivo BB_SOLIDTOHATCH carregado. Digite S2H para usar o comando.")
(princ)
