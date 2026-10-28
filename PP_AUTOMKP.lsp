(defun get-visual-center (blkRef / blkName blkDef minPt maxPt ent minE maxE found 
                          GeomMin GeomMax centerLocal insPt ang scaleX scaleY scaleZ x 
                          y z xRot yRot finalX finalY finalZ
                         ) 
  (vl-load-com)

  ;; 1. Get block definition
  (setq blkName (vla-get-name blkRef))
  (setq blkDef (vla-item (vla-get-blocks (vla-get-document blkRef)) blkName))

  (setq found nil)

  ;; 2. Get BoundingBox of GEOMETRY only
  (vlax-for ent blkDef 
    (if 
      (member (vla-get-objectname ent) 
              '("AcDbLine" "AcDbPolyline" "AcDb2dPolyline" "AcDbLwPolyline" 
                "AcDbCircle" "AcDbArc" "AcDbEllipse" "AcDbSpline" "AcDbHatch" 
                "AcDbSolid"
               )
      )
      (progn 
        (if 
          (not 
            (vl-catch-all-error-p 
              (vl-catch-all-apply 'vla-getboundingbox (list ent 'minE 'maxE))
            )
          )
          (progn 
            (setq minE (vlax-safearray->list minE))
            (setq maxE (vlax-safearray->list maxE))

            (if (not found) 
              (setq GeomMin minE
                    GeomMax maxE
                    found   T
              )
              (setq GeomMin (mapcar 'min GeomMin minE)
                    GeomMax (mapcar 'max GeomMax maxE)
              )
            )
          )
        )
      )
    )
  )

  ;; 3. Fallback to general bounding box if no geometry found
  (if (not found) 
    (progn 
      (vla-getboundingbox blkRef 'minE 'maxE)
      (setq minE (vlax-safearray->list minE)
            maxE (vlax-safearray->list maxE)
      )
      (list (/ (+ (car minE) (car maxE)) 2.0) 
            (/ (+ (cadr minE) (cadr maxE)) 2.0)
            (/ (+ (caddr minE) (caddr maxE)) 2.0)
      )
    )
    ;; 4. Calculate local center and transform to World
    (progn 
      (setq centerLocal (list 
                          (/ (+ (car GeomMin) (car GeomMax)) 2.0)
                          (/ (+ (cadr GeomMin) (cadr GeomMax)) 2.0)
                          (/ (+ (caddr GeomMin) (caddr GeomMax)) 2.0)
                        )
      )

      ;; Transformation data
      (setq insPt (vlax-get blkRef 'InsertionPoint))
      (setq ang (vla-get-Rotation blkRef))
      (setq scaleX (vla-get-XScaleFactor blkRef))
      (setq scaleY (vla-get-YScaleFactor blkRef))
      (setq scaleZ (vla-get-ZScaleFactor blkRef))

      ;; Local coords
      (setq x (car centerLocal)
            y (cadr centerLocal)
            z (caddr centerLocal)
      )

      ;; Scale
      (setq x (* x scaleX)
            y (* y scaleY)
            z (* z scaleZ)
      )

      ;; Rotation (2D XY)
      (setq xRot (- (* x (cos ang)) (* y (sin ang))))
      (setq yRot (+ (* x (sin ang)) (* y (cos ang))))

      ;; Translation
      (setq finalX (+ xRot (car insPt)))
      (setq finalY (+ yRot (cadr insPt)))
      (setq finalZ (+ z (caddr insPt)))

      (list finalX finalY finalZ)
    )
  )
)

;; Function to get dynamic property value (wildcard match)
(defun get-dynamic-prop-match (blk pattern / props val) 
  (setq val nil)
  (if (and blk (= (vla-get-isdynamicblock blk) :vlax-true)) 
    (progn 
      (setq props (vlax-invoke blk 'GetDynamicBlockProperties))
      (foreach p props 
        (if (wcmatch (strcase (vla-get-propertyname p)) (strcase pattern)) 
          (setq val (vlax-get p 'Value))
        )
      )
    )
  )
  val
)

;; Function to get attribute value (wildcard match on tag)
(defun get-attribute-value (blk tag / atts val) 
  (setq val nil)
  (if (and blk (= (vla-get-hasattributes blk) :vlax-true)) 
    (progn 
      (setq atts (vla-getattributes blk))
      (setq atts (vlax-safearray->list (vlax-variant-value atts)))
      (foreach att atts 
        (if (wcmatch (strcase (vla-get-tagstring att)) (strcase tag)) 
          (setq val (vl-string-trim " \t\n\r" (vla-get-textstring att)))
        )
      )
    )
  )
  (if (and val (= val "")) (setq val nil))
  val
)

;; Helper: Check if two rectangles overlap
;; Rect format: (MinX MinY MaxX MaxY)
(defun check-box-overlap (r1 r2 /) 
  (not 
    (or 
      (< (caddr r1) (car r2)) ; r1.MaxX < r2.MinX (r1 is left of r2)
      (> (car r1) (caddr r2)) ; r1.MinX > r2.MaxX (r1 is right of r2)
      (< (cadddr r1) (cadr r2)) ; r1.MaxY < r2.MinY (r1 is below r2)
      (> (cadr r1) (cadddr r2)) ; r1.MinY > r2.MaxY (r1 is above r2)
    )
  )
)

;; Helper: rtos with guaranteed leading zero (e.g. ".20" -> "0.20")
(defun rtos-lz (val mode prec / str) 
  (setq str (rtos val mode prec))
  (if (= (substr str 1 1) ".") 
    (strcat "0" str)
    str
  )
)

;; Helper: Formatar valor de dimensao de forma inteligente (metros, cm ou mm)
(defun format-dim-value (val / num) 
  (setq num (cond 
              ((numberp val) val)
              ((and (= (type val) 'STR) (distof val)) (distof val))
              (t nil)
            )
  )
  (if num 
    (cond 
      ((< num 1.0)  (rtos-lz (* num 100.0) 2 1)) ; metros (ex: 0.10 -> 10.0)
      ((> num 50.0) (rtos-lz (/ num 10.0) 2 1))  ; mm (ex: 100.0 -> 10.0)
      (t            (rtos-lz num 2 1))           ; cm (ex: 10.0 -> 10.0)
    )
    (if (and val (/= val "")) (vl-princ-to-string val) "10.0")
  )
)

;; Helper: Formatar texto de altura para o atributo
(defun get-altura-text (altu /) 
  (cond 
    ((numberp altu)
     (strcat "h= " (format-dim-value altu) "cm")
    )
    ((and (= (type altu) 'STR) (distof altu))
     (strcat "h= " (format-dim-value altu) "cm")
    )
    ((and (= (type altu) 'STR) (/= altu ""))
     (if (wcmatch (strcase altu) "H=*")
       altu
       (strcat "h= " altu)
     )
    )
    (t "")
  )
)

;; Helper: Atribuir valor de atributo em Multileader com Bloco (ActiveX)
(defun set-mleader-attrib (mleadObj tag val / doc blks blkName blkDef attId)
  (setq doc (vla-get-Document mleadObj))
  (setq blks (vla-get-Blocks doc))
  (setq blkName (vl-catch-all-apply 'vla-get-ContentBlockName (list mleadObj)))
  (if (and (not (vl-catch-all-error-p blkName)) blkName (/= blkName ""))
    (if (not (vl-catch-all-error-p (setq blkDef (vl-catch-all-apply 'vla-Item (list blks blkName)))))
      (vlax-for ent blkDef
        (if (and (= (vla-get-ObjectName ent) "AcDbAttributeDefinition")
                 (wcmatch (strcase (vla-get-TagString ent)) (strcase (strcat tag "*"))))
          (progn
            (setq attId (vla-get-ObjectID ent))
            (vl-catch-all-apply 'vla-SetBlockAttributeValue (list mleadObj attId (if val val "")))
          )
        )
      )
    )
  )
)

;; Helper: Ler valor de atributo de um Multileader com Bloco (ActiveX)
(defun get-mleader-attrib (mleadObj tag / doc blks blkName blkDef attId res)
  (setq doc (vla-get-Document mleadObj))
  (setq blks (vla-get-Blocks doc))
  (setq blkName (vl-catch-all-apply 'vla-get-ContentBlockName (list mleadObj)))
  (setq res "")
  (if (and (not (vl-catch-all-error-p blkName)) blkName (/= blkName ""))
    (if (not (vl-catch-all-error-p (setq blkDef (vl-catch-all-apply 'vla-Item (list blks blkName)))))
      (vlax-for ent blkDef
        (if (and (= (vla-get-ObjectName ent) "AcDbAttributeDefinition")
                 (wcmatch (strcase (vla-get-TagString ent)) (strcase (strcat tag "*"))))
          (progn
            (setq attId (vla-get-ObjectID ent))
            (setq res (vl-catch-all-apply 'vla-GetBlockAttributeValue (list mleadObj attId)))
            (if (vl-catch-all-error-p res) (setq res "") res)
          )
        )
      )
    )
  )
  res
)

;; Caminhos de busca para arquivos de biblioteca de blocos (.dwg)
(setq *MKP-LIB-PATHS*
  (list
    "C:/Users/Henrique/Desktop/TEMP"
    "C:/Proplan/OneDrive - Proplan Engenharia/02. ENGENHARIA/02. BIBLIOTECA/01.MODELOS, PADRÕES E PROCESSOS/00.TEMPLATES/04.HVAC"
    "C:\\Proplan\\OneDrive - Proplan Engenharia\\02. ENGENHARIA\\02. BIBLIOTECA\\01.MODELOS, PADRÕES E PROCESSOS\\00.TEMPLATES\\04.HVAC"
  )
)

;; Helper: Ativar estilo de Multileader se existir no dicionario
(defun set-mleader-style-safe (styleName / mldDict)
  (setq mldDict (dictsearch (namedobjdict) "ACAD_MLEADERSTYLE"))
  (if (and mldDict (dictsearch (cdr (assoc -1 mldDict)) styleName))
    (progn
      (setvar "CMLEADERSTYLE" styleName)
      T
    )
    nil
  )
)

;; Helper: Localizar arquivo .dwg nos caminhos locais, de rede ou de suporte
(defun find-dwg-file (baseName / candidatePaths foundPath lspDir)
  (setq lspDir (if (findfile "PP_AUTOMKP.lsp") 
                 (vl-filename-directory (findfile "PP_AUTOMKP.lsp")) 
                 "C:/Users/Henrique/Desktop/TEMP"
               )
  )
  (setq candidatePaths
    (list
      (strcat lspDir "/" baseName ".dwg")
      (strcat lspDir "/PP_" baseName ".dwg")
      (strcat "C:/Users/Henrique/Desktop/TEMP/" baseName ".dwg")
      (strcat "C:/Users/Henrique/Desktop/TEMP/PP_" baseName ".dwg")
      (strcat "C:/Proplan/OneDrive - Proplan Engenharia/02. ENGENHARIA/02. BIBLIOTECA/01.MODELOS, PADRÕES E PROCESSOS/00.TEMPLATES/04.HVAC/" baseName ".dwg")
      (strcat "C:/Proplan/OneDrive - Proplan Engenharia/02. ENGENHARIA/02. BIBLIOTECA/01.MODELOS, PADRÕES E PROCESSOS/00.TEMPLATES/04.HVAC/PP_" baseName ".dwg")
      (if (getvar "DWGPREFIX") (strcat (getvar "DWGPREFIX") baseName ".dwg") nil)
      (if (getvar "DWGPREFIX") (strcat (getvar "DWGPREFIX") "PP_" baseName ".dwg") nil)
    )
  )
  (setq foundPath nil)
  (foreach p candidatePaths
    (if (and (not foundPath) p (findfile p))
      (setq foundPath (findfile p))
    )
  )
  (if (not foundPath)
    (setq foundPath (or (findfile (strcat baseName ".dwg"))
                        (findfile (strcat "PP_" baseName ".dwg"))))
  )
  foundPath
)

;; Helper: Garantir que a definicao do bloco existe no desenho, importando via ActiveX
(defun ensure-block-loaded (targetBlkName / doc blks foundPath tempBlk cleanName)
  (vl-load-com)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq blks (vla-get-Blocks doc))

  ;; Se o bloco ja existe (ex: TAG_MKP_PADRAO ou PP_TAG_MKP_PADRAO), nao precisa importar
  (if (or (tblsearch "BLOCK" targetBlkName)
          (tblsearch "BLOCK" (strcat "PP_" targetBlkName))
          (and (wcmatch (strcase targetBlkName) "PP_*") (tblsearch "BLOCK" (substr targetBlkName 4))))
    T
    (progn
      (setq cleanName (if (wcmatch (strcase targetBlkName) "PP_*") (substr targetBlkName 4) targetBlkName))
      (setq foundPath (find-dwg-file cleanName))

      ;; Se encontrou o arquivo DWG, importa para o desenho via ActiveX sem dialogos
      (if (and foundPath (vl-file-size foundPath))
        (progn
          (setq tempBlk (vl-catch-all-apply 'vla-InsertBlock 
                          (list (vla-get-ModelSpace doc) 
                                (vlax-3d-point '(0.0 0.0 0.0)) 
                                foundPath 
                                1.0 1.0 1.0 0.0)))
          (if (and tempBlk (not (vl-catch-all-error-p tempBlk)))
            (progn
              (vl-catch-all-apply 'vla-Delete (list tempBlk))
              (princ (strcat "\n[BB_AUTOMKP] Bloco '" targetBlkName "' importado com sucesso de: " foundPath))
              T
            )
            (progn
              ;; Fallback com _.-INSERT
              (vl-cmdf "_.-INSERT" (strcat targetBlkName "=" foundPath) '(0 0 0) 1 1 0)
              (if (entlast) (entdel (entlast)))
              T
            )
          )
        )
        (progn
          (princ (strcat "\n[BB_AUTOMKP] AVISO: Bloco '" targetBlkName "' não encontrado no desenho nem na biblioteca."))
          nil
        )
      )
    )
  )
)

;; Helper: Obter o nome real do bloco carregado no desenho
(defun get-available-block-name (baseName / cleanName)
  (setq cleanName (if (wcmatch (strcase baseName) "PP_*") (substr baseName 4) baseName))
  (cond
    ((tblsearch "BLOCK" baseName) baseName)
    ((tblsearch "BLOCK" (strcat "PP_" cleanName)) (strcat "PP_" cleanName))
    ((tblsearch "BLOCK" cleanName) cleanName)
    (T baseName)
  )
)

;; Helper: Trazer objetos para a frente de forma 100% silenciosa via ActiveX (SortentsTable)
(defun send-to-front-silent (doc ssOrObjList / spc extDict sortTable objList i ent sa)
  (vl-load-com)
  (setq spc (if (= (getvar "CVPORT") 1) 
              (vla-get-PaperSpace doc)
              (vla-get-ModelSpace doc)
            )
  )
  (setq extDict (vla-GetExtensionDictionary spc))
  (setq sortTable (vl-catch-all-apply 'vla-GetObject (list extDict "ACAD_SORTENTS")))
  (if (or (vl-catch-all-error-p sortTable) (null sortTable))
    (setq sortTable (vl-catch-all-apply 'vla-AddObject (list extDict "ACAD_SORTENTS" "AcDbSortentsTable")))
  )
  (if (and sortTable (not (vl-catch-all-error-p sortTable)))
    (progn
      (setq objList '())
      (cond
        ((= (type ssOrObjList) 'PICKSET)
         (setq i 0)
         (repeat (sslength ssOrObjList)
           (setq ent (ssname ssOrObjList i))
           (if ent (setq objList (cons (vlax-ename->vla-object ent) objList)))
           (setq i (1+ i))
         )
        )
        ((listp ssOrObjList)
         (foreach item ssOrObjList
           (cond
             ((= (type item) 'ENAME) (setq objList (cons (vlax-ename->vla-object item) objList)))
             ((= (type item) 'VLA-OBJECT) (setq objList (cons item objList)))
           )
         )
        )
        ((= (type ssOrObjList) 'VLA-OBJECT)
         (setq objList (list ssOrObjList))
        )
      )
      (if objList
        (progn
          (setq sa (vlax-make-safearray vlax-vbObject (cons 0 (1- (length objList)))))
          (vlax-safearray-fill sa objList)
          (vl-catch-all-apply 'vla-MoveToTop (list sortTable (vlax-make-variant sa)))
        )
      )
    )
  )
)

;; Helper: Criar Multileader diretamente via ActiveX sem depender do comando interativo _.MLEADER
(defun create-mleader-direct (spc arrowPt textPt / ptList sa ptsVar mleadObj)
  (setq ptList (list (float (car arrowPt)) (float (cadr arrowPt)) (float (caddr arrowPt))
                     (float (car textPt))  (float (cadr textPt))  (float (caddr textPt))))
  (setq sa (vlax-make-safearray vlax-vbDouble '(0 . 5)))
  (vlax-safearray-fill sa ptList)
  (setq ptsVar (vlax-make-variant sa))
  (setq mleadObj (vl-catch-all-apply 'vla-AddMLeader (list spc ptsVar 0)))
  (if (vl-catch-all-error-p mleadObj)
    nil
    mleadObj
  )
)

;; Helper: Extrair TIPO, DIM e ALTURA de qualquer bloco de markup de forma universal
(defun extract-markup-info (obj effName blockLayer / isLaje strTipo strDim strAltu 
                                                      dynProps numProps pName pVal 
                                                      diam larg comp altu)
  (setq isLaje (= (strcase blockLayer) "PP_MKP_LAJE"))

  ;; 1. Determina TIPO
  (cond 
    ((= (strcase blockLayer) "PP_MKP_ALVENARIA") (setq strTipo "Furo em alvenaria"))
    ((= (strcase blockLayer) "PP_MKP_VIGA")      (setq strTipo "Furo em viga"))
    ((= (strcase blockLayer) "PP_MKP_LAJE")      (setq strTipo "Furo em laje"))
    (T                                           (setq strTipo "Furo"))
  )

  ;; 2. Coleta propriedades numericas do bloco dinamico
  (setq numProps '())
  (if (and obj (= (vla-get-isdynamicblock obj) :vlax-true))
    (progn
      (setq dynProps (vlax-invoke obj 'GetDynamicBlockProperties))
      (foreach p dynProps
        (setq pName (strcase (vla-get-propertyname p)))
        (setq pVal (vlax-get p 'Value))
        (if (and (numberp pVal) (> pVal 0.0) 
                 (not (wcmatch pName "*VISIBILIT*,*ORIGIN*,*LOOKUP*,*ALIGN*,*ROTATE*,*ANG*")))
          (if (not (wcmatch pName "*ALTURA*,*H*,*NIVEL*,*ELEV*"))
            (setq numProps (cons (cons pName pVal) numProps))
          )
        )
      )
    )
  )

  ;; 3. Determina ALTURA
  (setq altu (get-dynamic-prop-match obj "*ALTURA*"))
  (if (not altu) (setq altu (get-dynamic-prop-match obj "H*")))
  (if (not altu) (setq altu (get-dynamic-prop-match obj "*NIVEL*")))
  (if (not altu) (setq altu (get-attribute-value obj "*ALTURA*")))
  (if (not altu) (setq altu (get-attribute-value obj "H*")))
  (if (not altu) (setq altu (get-attribute-value obj "*NIVEL*")))
  (if (not isLaje)
    (setq strAltu (get-altura-text altu))
    (setq strAltu "")
  )

  ;; 4. Determina DIMENSOES
  (if (wcmatch (strcase effName) "*REDONDO*")
    ;; --- BLOCO REDONDO ---
    (progn
      (setq diam (get-dynamic-prop-match obj "*DI*METRO*"))
      (if (not diam) (setq diam (get-dynamic-prop-match obj "D*")))
      (if (not diam) (setq diam (get-dynamic-prop-match obj "*RAIO*")))
      (if (not diam) (setq diam (get-dynamic-prop-match obj "*DIM*")))
      (if (and (not diam) numProps) (setq diam (cdar numProps)))
      (if (not diam) (setq diam (get-attribute-value obj "*DI*METRO*")))
      (if (not diam) (setq diam (get-attribute-value obj "D*")))
      (if (not diam) (setq diam (get-attribute-value obj "*RAIO*")))
      (if (not diam) (setq diam 0.10))

      (setq strDim (strcat "%%C " (format-dim-value diam) "cm"))
    )
    ;; --- BLOCO RETANGULAR / QUADRADO ---
    (progn
      (setq larg (get-dynamic-prop-match obj "*LARGURA*"))
      (if (not larg) (setq larg (get-dynamic-prop-match obj "L*")))
      (if (not larg) (setq larg (get-dynamic-prop-match obj "*BASE*")))
      (if (and (not larg) numProps) (setq larg (cdar numProps)))
      (if (not larg) (setq larg (get-attribute-value obj "*LARGURA*")))
      (if (not larg) (setq larg (get-attribute-value obj "L*")))

      (setq comp (get-dynamic-prop-match obj "*COMPRIMENTO*"))
      (if (not comp) (setq comp (get-dynamic-prop-match obj "C*")))
      (if (not comp) (setq comp (get-dynamic-prop-match obj "*ALT*")))
      (if (and (not comp) (> (length numProps) 1)) (setq comp (cdadr numProps)))
      (if (not comp) (setq comp (get-attribute-value obj "*COMPRIMENTO*")))
      (if (not comp) (setq comp (get-attribute-value obj "C*")))

      (if (and larg comp)
        (setq strDim (strcat (format-dim-value larg) " x " (format-dim-value comp) "cm"))
        (if larg
          (setq strDim (strcat (format-dim-value larg) " x " (format-dim-value larg) "cm"))
          (setq strDim "10.0 x 10.0cm")
        )
      )
    )
  )

  (list strTipo strDim strAltu isLaje)
)

;;; ==========================================================================
;;;  COMANDO MKP: Inserir anotações com blocos TAG_MKP_PADRAO e TAG_MKP_LAJE
;;; ==========================================================================
(defun c:MKP (/ *error* doc spc oldOsMode oldEcho oldLayer oldAttReq oldAttDia 
              oldCmdDia oldMLeaderStyle ss ssFilter i ent obj effName count 
              foundBlocks item insPt textPt blockLayer mleaderObj 
              sortedBlocks occupiedRects rectW rectH candidatePt candidateRect 
              overlap retryCount maxRetries baseTextPt isLaje targetBlock 
              targetStyle strTipo strDim strAltu arrowPt info blks
              ssNewLeaders ssOrder dxf10
             ) 
  (vl-load-com)

  ;; 1. Setup Active X Objects
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq spc (if (= (getvar "CVPORT") 1) 
              (vla-get-PaperSpace doc)
              (vla-get-ModelSpace doc)
            )
  )

  ;; 2. Define Error Handler
  (defun *error* (msg) 
    (if (and msg (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*"))) 
      (princ (strcat "\nErro: " msg))
    )
    ;; Restore Variables
    (if oldOsMode (setvar "OSMODE" oldOsMode))
    (if oldEcho (setvar "CMDECHO" oldEcho))
    (if oldLayer (setvar "CLAYER" oldLayer))
    (if oldAttReq (setvar "ATTREQ" oldAttReq))
    (if oldAttDia (setvar "ATTDIA" oldAttDia))
    (if oldCmdDia (setvar "CMDDIA" oldCmdDia))
    (if oldMLeaderStyle (setvar "CMLEADERSTYLE" oldMLeaderStyle))
    (vla-EndUndoMark doc)
    (princ)
  )

  ;; 3. Main Execution Setup
  (vla-StartUndoMark doc)
  (setq oldOsMode (getvar "OSMODE"))
  (setq oldEcho (getvar "CMDECHO"))
  (setq oldLayer (getvar "CLAYER"))
  (setq oldAttReq (getvar "ATTREQ"))
  (setq oldAttDia (getvar "ATTDIA"))
  (setq oldCmdDia (getvar "CMDDIA"))
  (setq oldMLeaderStyle (getvar "CMLEADERSTYLE"))

  (setvar "CMDECHO" 0)
  (setvar "OSMODE" 0)   ; Desativa snaps durante o cálculo
  (setvar "ATTREQ" 0)   ; Inserção silenciosa de atributos
  (setvar "ATTDIA" 0)
  (setvar "CMDDIA" 0)
  (setq ssNewLeaders (ssadd))

  ;; 4. Validar e carregar automaticamente os blocos de anotação da biblioteca
  (ensure-block-loaded "TAG_MKP_PADRAO")
  (ensure-block-loaded "TAG_MKP_LAJE")

  ;; 5. Seleção dos blocos de markup
  (princ "\nSelecione os blocos de markup:")
  (setq ss (ssget '((0 . "INSERT") (410 . "Model"))))
  (setq ssFilter (ssadd))
  (setq count 0)
  (setq foundBlocks '())

  (if ss 
    (progn 
      (setq i 0)
      (repeat (sslength ss) 
        (setq ent (ssname ss i))
        (setq obj (vlax-ename->vla-object ent))

        ;; Get Effective Name (suporte a blocos dinâmicos)
        (setq effName (vla-get-effectivename obj))

        ;; Verifica se o bloco e um markup (Redondo, Quadrado, Retangular ou PP_MARKUP_*)
        (if (wcmatch (strcase effName) "*MARKUP*QUADRADO*,*MARKUP*RETANG*,*MARKUP*REDONDO*,PP_MARKUP_*")
          (progn 
            (ssadd ent ssFilter)
            (setq count (1+ count))

            ;; Ponto de insercao 2D garantido via DXF 10
            (setq dxf10 (cdr (assoc 10 (entget ent))))
            (setq insPt (list (car dxf10) (cadr dxf10) (if (caddr dxf10) (caddr dxf10) 0.0)))

            ;; Adiciona na lista: (VLA-OBJECT INS-POINT EFF-NAME ENTITY)
            (setq foundBlocks (cons (list obj insPt effName ent) foundBlocks))
          )
        )
        (setq i (1+ i))
      )

      (if (> count 0) 
        (progn 
          ;; Destaca blocos selecionados
          (sssetfirst nil ssFilter)

          ;; Ordena de cima para baixo (Y decrescente)
          (setq sortedBlocks (vl-sort foundBlocks 
                                      '(lambda (a b) 
                                         (> (cadr (cadr a)) (cadr (cadr b)))
                                       )
                             )
          )

          (setq occupiedRects '())
          (setq maxRetries 50)

          ;; Processar cada bloco
          (foreach item sortedBlocks 
            (setq obj (car item))
            (setq insPt (cadr item))
            (setq effName (caddr item))
            (setq ent (cadddr item))
            (setq blockLayer (vla-get-layer obj))

            ;; Extrai informacoes completas (TIPO, DIM, ALTURA, isLaje)
            (setq info (extract-markup-info obj effName blockLayer))
            (setq strTipo (nth 0 info))
            (setq strDim  (nth 1 info))
            (setq strAltu (nth 2 info))
            (setq isLaje  (nth 3 info))

            ;; Determina o estilo, bloco e dimensoes da caixa de colisao
            (if isLaje
              (setq targetBlock (get-available-block-name "TAG_MKP_LAJE")
                    targetStyle "MKP_LAJE"
                    rectW       1.60
                    rectH       0.55
              )
              (setq targetBlock (get-available-block-name "TAG_MKP_PADRAO")
                    targetStyle "MKP_PADRAO"
                    rectW       1.60
                    rectH       0.85
              )
            )

            ;; Ponta da seta: localizada exatamente no topo do simbolo 2D (5cm acima do centro de insercao)
            (setq arrowPt (list (car insPt) (+ (cadr insPt) 0.05) (caddr insPt)))

            ;; Posicao inicial da tag: 0.50m a direita e 0.20m acima para uma chamada curta e limpa
            (setq baseTextPt (list 
                               (+ (car arrowPt) 0.50) 
                               (+ (cadr arrowPt) 0.20) 
                               (caddr arrowPt)
                             )
            )

            (setq candidatePt baseTextPt)
            (setq retryCount 0)
            (setq overlap T)

            ;; Loop de Anti-Colisao
            (while (and overlap (< retryCount maxRetries)) 
              (setq overlap nil)

              (setq candidateRect (list 
                                    (car candidatePt)
                                    (- (cadr candidatePt) (/ rectH 2.0))
                                    (+ (car candidatePt) rectW)
                                    (+ (cadr candidatePt) (/ rectH 2.0))
                                  )
              )

              (foreach r occupiedRects 
                (if (check-box-overlap candidateRect r) 
                  (setq overlap T)
                )
              )

              (if overlap 
                (progn 
                  (setq retryCount (1+ retryCount))
                  (if (= (rem retryCount 5) 0) 
                    (setq candidatePt (list 
                                        (+ (car candidatePt) (+ rectW 0.2)) 
                                        (cadr baseTextPt) 
                                        (caddr candidatePt)
                                      )
                    )
                    (setq candidatePt (list 
                                        (car candidatePt) 
                                        (- (cadr candidatePt) (/ rectH 1.2)) 
                                        (caddr candidatePt)
                                      )
                    )
                  )
                )
              )
            )

            (setq occupiedRects (cons candidateRect occupiedRects))
            (setq textPt candidatePt)

            ;; Cria o Multileader diretamente via ActiveX
            (setq mleaderObj (create-mleader-direct spc arrowPt textPt))

            (if (and mleaderObj (vlax-write-enabled-p mleaderObj))
              (progn 
                ;; Adiciona ao selection set para draworder
                (setq lastEnt (vlax-vla-object->ename mleaderObj))
                (if lastEnt (ssadd lastEnt ssNewLeaders))

                ;; Define estilo, bloco e conexao
                (or (set-mleader-style-safe targetStyle)
                    (set-mleader-style-safe targetBlock))

                (vl-catch-all-apply 'vla-put-ContentType (list mleaderObj 1))
                (vl-catch-all-apply 'vla-put-ContentBlockName (list mleaderObj targetBlock))
                (vl-catch-all-apply 'vla-put-BlockConnectionType (list mleaderObj 0))

                ;; Injeta os valores nos atributos por Tag
                (set-mleader-attrib mleaderObj "TIPO" strTipo)
                (set-mleader-attrib mleaderObj "DIM" strDim)
                (if (not isLaje)
                  (set-mleader-attrib mleaderObj "ALTURA" strAltu)
                )

                ;; Ajustes visuais finos
                (vl-catch-all-apply 'vla-put-ArrowheadSize (list mleaderObj 0.15))
                (vl-catch-all-apply 'vla-put-LandingGap (list mleaderObj 0.05))
                (vl-catch-all-apply 'vla-put-DoglegLength (list mleaderObj 0.1))

                ;; Define Layer e Cor Index 210
                (vl-catch-all-apply 'vla-put-Layer (list mleaderObj blockLayer))
                (vl-catch-all-apply 'vla-put-Color (list mleaderObj 210))
                (setq colObj (vl-catch-all-apply 'vla-getinterfaceobject (list (vlax-get-acad-object) (strcat "AutoCAD.AcCmColor." (substr (getvar "ACADVER") 1 2)))))
                (if (and (not (vl-catch-all-error-p colObj)) colObj)
                  (progn
                    (vla-put-ColorIndex colObj 210)
                    (vl-catch-all-apply 'vla-put-LeaderLineColor (list mleaderObj colObj))
                    (vlax-release-object colObj)
                  )
                )
              )
            )
          ) ; end foreach

          ;; 5. Aplicar Draw Order: trazer todas as anotações para a frente silenciosamente
          (setq ssOrder (ssget "_X" '((0 . "MULTILEADER") (8 . "PP_MKP_*") (410 . "Model"))))
          (if (and ssOrder (> (sslength ssOrder) 0))
            (send-to-front-silent doc ssOrder)
            (if (and ssNewLeaders (> (sslength ssNewLeaders) 0))
              (send-to-front-silent doc ssNewLeaders)
            )
          )

          ;; Restaura Layer e Estilo
          (setvar "CLAYER" oldLayer)
          (princ 
            (strcat "\n" (itoa count) " bloco(s) processado(s) e anotações criadas com sucesso (trazidas para a frente).")
          )
        )
        (princ "\nNenhum bloco de markup correspondente foi encontrado.")
      )
    )
    (princ "\nNenhum bloco selecionado.")
  )

  ;; 6. Clean Exit
  (*error* nil)
  (princ)
)

;;; ==========================================================================
;;;  COMANDO UPDKP: Atualizar anotações existentes com base nos furos
;;; ==========================================================================
(defun c:UPDKP (/ doc spc ssMLeaders i entMLeader mleaderObj lineIds vertices 
                 arrowPt ssBlocks j entBlock blockObj effName insPt blockList 
                 closestBlock minDist dist blockLayer isLaje strTipo strDim 
                 strAltu diam altu larg comp count curTipo curDim curAltu 
                 changed blkName ssOrder info dxf10
                )
  (vl-load-com)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq spc (if (= (getvar "CVPORT") 1) 
              (vla-get-PaperSpace doc)
              (vla-get-ModelSpace doc)
            )
  )

  (vla-StartUndoMark doc)
  (setq count 0)

  ;; 1. Obter todos os blocos de furo no Model
  (setq ssBlocks (ssget "_A" '((0 . "INSERT") (410 . "Model"))))
  (setq blockList '())
  (if ssBlocks
    (progn
      (setq j 0)
      (repeat (sslength ssBlocks)
        (setq entBlock (ssname ssBlocks j))
        (setq blockObj (vlax-ename->vla-object entBlock))
        (setq effName (vla-get-effectivename blockObj))
        (if (wcmatch (strcase effName) "*MARKUP*QUADRADO*,*MARKUP*RETANG*,*MARKUP*REDONDO*,PP_MARKUP_*")
          (progn
            (setq dxf10 (cdr (assoc 10 (entget entBlock))))
            (setq insPt (list (car dxf10) (cadr dxf10) (if (caddr dxf10) (caddr dxf10) 0.0)))
            (setq blockList (cons (cons blockObj insPt) blockList))
          )
        )
        (setq j (1+ j))
      )
    )
  )

  (if (null blockList)
    (progn
      (princ "\nNenhum bloco de markup encontrado no desenho.")
      (vla-EndUndoMark doc)
      (princ)
    )
    (progn
      ;; 2. Selecionar todos os Multileaders no Model
      (setq ssMLeaders (ssget "_A" '((0 . "MULTILEADER") (410 . "Model"))))
      (if ssMLeaders
        (progn
          (setq i 0)
          (repeat (sslength ssMLeaders)
            (setq entMLeader (ssname ssMLeaders i))
            (setq mleaderObj (vlax-ename->vla-object entMLeader))
            
            ;; Verifica se o MLeader utiliza Bloco
            (setq blkName (vl-catch-all-apply 'vla-get-ContentBlockName (list mleaderObj)))
            (if (and (not (vl-catch-all-error-p blkName)) 
                     blkName 
                     (member (strcase blkName) '("TAG_MKP_PADRAO" "TAG_MKP_LAJE" "PP_TAG_MKP_PADRAO" "PP_TAG_MKP_LAJE")))
              (progn
                ;; Obter o ponto de início (ponta da seta)
                (setq lineIds (vl-catch-all-apply 'vlax-invoke (list mleaderObj 'GetLeaderLineIds)))
                (if (and (not (vl-catch-all-error-p lineIds)) lineIds)
                  (progn
                    (setq vertices (vl-catch-all-apply 'vlax-invoke (list mleaderObj 'GetLeaderLineVertices (car lineIds))))
                    (if (and (not (vl-catch-all-error-p vertices)) (>= (length vertices) 3))
                      (progn
                        (setq arrowPt (list (car vertices) (cadr vertices) (caddr vertices)))
                        
                        ;; Encontra o bloco de furo mais próximo da ponta da seta
                        (setq closestBlock nil)
                        (setq minDist 1e9)
                        (foreach item blockList
                          (setq blockObj (car item))
                          (setq insPt (cdr item))
                          (setq dist (distance arrowPt insPt))
                          (if (< dist minDist)
                            (setq minDist dist
                                  closestBlock blockObj
                            )
                          )
                        )

                        ;; Se encontrou bloco proximo (raio de tolerancia adequado ao topo)
                        (if (and closestBlock (< minDist 0.50))
                          (progn
                            (setq blockLayer (vla-get-layer closestBlock))
                            (setq effName (vla-get-effectivename closestBlock))

                            ;; Extrai informacoes atualizadas
                            (setq info (extract-markup-info closestBlock effName blockLayer))
                            (setq strTipo (nth 0 info))
                            (setq strDim  (nth 1 info))
                            (setq strAltu (nth 2 info))
                            (setq isLaje  (nth 3 info))

                            ;; Verifica se houve alteração nos atributos
                            (setq curTipo (get-mleader-attrib mleaderObj "TIPO"))
                            (setq curDim  (get-mleader-attrib mleaderObj "DIM"))
                            (setq curAltu (get-mleader-attrib mleaderObj "ALTURA"))
                            (setq changed nil)

                            (if (/= curTipo strTipo)
                              (progn (set-mleader-attrib mleaderObj "TIPO" strTipo) (setq changed T))
                            )
                            (if (/= curDim strDim)
                              (progn (set-mleader-attrib mleaderObj "DIM" strDim) (setq changed T))
                            )
                            (if (and (not isLaje) (/= curAltu strAltu))
                              (progn (set-mleader-attrib mleaderObj "ALTURA" strAltu) (setq changed T))
                            )
                            (if (/= (vla-get-layer mleaderObj) blockLayer)
                              (progn (vla-put-layer mleaderObj blockLayer) (setq changed T))
                            )
                            (if (/= (vla-get-Color mleaderObj) 210)
                              (progn
                                (vl-catch-all-apply 'vla-put-Color (list mleaderObj 210))
                                (setq changed T)
                              )
                            )

                            (if changed
                              (setq count (1+ count))
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
            (setq i (1+ i))
          )
        )
      )
      ;; Aplicar Draw Order ao finalizar a atualizacao
      (setq ssOrder (ssget "_X" '((0 . "MULTILEADER") (8 . "PP_MKP_*") (410 . "Model"))))
      (if (and ssOrder (> (sslength ssOrder) 0))
        (send-to-front-silent doc ssOrder)
      )

      (vla-EndUndoMark doc)
      (princ (strcat "\n" (itoa count) " anotação(ões) atualizada(s) (trazidas para a frente)."))
      (princ)
    )
  )
)

;;; ==========================================================================
;;;  COMANDO SCMKP: Alterar a escala das anotações/leaders individualmente
;;; ==========================================================================
(if (null *MKP-LAST-SCALE*) (setq *MKP-LAST-SCALE* 1.0))

(defun c:SCMKP (/ doc spc ssMLeaders i entMLeader mleaderObj blkName 
                  targetList count userScale promptStr ssHighlight)
  (vl-load-com)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq spc (if (= (getvar "CVPORT") 1) 
              (vla-get-PaperSpace doc)
              (vla-get-ModelSpace doc)
            )
  )

  (vla-StartUndoMark doc)

  ;; 1. Selecionar todas as leaders de markup no desenho
  (setq ssMLeaders (ssget "_A" '((0 . "MULTILEADER") (410 . "Model"))))
  (setq targetList '())
  (setq ssHighlight (ssadd))

  (if ssMLeaders
    (progn
      (setq i 0)
      (repeat (sslength ssMLeaders)
        (setq entMLeader (ssname ssMLeaders i))
        (setq mleaderObj (vlax-ename->vla-object entMLeader))
        (setq blkName (vl-catch-all-apply 'vla-get-ContentBlockName (list mleaderObj)))
        (if (and (not (vl-catch-all-error-p blkName)) 
                 blkName 
                 (member (strcase blkName) '("TAG_MKP_PADRAO" "TAG_MKP_LAJE" "PP_TAG_MKP_PADRAO" "PP_TAG_MKP_LAJE")))
          (progn
            (setq targetList (cons mleaderObj targetList))
            (ssadd entMLeader ssHighlight)
          )
        )
        (setq i (1+ i))
      )
    )
  )

  (if (null targetList)
    (progn
      (princ "\nNenhuma anotação de markup (TAG_MKP) encontrada no desenho.")
      (vla-EndUndoMark doc)
      (princ)
    )
    (progn
      ;; Destaca visualmente todas as leaders selecionadas
      (sssetfirst nil ssHighlight)
      (princ (strcat "\n" (itoa (length targetList)) " anotação(ões) de markup selecionada(s)."))

      ;; 2. Solicita o fator de escala ao usuario
      (setq promptStr (strcat "\nInforme o novo fator de escala para as anotações <" (rtos *MKP-LAST-SCALE* 2 2) ">: "))
      (setq userScale (getreal promptStr))
      (if (null userScale) (setq userScale *MKP-LAST-SCALE*))

      (if (<= userScale 0.0)
        (progn
          (princ "\nFator de escala inválido (deve ser maior que zero).")
          (sssetfirst nil nil)
          (vla-EndUndoMark doc)
          (princ)
        )
        (progn
          (setq *MKP-LAST-SCALE* userScale)
          (setq count 0)

          ;; 3. Aplica a escala individualmente a cada Multileader
          (foreach mleaderObj targetList
            ;; Altera o fator de escala geral do Multileader (mantém o ponto individual da seta e bloco)
            (vl-catch-all-apply 'vla-put-ScaleFactor (list mleaderObj userScale))
            (vl-catch-all-apply 'vla-put-BlockScale (list mleaderObj userScale))
            (vl-catch-all-apply 'vla-put-ArrowheadSize (list mleaderObj (* 0.15 userScale)))
            (vl-catch-all-apply 'vla-put-LandingGap (list mleaderObj (* 0.05 userScale)))
            (vl-catch-all-apply 'vla-put-DoglegLength (list mleaderObj (* 0.10 userScale)))
            (setq count (1+ count))
          )

          ;; Remove selecao visual
          (sssetfirst nil nil)

          ;; 4. Aplica Draw Order para manter na frente
          (send-to-front-silent doc ssHighlight)

          (vla-EndUndoMark doc)
          (princ (strcat "\nEscala de " (itoa count) " anotação(ões) alterada para " (rtos userScale 2 2) " com sucesso."))
          (princ)
        )
      )
    )
  )
)

(princ "\nBB_AUTOMKP carregado com sucesso!")
(princ "\nComandos disponíveis:")
(princ "\n  - MKP   : Gera anotações em blocos TAG_MKP_PADRAO e TAG_MKP_LAJE")
(princ "\n  - UPDKP : Atualiza as anotações existentes após alterações nos furos")
(princ "\n  - SCMKP : Altera a escala de todas as anotações/leaders individualmente")
(princ)

