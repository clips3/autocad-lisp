(defun c:TXA ( / doc fontPath)
  ;; Carrega as extensões do Visual LISP (ActiveX)
  (vl-load-com)
  
  ;; Obtém o documento ativo do AutoCAD
  (setq doc (vla-get-activedocument (vlax-get-acad-object)))
  
  ;; Identifica o caminho correto da fonte instalada no Windows
  (setq fontPath (findfile "bahnschrift.ttf"))
  (if (not fontPath)
    (setq fontPath "bahnschrift.ttf") ;; fallback caso o findfile falhe
  )
  
  ;; Percorre todos os estilos de texto (Styles) no desenho iterativamente
  (vlax-for style (vla-get-textstyles doc)
    ;; 1. Força o caminho do arquivo de fonte (resolve fontes simples/SHX ou não encontradas)
    (vl-catch-all-apply 'vla-put-fontfile (list style fontPath))
    
    ;; 2. Força agressivamente as propriedades internas de família TrueType para "Bahnschrift" puro
    ;; Parâmetros: Objeto, FaceName(Bahnschrift), Negrito(Falso), Itálico(Falso), Charset(0), PitchAndFamily(34)
    (vl-catch-all-apply 'vla-setfont (list style "Bahnschrift" :vlax-false :vlax-false 0 34))
  )
  
  ;; Aplica o comando REGENALL (que é o comando completo do atalho REA)
  (command "_.REGENALL")
  
  (princ "\nTodos os estilos de texto (incluindo ausentes/não nativos) foram forçados para a fonte Bahnschrift!")
  (princ)
)

(princ "\nLISP TXARIAL carregada com sucesso. Digite TXA para rodar o comando.")
(princ)
