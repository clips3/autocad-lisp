# AutoLISP para AutoCAD

Coleção de rotinas AutoLISP da organização, padronizadas com o prefixo `PP_`.

## Conteúdo

- `PP_*.lsp`: arquivos-fonte das rotinas.
- `PP_FLD2TXT.vlx`: aplicativo AutoLISP compilado.

## Uso

Carregue o arquivo desejado no AutoCAD com `APPLOAD`. Os comandos disponíveis são indicados no cabeçalho ou na mensagem de carregamento de cada rotina. Algumas rotinas podem depender de bibliotecas ou caminhos configurados no ambiente da organização.

## Testes de regressão

A rotina `PP_CHLAY.lsp` tem testes automatizados executados no AutoCAD:

```powershell
python tests/pp_chlay/run.py check
```

Consulte [a documentação dos testes](tests/pp_chlay/README.md) para requisitos,
cobertura e atualização da referência.
