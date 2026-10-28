# Regressão automatizada da PP_CHLAY

Execute na raiz do repositório:

```powershell
python tests/pp_chlay/run.py check
```

O comando abre uma **nova instância do AutoCAD**, cria os objetos sintéticos em
um desenho novo e vazio, executa a LISP e compara os resultados com
`baseline.json`. A instância já aberta pelo usuário não é acessada. O runner
fecha somente o processo que criou. Os desenhos de teste não são salvos.

Requisitos: Windows, Python 3.10+ e AutoCAD completo com ActiveX e licença
disponível. Não requer pacotes Python externos. O executável e `acadiso.dwt`
são encontrados automaticamente; podem ser indicados com `--acad` e `--template`.
O Core Console não é usado, pois a rotina depende do objeto ActiveX do AutoCAD.

Resultados e códigos de saída:

| Saída | Código | Significado |
|---|---:|---|
| APROVADO (verde) | 0 | Todos os grupos coincidem com a referência; nenhuma regressão detectada nos cenários testados. |
| FALHOU (vermelho) | 1 | A execução terminou, mas o comportamento mudou. Revisar `comparison.json`. |
| ERRO (vermelho) | 2 | A verificação não foi concluída: falha de execução, ambiente incompatível ou saída incompleta. |

O terminal mostra o andamento, os registros e resultados por grupo, a quantidade
de diferenças e os tempos em segundos. Os 4.289 registros da referência inicial
são observações de comportamento, não 4.289 testes independentes.
O tempo total inclui iniciar e encerrar a instância de teste do AutoCAD.
Os tempos de CHLAUTO e CHLDEL representam apenas o processamento de cada comando.

As cores são habilitadas automaticamente em terminais compatíveis, inclusive
Windows Terminal/PowerShell. Arquivos redirecionados recebem texto sem códigos
de cor. Para desativar explicitamente:

```powershell
python tests/pp_chlay/run.py check --color never
```

Também é possível usar `--color always` para forçar cores ou a variável
`NO_COLOR` para desativar a seleção automática. O significado do resultado é
sempre escrito por extenso e não depende de distinguir cores.

Os relatórios ficam em `tests/pp_chlay/runs/<execução>/`, ignorados pelo Git.
Eles registram os hashes dos fontes e dos testes, resultados por cenário,
diferenças e duração do processamento. Tempo não é critério de aprovação.
AutoCAD e `LISPSYS` precisam coincidir com o ambiente que capturou a referência.
O tempo limite padrão é de 600 segundos, ajustável com `--timeout`.

## Cobertura

- 648 objetos de entrada por execução de `CHLAUTO` e `CHLDEL`, incluindo
  exemplos concretos derivados das 124 regras de exclusão e 404 de remapeamento.
  Exemplos de padrões sobrepostos não provam que cada regra seja alcançável.
- Matriz de `LINE`, `TEXT`, `MTEXT`, polilinha aberta, polilinha fechada e `HATCH`
  em 19 camadas representativas, incluindo condições especiais e camada desconhecida.
- 3.168 comparações do matcher real: nomes comuns, minúsculas, prefixos de XREF,
  prefixos de bind e casos negativos. Nomes com `|` são testados como strings,
  sem simular um arquivo XREF vinculado.
- Aplicação manual de camadas, normalização de nomes e propriedades de camadas PP.
- Cor, tipo e escala de linha, espessura, transparência, fechamento, vértices,
  camada/layout, padrão, escala, ângulo e área das hachuras.
- Repetição da formatação de pilares, deduplicação e execução de `CHLHATCH`
  com seleção implícita e uma hachura fora da seleção como sentinela.
- Estado final de `CMDECHO`, `HIGHLIGHT`, `LTSCALE`, `CLAYER` e `CETRANSPARENCY`.

Os comandos automáticos/manual são exercitados pelas mesmas funções de produção
chamadas por `CHLAUTO`, `CHLDEL` e `CHL`. `CHLHATCH` é invocado diretamente com
seleção implícita. Não há substituição de `ssget`, DXF, ActiveX ou funções da LISP.
O fluxo interativo de digitação de nomes/seleção de `CHL` não é automatizado.

São comparados estados observáveis, sem handles e identificadores transitórios.
A multiplicidade dos objetos é preservada: uma cópia extra também causa falha.
Números usam tolerância absoluta de `1e-7` e relativa de `1e-9`.

## Referência e mudanças intencionais

A referência inicial vem do commit **d0b5415**, anterior às otimizações.
`record` sempre lê o fonte pelo Git, sem aceitar automaticamente o arquivo em edição:

```powershell
python tests/pp_chlay/run.py record
```

Esse comando recusa sobrescrever uma referência existente. Para uma atualização
intencional, revisar primeiro as diferenças, registrar a versão desejada em um
commit e então capturar explicitamente esse commit:

```powershell
python tests/pp_chlay/run.py record --ref <commit-revisado> --replace-baseline
```

Alterações nos cenários ou no harness exigem recaptura da referência; o runner
confere seus hashes antes de testar. Durante a construção dos testes, recapturar
`d0b5415` mantém o comportamento de produção original como referência.

**Preservar comportamento não significa aprovar todos os comportamentos atuais.**
A referência também registra problemas existentes: regravar DXF antigo ao fechar
um pilar, repetição que cria hachuras adicionais e padronização fora da seleção.
Uma correção desses problemas deve produzir diferença para revisão, seguida de
uma atualização explícita da referência e de um cenário que proteja a correção.

Essa suíte não garante equivalência em qualquer DWG. Ainda não cobre blocos e
atributos, cotas, proxies, camadas bloqueadas, vínculos XREF reais, cancelamento
por ESC, UNDO, sequência visual de draw order ou toda a geometria de hachuras.
Ela deve ser ampliada com cenários dos arquivos reais à medida que forem relevantes.

## Validação do próprio harness

Testes rápidos do parser e do comparador:

```powershell
python -m unittest discover -s tests/pp_chlay -p test_runner.py
```

Prova de detecção no AutoCAD, alterando apenas uma cópia temporária da referência:

```powershell
python tests/pp_chlay/verify_detection.py
```

Essa prova injeta três mudanças: destino de `NOMEPILAR`, exclusão desativada e
cor explícita no lugar de ByLayer. Só passa quando as três alterações são
detectadas. Ela não modifica o fonte de produção nem a referência.
