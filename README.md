# Projeto de Haskell - Paradigmas de Programação (MC346)

## Alunos

- [Tiago de Paula Alves](mailto:tiagodepalves@gmail.com) (187679)
- João Pedro de Amorim (176131)

## Enunciado do Projeto

Uma técnica em aprendizado de máquina é chamada de classificação semi-supervisionada. Alguns dados recebem um label (uma classe) e varios dados não. O objetivo é atribuir uma classe/label para cada um dos dados sem classe.

Faremos esta atribuição por proximidade - se um dado X sem label esta mais proximo de um dado do label A que de qualquer outro dado com label, ele assume o label A. Mas agora X é um dado com label e ele vai propagar seu label para o dado mais proximo sem label (que nao esta ainda mais proximo de outro dado com outro label).

O algoritmo que usaremos é

- separar os pontos com e sem label
- calcular a distancia entre os pontos dos 2 grupos
- selecionar o ponto do grupo sem classe mais proximo de algum do grupo com classe
- atribuir a classe a este ponto e atualizar os 2 grupos
- repetir

Este algoritmo é cubico no numero de pontos. Provavelmente há algoritmos mais eficientes para calcular como propagar os labels, mas a não ser que voce tenha uma ideia de um algoritmo melhor, usem o algoritmo cubico. (O Prof. Falcao do IC usa muito propagação baseada em distancia para classificação semi-supervisionada, e ele propos um/uma familia de algoritmos chamado Optimal Path Forest (OPF) que eu acho é baseada em calcular o Minimum Spanning Tree (MST) dos pontos. Talvez exista uma solução mais eficiente para o nosso problema usando MST).

Escreva um programa em Haskel que recebe os dados no seguinte formato

```raw
aa x1 x2 x3
b3   y1  y2 y3
ez34 z1 z2 z3
...
g78 n1 n2 n3

aa 1
gg7 3
...
```

onde aa (b3 etc.) são os "nomes" dos pontos, e x1 x2 x3 nesse caso são as coordenadas num espaço de 3 dimensões do ponto aa, y1 y2 y3 são as coordenadas do ponto b3 e assim por diante. Voce não sabe de antemão o numero de dimensoes dos dados. Os dados acima estão em 3D mas seu program pode receber dados em 2D ou 13D etc. As coordenadas do ponto (quantas forem) serão os números que seguem o nome do ponto. Pode haver mais de um branco separando os dados em cada linha.

Segue uma linha em branco e depois uma linha para cada dado que tem um label, seguido do label.

O programa devera imprimir o label e o nome dos pontos em cada grupo - um grupo por linha, ordenados alfabeticamente. Por exemplo se tivermos que dividir os dados em 3 grupos então a saída

```raw
1 aa b3 g78 g79 g80
2 ez34 f45 zz9
3 a47 gg7
...
```

indica os 3 grupos (a ordem que os grupos são impressos não é importante). A saída pode ser também no formato

```raw
(1, ["aa", "b3", "g78", "g79", "g80"])
(2, ["ez34", "f45", "zz9"])
(3, ["a47", "gg7"])
```

O seu programa rodará como:

```bash
    runhaskell projeto2.hs < dados.txt
```

ou seja os dados devem ser lidos do standard input.
