import Array (..)
import Text
import Graphics.Element as Element
import Graphics.Input as Input
import Window
import Signal
import Maybe
import Color

-- Modelo

type Casa = Vazia | Pedra
type alias Índice = Int
type alias Posição = (Disposição, Índice, Índice)
type Quadrante = Quadrante (Disposição, Array Casa)
type alias Tabuleiro = Array Quadrante
type alias Estado =
    { tabuleiro   : Tabuleiro
    , selecionada : Maybe Posição}

type Disposição
    = Superior
    | Inferior
    | Oeste
    | Leste
    | Central

índice : Disposição -> Índice
índice d = case d of
    Superior -> 0
    Inferior -> 1
    Oeste    -> 2
    Leste    -> 3
    Central  -> 4

disposição : Índice -> Disposição
disposição i = case i of
    0 -> Superior
    1 -> Inferior
    2 -> Oeste   
    3 -> Leste   
    4 -> Central 

quadrante : Disposição -> Tabuleiro -> Quadrante
quadrante d t = get (índice d) t |> Maybe.withDefault (Quadrante (Superior, empty))

tabuleiroInicial : Tabuleiro
tabuleiroInicial =
    let início = initialize 4 (\i -> Quadrante (disposição i, repeat 25 Pedra))
    in  push (Quadrante (Central, initialize 25 (\i -> if i == 12 then Vazia else Pedra))) início

estadoInicial : Estado
estadoInicial = { tabuleiro = tabuleiroInicial, selecionada = Nothing }

posição : Disposição -> Int -> Posição
posição d índice =
    let linha = índice//5
        coluna = índice - linha*5
    in  (d, linha, coluna)

-- Atualização

atualizarCasa : Casa -> Posição -> Tabuleiro -> Tabuleiro
atualizarCasa novaCasa (d, i, j) t =
    let (Quadrante (_, array)) = quadrante d t
        novoQuadrante =
            Quadrante (d, indexedMap (\k c -> if k == 5*i+j then novaCasa else c) array)
    in set (índice d) novoQuadrante t

removerPedra : Posição -> Tabuleiro -> Tabuleiro
removerPedra = atualizarCasa Vazia

inserirPedra : Posição -> Tabuleiro -> Tabuleiro
inserirPedra = atualizarCasa Pedra

mudarSeleção : Posição -> Estado -> Estado
mudarSeleção pos e =
    if e.selecionada == Just pos
       then { e | selecionada <- Nothing }
       else { e | selecionada <- Just pos }

atualizar : Comando -> Estado -> Estado
atualizar c e = case c of
    MudarSeleção pos -> mudarSeleção pos e

-- Exibição

exibirQuadrante : Maybe Posição -> Quadrante -> Element.Element
exibirQuadrante selecionada (Quadrante (disp, array))  =
    let info i =
            let posiçãoDaCasa = posição disp i
                foiSelecionada = selecionada == Just posiçãoDaCasa 
            in (foiSelecionada, posiçãoDaCasa)
        casas = indexedMap (exibirCasa info) array
    in  Element.flow Element.down
            [ slice 0  5  casas |> toList |> Element.flow Element.right
            , slice 5  10 casas |> toList |> Element.flow Element.right
            , slice 10 15 casas |> toList |> Element.flow Element.right
            , slice 15 20 casas |> toList |> Element.flow Element.right
            , slice 20 25 casas |> toList |> Element.flow Element.right
            ]

exibirCasa : (Índice -> (Bool, Posição)) -> Índice -> Casa -> Element.Element
exibirCasa f i c =
    let (selecionada, pos) = f i
    in Input.clickable (Signal.send canal (MudarSeleção pos)) <|
           case c of
               Vazia -> Text.plainText " _ "
               Pedra -> exibirPedra selecionada

exibirPedra : Bool -> Element.Element
exibirPedra selecionada =
    if selecionada
        then Text.color Color.lightGreen (Text.fromString " o ") |> Text.centered
        else Text.plainText " o "

desenhoDoQuadrante : Disposição -> Estado -> Element.Element
desenhoDoQuadrante d e = exibirQuadrante e.selecionada (quadrante d e.tabuleiro)

quadranteSuperior = desenhoDoQuadrante Superior
quadranteInferior = desenhoDoQuadrante Inferior
quadranteOeste = desenhoDoQuadrante Oeste
quadranteLeste = desenhoDoQuadrante Leste
quadranteCentral = desenhoDoQuadrante Central
espaçoVazio e =
    Element.spacer (Element.widthOf <| quadranteOeste e) (Element.heightOf <| quadranteSuperior e)

exibir : (Int, Int) -> Estado -> Element.Element
exibir (x, y) e = Element.container x y Element.middle <| Element.flow Element.down
    [ Element.flow Element.right [ espaçoVazio e,    quadranteSuperior e, espaçoVazio e    ]
    , Element.flow Element.right [ quadranteOeste e, quadranteCentral e,  quadranteLeste e ]
    , Element.flow Element.right [ espaçoVazio e,    quadranteInferior e, espaçoVazio e    ]
    ]

-- Sinais

type Comando
    = MudarSeleção Posição

canal : Signal.Channel Comando
canal = Signal.channel <| MudarSeleção (Central, 2, 2)

main : Signal Element.Element
main =  Signal.subscribe canal
     |> Signal.foldp atualizar estadoInicial
     |> Signal.map2 exibir Window.dimensions
