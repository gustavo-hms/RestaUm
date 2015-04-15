import Array
import Text
import Graphics.Element (..)
import Graphics.Input as Input
import Window
import Signal
import Maybe
import Color

-- Modelo

type Pedra = Desmarcada | Marcada
type Casa = Vazia | Casa Pedra
type Quadrante = Quadrante (Array.Array Casa)
type alias Tabuleiro = Array.Array Quadrante 
type alias PosiçãoDaCasa = (Disposição, Int, Int)

type Disposição
    = Superior
    | Inferior
    | Oeste
    | Leste
    | Central

índice : Disposição -> Int
índice d = case d of
    Superior -> 0
    Inferior -> 1
    Oeste    -> 2
    Leste    -> 3
    Central  -> 4

quadrante : Disposição -> Tabuleiro -> Quadrante
quadrante d t = Array.get (índice d) t |> Maybe.withDefault (Quadrante Array.empty)

tabuleiroInicial : Tabuleiro
tabuleiroInicial = Array.fromList
    [ Quadrante <| Array.repeat 25 (Casa Desmarcada)
    , Quadrante <| Array.repeat 25 (Casa Desmarcada)
    , Quadrante <| Array.repeat 25 (Casa Desmarcada)
    , Quadrante <| Array.repeat 25 (Casa Desmarcada)
    , Quadrante <| Array.initialize 25 (\i -> if i == 12 then Vazia else Casa Desmarcada)
    ]

casa : PosiçãoDaCasa -> Tabuleiro -> Casa
casa (d, i, j) t =
    let (Quadrante q) = quadrante d t
    in  Array.get (5*i+j) q |> Maybe.withDefault (Vazia)

posição : Disposição -> Int -> PosiçãoDaCasa
posição d índice =
    let linha = índice//5
        coluna = índice - linha*5
    in  (d, linha, coluna)

-- Atualização

atualizarCasa : PosiçãoDaCasa -> (Casa -> Casa) -> Tabuleiro -> Tabuleiro
atualizarCasa (d, i, j) f t =
    let (Quadrante q) = quadrante d t
        novoQuadrante =
            Quadrante <| Array.indexedMap (\k c -> if k == 5*i+j then f c else c) q
    in  Array.set (índice d) novoQuadrante t

removerPedra : PosiçãoDaCasa -> Tabuleiro -> Tabuleiro
removerPedra p t = atualizarCasa p (\_ -> Vazia) t

inserirPedra : PosiçãoDaCasa -> Pedra -> Tabuleiro -> Tabuleiro
inserirPedra pos p t = atualizarCasa pos (\_ -> Casa p) t

alterarMarcação : PosiçãoDaCasa -> Tabuleiro -> Tabuleiro
alterarMarcação pos t =
    case casa pos t of
        Vazia  -> t
        Casa p -> case p of
            Marcada    -> inserirPedra pos Desmarcada t
            Desmarcada -> inserirPedra pos Marcada t

atualizar : Comando -> Tabuleiro -> Tabuleiro
atualizar comando t = case comando of
    AlterarMarcação p -> alterarMarcação p t

-- Exibição

exibirQuadrante : Disposição -> Tabuleiro -> Element
exibirQuadrante d t =
    let (Quadrante array) = quadrante d t
        casas = Array.indexedMap (exibirCasa d) array
    in  flow down
            [ Array.slice 0  5  casas |> Array.toList |> flow right
            , Array.slice 5  10 casas |> Array.toList |> flow right
            , Array.slice 10 15 casas |> Array.toList |> flow right
            , Array.slice 15 20 casas |> Array.toList |> flow right
            , Array.slice 20 25 casas |> Array.toList |> flow right
            ]

exibirCasa : Disposição -> Int -> Casa -> Element
exibirCasa d índice c =
    let p = posição d índice
    in case c of
        Vazia      -> Text.plainText " _ " |> Input.clickable (Signal.send canal (AlterarMarcação p))
        Casa pedra -> exibirPedra pedra |> Input.clickable (Signal.send canal (AlterarMarcação p))

exibirPedra : Pedra -> Element
exibirPedra p = case p of
    Marcada    -> Text.color Color.lightGreen (Text.fromString " o ") |> Text.centered
    Desmarcada -> Text.plainText " o "

quadranteSuperior = exibirQuadrante Superior
quadranteInferior = exibirQuadrante Inferior
quadranteOeste = exibirQuadrante Oeste
quadranteLeste = exibirQuadrante Leste
quadranteCentral = exibirQuadrante Central
espaçoVazio t = spacer (widthOf <| quadranteOeste t) (heightOf <| quadranteSuperior t)

exibir : (Int, Int) -> Tabuleiro -> Element
exibir (x, y) t = container x y middle <| flow down
    [ flow right [ espaçoVazio t,    quadranteSuperior t, espaçoVazio t    ]
    , flow right [ quadranteOeste t, quadranteCentral t,  quadranteLeste t ]
    , flow right [ espaçoVazio t,    quadranteInferior t, espaçoVazio t    ]
    ]

-- Sinais

type Comando
    = AlterarMarcação PosiçãoDaCasa

canal : Signal.Channel Comando
canal = Signal.channel <| AlterarMarcação (Central, 2, 2)

main : Signal Element
main =  Signal.subscribe canal
     |> Signal.foldp atualizar tabuleiroInicial
     |> Signal.map2 exibir Window.dimensions
