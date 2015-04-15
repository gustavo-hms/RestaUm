module RestaUm where

import Array
import Text
import Graphics.Element (..)
import Window
import Signal
import Maybe

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

tabuleiro =  tabuleiroInicial
    -- |> removerPedra (Inferior, 2, 3)
    -- |> inserirPedra (Central, 2, 2) Marcada

-- Atualização

atualizarCasa : PosiçãoDaCasa -> (Casa -> Casa) -> Tabuleiro -> Tabuleiro
atualizarCasa (d, i, j) f t =
    let (Quadrante q) = quadrante d t
        novoQuadrante =
            Quadrante <| Array.indexedMap (\k c -> if k == 5*i+j then f c else c) q
    in  Array.set (índice d) novoQuadrante t

removerPedra : PosiçãoDaCasa -> Tabuleiro -> Tabuleiro
removerPedra posição t = atualizarCasa posição (\c -> Vazia) t

inserirPedra : PosiçãoDaCasa -> Pedra -> Tabuleiro -> Tabuleiro
inserirPedra posição p t = atualizarCasa posição (\c -> Casa p) t

-- Exibição

exibirQuadrante : Quadrante -> Element
exibirQuadrante (Quadrante a) =
    flow down
    [ Array.slice 0  5  a |> Array.map exibirCasa |> Array.toList |> flow right
    , Array.slice 5  10 a |> Array.map exibirCasa |> Array.toList |> flow right
    , Array.slice 10 15 a |> Array.map exibirCasa |> Array.toList |> flow right
    , Array.slice 15 20 a |> Array.map exibirCasa |> Array.toList |> flow right
    , Array.slice 20 25 a |> Array.map exibirCasa |> Array.toList |> flow right
    ]

exibirCasa : Casa -> Element
exibirCasa c = case c of
    Vazia   -> Text.plainText " _ "
    Casa  _ -> Text.plainText " o "

quadranteSuperior = exibirQuadrante <| quadrante Superior tabuleiro
quadranteInferior = exibirQuadrante <| quadrante Inferior tabuleiro
quadranteOeste = exibirQuadrante <| quadrante Oeste tabuleiro
quadranteLeste = exibirQuadrante <| quadrante Leste tabuleiro
quadranteCentral = exibirQuadrante <| quadrante Central tabuleiro
espaçoVazio = spacer (widthOf quadranteOeste) (heightOf quadranteSuperior)

exibir : (Int, Int) -> Element
exibir (x, y) = container x y middle <| flow down
    [ flow right [ espaçoVazio,    quadranteSuperior, espaçoVazio    ]
    , flow right [ quadranteOeste, quadranteCentral,  quadranteLeste ]
    , flow right [ espaçoVazio,    quadranteInferior, espaçoVazio    ]
    ]

-- Sinais

main : Signal Element
main = Signal.map exibir Window.dimensions
