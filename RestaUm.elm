import Array (..)
import Text
import Graphics.Element as Element
import Graphics.Input as Input
import Graphics.Collage as Collage
import Window
import Signal
import Maybe
import Color
import List

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

casa : Tabuleiro -> Posição -> Casa
casa t (d, i, j) =
    let (Quadrante (_, a)) = quadrante d t
    in  get (5*i+j) a |> Maybe.withDefault (Vazia)

casaIntermediária : Posição -> Posição -> Tabuleiro -> Maybe Casa
casaIntermediária a b t = Maybe.map (casa t) (posiçãoIntermediária a b)

posiçãoIntermediária : Posição -> Posição -> Maybe Posição
posiçãoIntermediária a b = case distância a b of
    (2, 0) ->
        let (i, j) = posiçãoAbsoluta a
        in  posiçãoNoQuadrante (i+1, j)

    (-2, 0) ->
        let (i, j) = posiçãoAbsoluta a
        in  posiçãoNoQuadrante (i-1, j)

    (0, 2) ->
        let (i, j) = posiçãoAbsoluta a
        in  posiçãoNoQuadrante (i, j+1)

    (0, -2) ->
        let (i, j) = posiçãoAbsoluta a
        in  posiçãoNoQuadrante (i, j-1)

    _      -> Nothing

posiçãoAbsoluta : Posição -> (Índice, Índice)
posiçãoAbsoluta (d, i, j) =
    let iAbsoluto = case d of
            Superior -> i
            Inferior -> i + 10
            _        -> i + 5
        jAbsoluto = case d of
            Oeste -> j
            Leste -> j + 10
            _     -> j + 5
    in  (iAbsoluto, jAbsoluto)

distância : Posição -> Posição -> (Índice, Índice)
distância p1 p2 =
    let (i1, j1) = posiçãoAbsoluta p1
        (i2, j2) = posiçãoAbsoluta p2
    in  (i2 - i1, j2 - j1)

posiçãoNoQuadrante : (Índice, Índice) -> Maybe Posição
posiçãoNoQuadrante (i, j) =
    let quadrantesX = if | i < 5     -> [Superior]
                         | i < 10    -> [Oeste, Central, Leste]
                         | otherwise -> [Inferior]

        quadrantesY = if | j < 5     -> [Oeste]
                         | j < 10    -> [Superior, Central, Inferior]
                         | otherwise -> [Leste]

        quadrante = List.filter (flip List.member quadrantesY) quadrantesX

    in if List.length quadrante == 0 then Nothing else Just (List.head quadrante, i%5, j%5)


-- Atualização

atualizarCasa : Casa -> Posição -> Tabuleiro -> Tabuleiro
atualizarCasa novaCasa (d, i, j) t =
    let (Quadrante (_, array)) = quadrante d t
        novoQuadrante = Quadrante (d, set (5*i+j) novaCasa array)
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

mover : Maybe Posição -> Posição -> Estado -> Estado
mover ma b e =
    case ma of
        Nothing -> e
        Just a  ->
            let intermediária = posiçãoIntermediária a b
                casaIntermediária = Maybe.map (casa e.tabuleiro) intermediária
            in  case intermediária of
                    Nothing  -> e
                    Just pos -> if casaIntermediária == Just Vazia
                                   then e
                                   else { e | tabuleiro <- e.tabuleiro  
                                                        |> removerPedra a
                                                        |> removerPedra pos
                                                        |> inserirPedra b }

atualizar : Comando -> Estado -> Estado
atualizar c e = case c of
    MudarSeleção pos -> mudarSeleção pos e
    Mover pos        -> mover e.selecionada pos e

-- Exibição

exibirQuadrante : Int -> Maybe Posição -> Quadrante -> Element.Element
exibirQuadrante lado selecionada (Quadrante (disp, array))  =
    let info i =
            let posiçãoDaCasa = posição disp i
                foiSelecionada = selecionada == Just posiçãoDaCasa 
            in (foiSelecionada, posiçãoDaCasa)
        casas = indexedMap (info >> exibirCasa (lado//5)) array
    in  Element.flow Element.down
            [ slice 0  5  casas |> toList |> Element.flow Element.right
            , slice 5  10 casas |> toList |> Element.flow Element.right
            , slice 10 15 casas |> toList |> Element.flow Element.right
            , slice 15 20 casas |> toList |> Element.flow Element.right
            , slice 20 25 casas |> toList |> Element.flow Element.right
            ]

exibirCasa : Int -> (Bool, Posição) -> Casa -> Element.Element
exibirCasa lado (selecionada, pos) c = case c of
    Vazia -> vazio lado |> Input.clickable (Signal.send canal (Mover pos))
    Pedra -> pedra lado selecionada |> Input.clickable (Signal.send canal (MudarSeleção pos))

pedra : Int -> Bool -> Element.Element
pedra lado selecionada =
    let (corInterna, corDaBorda) = if selecionada then (Color.lightGreen, Color.darkGreen) else (Color.black, Color.black)
    in  desenharPedra lado corInterna corDaBorda 

desenharPedra : Int -> Color.Color -> Color.Color -> Element.Element
desenharPedra lado corInterna corDaBorda =
    let raio = (toFloat lado)/2 - 3
    in  Collage.collage lado lado
        [ Collage.circle raio |> Collage.filled corInterna
        , Collage.circle raio |> Collage.outlined (Collage.solid corDaBorda)]

vazio : Int -> Element.Element
vazio lado = Collage.collage lado lado [Collage.circle 1.5 |> Collage.filled Color.black]

desenhoDoQuadrante : Int -> Disposição -> Estado -> Element.Element
desenhoDoQuadrante lado d e = exibirQuadrante lado e.selecionada (quadrante d e.tabuleiro)

quadranteSuperior lado = desenhoDoQuadrante lado Superior
quadranteInferior lado = desenhoDoQuadrante lado Inferior
quadranteOeste lado = desenhoDoQuadrante lado Oeste
quadranteLeste lado = desenhoDoQuadrante lado Leste
quadranteCentral lado = desenhoDoQuadrante lado Central
espaçoVazio lado = Element.spacer lado lado

exibir : (Int, Int) -> Estado -> Element.Element
exibir (x, y) e =
    let lado = 120
    in  Element.container x y Element.middle <| Element.flow Element.down
            [ Element.flow Element.right [ espaçoVazio lado,      quadranteSuperior lado e, espaçoVazio lado      ]
            , Element.flow Element.right [ quadranteOeste lado e, quadranteCentral lado e,  quadranteLeste lado e ]
            , Element.flow Element.right [ espaçoVazio lado,      quadranteInferior lado e, espaçoVazio lado      ]
            ]

-- Sinais

type Comando
    = MudarSeleção Posição
    | Mover Posição

canal : Signal.Channel Comando
canal = Signal.channel <| MudarSeleção (Central, 2, 2)

main : Signal Element.Element
main =  Signal.subscribe canal
     |> Signal.foldp atualizar estadoInicial
     |> Signal.map2 exibir Window.dimensions
