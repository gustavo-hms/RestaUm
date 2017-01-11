module RestaUm exposing (..)

import Matrix


-- Modelo


type Casa
    = Vazia
    | Pedra


type alias Posição =
    ( Int, Int )


type alias Tabuleiro =
    Matrix.Matrix Casa


type alias Jogada =
    { origem : Posição
    , destino : Posição
    }


type alias Modelo =
    { tabuleiro : Tabuleiro
    , escolhida : Maybe Posição
    }


novoTabuleiro =
    let
        casas =
            Matrix.repeat 15 15 Pedra
    in
        Matrix.set 6 6 Vazia casas



-- Atualização


type Ação
    = Escolher Posição
    | MoverPara Posição


atualizar : Ação -> Modelo -> Modelo
atualizar ação modelo =
    case ação of
        Escolher posição ->
            escolher posição modelo

        MoverPara posição ->
            let
                jogada =
                    { origem = modelo.escolhida, destino = posição }
            in
                { tabuleiro = mover jogada modelo.tabuleiro, escolhida = Nothing }


escolher : Posição -> Modelo -> Modelo
escolher posição modelo =
    if ocupada posição modelo then
        { modelo | escolhida = Just posição }
    else
        modelo


mover : Jogada -> Tabuleiro -> Tabuleiro
mover jogada tabuleiro =
    if not (jogadaVálida jogada) then
        tabuleiro
    else
        let
            { origem, destino } =
                jogada

            casaDoMeio =
                metade origem destino
        in
            tabuleiro
                |> remover origem
                |> remover casaDoMeio
                |> inserir destino


jogadaVálida : Jogada -> Bool
jogadaVálida { origem, destino } =
    let
        meio =
            talvezMetade origem destino
    in
        case meio of
            Nothing ->
                False

            Just casaDoMeio ->
                ocupada origem && ocupada casaDoMeio && not (ocupada destino)


talvezMetade : Posição -> Posição -> Maybe Posição
talvezMetade a c =
    let
        ( b1, b2 ) =
            metade a c

        ( a1, a2 ) =
            a

        distância =
            (b1 - a1) ^ 2 + (b2 - a2) ^ 2
    in
        if distância /= 1 then
            Nothing
        else
            Just ( b1, b2 )


metade : Posição -> Posição -> Posição
metade ( a1, a2 ) ( c1, c2 ) =
    let
        b1 =
            c1 - a1

        b2 =
            c2 - a2
    in
        ( a1 + b1 / 2, a2 + b2 / 2 )


dentroDoTabuleiro : Posição -> Bool
dentroDoTabuleiro ( i, j ) =
    if i < 0 || j < 0 || i > 14 || j > 14 then
        False
    else if i < 5 || i >= 10 then
        j > 4 && j < 10
    else
        True


ocupada : Posição -> Tabuleiro -> Bool
ocupada ( i, j ) tabuleiro =
    if not <| dentroDoTabuleiro ( i, j ) then
        False
    else
        let
            casa =
                Matrix.get i j tabuleiro
        in
            case casa of
                Nothing ->
                    False

                Just Vazia ->
                    False

                Just Pedra ->
                    True


inserir : Posição -> Tabuleiro -> Tabuleiro
inserir ( i, j ) tabuleiro =
    Matrix.set i j Pedra


remover : Posição -> Tabuleiro -> Tabuleiro
remover ( i, j ) tabuleiro =
    Matrix.set i j Vazia
