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


jogadaVálida : Jogada -> Bool


inserir : Posição -> Tabuleiro -> Tabuleiro
inserir ( i, j ) tabuleiro =
    Matrix.set i j Pedra


remover : Posição -> Tabuleiro -> Tabuleiro
remover ( i, j ) tabuleiro =
    Matrix.set i j Vazia
