module RestaUm exposing (..)

import Matrix


-- Modelo


type Casa
    = Inexistente
    | Vazia
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


tabuleiroNovo =
    let
        casa i j _ =
            if ( i, j ) == ( 7, 7 ) then
                Vazia
            else if dentroDoTabuleiro ( i, j ) then
                Pedra
            else
                Inexistente
    in
        Matrix.repeat 15 15 Inexistente
            |> Matrix.indexedMap casa


modelo =
    Modelo tabuleiroNovo Nothing



-- Atualização


type Ação
    = Escolher Posição
    | Ocupar Posição


atualizar : Ação -> Modelo -> Modelo
atualizar ação modelo =
    case ação of
        Escolher posição ->
            escolher posição modelo

        Ocupar posição ->
            case modelo.escolhida of
                Nothing ->
                    modelo

                Just escolhida ->
                    let
                        jogada =
                            { origem = escolhida, destino = posição }
                    in
                        { tabuleiro = jogar jogada modelo.tabuleiro, escolhida = Nothing }


escolher : Posição -> Modelo -> Modelo
escolher posição modelo =
    if ocupada posição modelo.tabuleiro then
        { modelo | escolhida = Just posição }
    else
        modelo


jogar : Jogada -> Tabuleiro -> Tabuleiro
jogar jogada tabuleiro =
    if not (jogadaVálida jogada tabuleiro) then
        tabuleiro
    else
        let
            { origem, destino } =
                jogada

            casaDoMeio =
                entre origem destino
        in
            tabuleiro
                |> remover origem
                |> remover casaDoMeio
                |> inserir destino


jogadaVálida : Jogada -> Tabuleiro -> Bool
jogadaVálida { origem, destino } t =
    let
        meio =
            talvezEntre origem destino
    in
        case meio of
            Nothing ->
                False

            Just casaDoMeio ->
                ocupada origem t && ocupada casaDoMeio t && not (ocupada destino t)


talvezEntre : Posição -> Posição -> Maybe Posição
talvezEntre a b =
    let
        distância ( x1, x2 ) ( y1, y2 ) =
            (y1 - x1) ^ 2 + (y2 - x2) ^ 2
    in
        if distância a b /= 4 then
            Nothing
        else
            Just (entre a b)


entre : Posição -> Posição -> Posição
entre ( a1, a2 ) ( b1, b2 ) =
    let
        ( dx, dy ) =
            ( b1 - a1, b2 - a2 )
    in
        ( a1 + dx // 2, a2 + dy // 2 )


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
        case Matrix.get i j tabuleiro of
            Just Pedra ->
                True

            _ ->
                False


inserir : Posição -> Tabuleiro -> Tabuleiro
inserir ( i, j ) =
    Matrix.set i j Pedra


remover : Posição -> Tabuleiro -> Tabuleiro
remover ( i, j ) =
    Matrix.set i j Vazia
