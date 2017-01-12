module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import RestaUm exposing (..)


escolher : Test
escolher =
    describe "Escolher uma casa"
        [ test "Casa com pedra" <|
            \() ->
                let
                    esperado =
                        { tabuleiro = tabuleiroNovo
                        , escolhida = Just ( 7, 1 )
                        }
                in
                    modelo
                        |> escolher ( 7, 1 )
                        |> Expect.equal esperado
        ]


all : Test
all =
    describe "Testes"
        [ escolher ]
