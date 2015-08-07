module Tictactoe where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe exposing (..)
import Matrix exposing (..)
import Debug exposing (..)
import StartApp

-- MODEL

type Player = X | Circle | Empty
type alias Model = { board : Matrix Player, currentPlayer : Player, winner : Player }

initialModel : Model
initialModel = 
    {
        board = Matrix.square 3 (\_ -> Empty),
        currentPlayer = X,
        winner = Empty
    }


togglePlayer : Player -> Player
togglePlayer currentPlayer =
    case currentPlayer of
        X ->
            Circle
        Circle ->
            X
        _ ->
            Empty


winningConditions : List (List Int)
winningConditions =
    [
        [0..2],
        [3..5],
        [6..8],
        [0,3,6],
        [1,4,7],
        [2,5,8],
        [0,4,8],
        [2,4,6]
    ]


isWinner : Matrix Player -> Player -> Bool
isWinner board player =
    let
        flatenned_board = Matrix.flatten board
        player_moves_indexes = flatenned_board
            |> List.indexedMap (\index list_item -> if list_item == player then index else -1)
            |> List.filter (\index -> index > -1)
            |> List.sort
        has_winning_conditions = winningConditions
            |> List.any (\winning_cond -> 
                List.all (\wc_item -> 
                    List.any (\i -> wc_item == i) player_moves_indexes ) winning_cond)
    in
        List.length player_moves_indexes >= 3 && has_winning_conditions


-- UPDATE

type Action
    = NoOp
    | Mark Location


update : Action -> Model -> Model
update action model = 
    case action of
        NoOp ->
            model
        Mark loc ->
            let
                maybe_value = Matrix.get loc model.board
                new_board = Matrix.set loc model.currentPlayer model.board
            in
                if maybe_value == Just Empty then
                    if isWinner new_board model.currentPlayer then
                        { model | 
                            board <- new_board,
                            winner <- model.currentPlayer }
                    else
                        { model | 
                            board <- new_board, 
                            currentPlayer <- togglePlayer model.currentPlayer }
                    
                else
                    model

-- VIEW

playerToString player =
    case player of
        X ->
            "X"
        Circle ->
            "O"
        _ ->
            ""


getLocFromId id =
    let
        row = id // 3
        column = id % 3
    in
        (row, column)


getTextFromCellId model loc =
    let
        maybe_value = Matrix.get loc model.board
    in
        maybe_value
            |> withDefault Empty
            |> playerToString
            |> text


cell address model id' = 
    let
        loc = getLocFromId id'
    in
        td [ class "cell", id ("cell_" ++ (toString id')), onClick address (Mark loc) ]
           [ getTextFromCellId model loc ]

line address model id = 
    tr [ ] (List.map (cell address model) [id..id+2])
       

view address model =
    let
        playerAsString = playerToString model.currentPlayer
    in
        div []
            [
                h1 [] [ text ("Current Player: " ++ playerAsString) ],
                h1 [] [ text (if model.winner /= Empty then "Winner " ++ playerAsString else "")],
                table [ ] (List.map (line address model) [0,3,6])
            ]    


-- WIRING THE APP

main : Signal Html
main =
    StartApp.start
    {
        model = initialModel,
        view = view,
        update = update
    }