module Main exposing (..)
import Http exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Browser


-- Main

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }


api: String
api = "https://golex.herokuapp.com/"

-- Model

type alias Token =
  { type_ : String
  , value : String
  , position : Int
  }

type alias Error =
  { errors : List String }

type alias Model =
  { listOfTokens : List Token
  , listOfErrors : List String
  , code : String
  }

initialCode =
  """package main

import \"fmt\"

func sum(x int, y int) int {
    return x + y
}

func main(){
    fmt.Println(sum(4,7))
}"""

init: () -> (Model, Cmd msg)
init _= 
  ( Model [] [] initialCode
  , Cmd.none
  )

-- Update

type Msg
  = PostCode (Result Http.Error (List Token))
  | Analyze String
  | Change String

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    PostCode (Ok result) ->
          ( { model | listOfTokens = result } --ListOfToken TODO
          , Cmd.none
          )
    PostCode (Err _) ->
          ( model
          , Cmd.none)


    Analyze code ->
      ( model
      , sendPostCode code)

    Change code ->
      ( {model | code = code }
      , Cmd.none)


-- Decoders / Encoders

tokenDecoder =
  Decode.map3 Token
    (Decode.field "type" Decode.string)
    (Decode.field "value" Decode.string)
    (Decode.field "position" Decode.int)

listOfTokensDecoder = 
  Decode.list tokenDecoder

encodedCode code =
    Encode.object [ ("code", Encode.string code) ]


-- HTTP requests
                                                 
postCode code = 
  Http.post api (Http.jsonBody (encodedCode code) ) listOfTokensDecoder

sendPostCode code =
  Http.send PostCode (postCode code)


-- Views

tokenRow token =
  tr []
    [ td [] [text token.type_]
    , td [] [text token.value]
    , td [] [text (String.fromInt(token.position))]
    ]
  

view model =
  div [class "elm-container", id "lexical-analyzer"] 
    [ div [class "analyzer-input"]
      [ textarea [class "scroll", onInput Change, spellcheck False] [ text model.code ]
      , button [onClick (Analyze model.code)] [ text "Analyze" ]
      ]
    , div [class "analyzer-output scroll"]
      [
      table []
        [ 
          thead [] 
          [ th [] [ text "Type" ]
          , th [] [ text "Value" ]
          , th [] [ text "Position" ]
          ],
          tbody [] (List.map tokenRow model.listOfTokens)
        ]
      ]     
    ]

-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none
