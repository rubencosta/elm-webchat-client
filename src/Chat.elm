module Chat where

import Html exposing (..)
import Html.Attributes exposing (..)
import Time exposing (..)
import Signal exposing (Address)
import Window
import StartApp

main =
  StartApp.start { model = model, view = view, update = update }


---- MODEL ----

type alias Model =
    { chats : List Chat
    , openChatID : Int
    }

type alias Chat =
    { id : Int
    , user : User
    , messages: List Message
    }

type alias Message =
    { from : Int
    , contents : String
    , timestamp : Float
    }

type alias User =
    { id : Int
    , profilePicture : String
    , name : String
    }

model: Model
model =
    { chats = []
    , openChatID = 1
    }

newMessage: String -> Time -> Message
newMessage contents timestamp =
    { from = 1
    , contents = contents
    , timestamp = timestamp
    }

---- UPDATE ----

type Action
    = NoOp
    | UpdateOpenChatID Int
    | NewMessage String Time

update : Action -> Model -> Model
update action model =
    case action of
        NoOp -> model

        UpdateOpenChatID id ->
                    { model |
                        openChatID <- id
                    }

        NewMessage contents timestamp ->
            let updateChat chat = if chat.id == model.openChatID then {chat | messages <- chat.messages ++ [newMessage contents timestamp] } else chat
            in
            {   model |
                    chats <- List.map updateChat model.chats
            }



---- VIEW ----
title : Html
title = text "chats"
link : Html
link = text "link"

view : Address Action -> Model -> Html
view address model =
    {--}
    div
    [ class "mdl-layout mdl-js-layout mdl-layout--fixed-drawer mdl-layout--overlay-drawer-button"
    ]
    [ div
        [ class "mdl-layout__drawer"
        ]
        [ span
            [ class "mdl-layout-title"
            ]
            [ title
            ]
        , nav
            [ class "mdl-navigation"
            ]
            [ a
                [ class "mdl-navigation__link"
                ]
                [ link
                ]
            ]
        ]
    ]
    --}
