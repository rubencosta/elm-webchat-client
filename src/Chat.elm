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
    { chats =
        [   { id = 1
            , user =
                { id = 2
                , profilePicture = "https://avatars0.githubusercontent.com/u/7922109?v=3&s=460"
                , name = "Ryan Clark"
                }
            , messages=
                [   { contents = "Hey!"
                    , from = 2
                    , timestamp = 1424469793023
                    }
                ,   { contents = "Hey, what's up?"
                    , from = 1
                    , timestamp = 1424469794000
                    }
                ]
            }
        ,   { id = 2
            , user =
              { id = 3
              , profilePicture = "https://avatars3.githubusercontent.com/u/2955483?v=3&s=460"
              , name = "Jilles Soeters"
              }
            , messages=
              [   { contents = "Want a game of ping pong?"
                  , from = 3
                  , timestamp = 1424352522000
                  }
              ]
            }
        ,    { id = 3
                    , user =
                      { id = 4
                      , profilePicture = "https://avatars1.githubusercontent.com/u/1655968?v=3&s=460"
                      , name = "JTodd Motto"
                      }
                    , messages=
                      [   { contents = "Please follow me on twitter I'll pay you"
                          , from = 4
                          , timestamp = 1424423579000
                          }
                      ]
                    }
        ]
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

        UpdateOpenChatID id -> { model | openChatID <- id }

        NewMessage contents timestamp ->
            let updateChat chat =
                if chat.id == model.openChatID
                then {chat | messages <- chat.messages ++ [newMessage contents timestamp] }
                else chat
            in
                { model |
                    chats <- List.map updateChat model.chats
                }


---- VIEW ----
title : Html
title = text "chats"
chatListItem : Chat -> Html
chatListItem chat = a
           [ class "mdl-navigation__link"
           ]
           [ text chat.user.name
           ]

chatList : List Html
chatList = List.map (\chat -> chatListItem chat) model.chats

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
            chatList
        ]
    ]
    --}
