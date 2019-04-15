module Jabara.Views exposing
    ( fas
    , fas_
    )

{-| This module provides some views.


# Font Awesome Icon.

@docs fas, fas_

-}

import Html exposing (..)
import Html.Attributes exposing (..)


{-| Font Awesome Icon with class attributes.
Please refer Font Awesome css from html file.

    button [] [ fas_ "class-button-icon" "sync" ]

-}
fas : String -> String -> Html msg
fas clsName iconName =
    i [ class <| clsName ++ " fas fa-" ++ iconName ] []


{-| Font Awesome Icon without class attributes.
Please refer Font Awesome css from html file.

    div [] [ input [] [], fas "sync" ]

-}
fas_ : String -> Html msg
fas_ iconName =
    i [ class <| "fas fa-" ++ iconName ] []
