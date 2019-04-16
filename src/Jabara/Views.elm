module Jabara.Views exposing
    ( fas
    , fas_
    , fasCheck
    )

{-| This module provides some views.


# Font Awesome Icon.

@docs fas, fas_, fasCheck

-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


{-| Font Awesome Icon with class attributes.
Please refer Font Awesome css from html file.

    button [] [ fas "class-button-icon" "sync" ]

-}
fas : String -> String -> Html msg
fas clsName iconName =
    i [ class <| clsName ++ " fas fa-" ++ iconName ] []


{-| Font Awesome Icon without class attributes.
Please refer Font Awesome css from html file.

    div [] [ input [] [], fas_ "sync" ]

-}
fas_ : String -> Html msg
fas_ iconName =
    i [ class <| "fas fa-" ++ iconName ] []


{-| A little cool checkbox by Font Awesome Icon.
Please refer Font Awesome css from html file.

    View only:
        fasCheck True "Alwaiy Checked" Nothing

    With action:
        fasCheck True "Alwaiy Checked" (Just SomeMessage)
-}
fasCheck : Bool -> String -> Maybe msg -> Html msg
fasCheck checked label mAction =
            span
                ([ classList [("fas-check", True), ("clickable", isJust mAction)] ]
                    ++ (Maybe.withDefault [] <| Maybe.map (List.singleton  << onClick) mAction))
                [ fas
                    (if checked then
                        "true"

                     else
                        "false"
                    )
                    (if checked then
                        "check-circle"

                     else
                        "times-circle"
                    )
                , span [] [ text label ]
                ]

isJust : Maybe a -> Bool
isJust m =
    case m of
        Just _  -> True
        Nothing -> False