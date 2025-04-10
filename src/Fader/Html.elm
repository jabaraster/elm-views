module Fader.Html exposing (..)

import Html as H
import Html.Attributes as A
import Html.Events as E
import Json.Decode
import Process
import Task
import Util exposing (ListElement(..))


type FadeProperties msg
    = FadeProperties
        { hidden : Bool
        , animationReady : Bool
        , duration : String
        , onAfterShow : () -> msg
        , onHideEnd : msg
        }


new :
    Int
    -> (() -> msg)
    -> msg
    -> FadeProperties msg
new durationMillisec onAfterShow onHideEnd =
    FadeProperties
        { hidden = True
        , animationReady = False
        , duration = String.fromInt durationMillisec ++ "ms"
        , onAfterShow = onAfterShow
        , onHideEnd = onHideEnd
        }


apply :
    FadeProperties msg
    -> (List (H.Attribute msg) -> List (H.Html msg) -> H.Html msg)
    -> List (H.Attribute msg)
    -> List (H.Html msg)
    -> H.Html msg
apply fp tag attrs children =
    let
        (FadeProperties props) =
            fp
    in
    tag
        ((E.on "transitionend" <| Json.Decode.succeed props.onHideEnd)
            :: ariaHidden fp
            :: transition fp
            :: attrHidden fp
            ++ attrs
        )
        children


show :
    FadeProperties msg
    -> model
    -> (() -> msg)
    -> (model -> FadeProperties msg -> model)
    -> ( model, Cmd msg )
show fp model showHandler setter =
    let
        t =
            Process.sleep 0 |> Task.map (always ())

        (FadeProperties props) =
            fp

        newFp =
            FadeProperties { props | animationReady = True, hidden = True }
    in
    ( setter model newFp
    , Task.perform props.onAfterShow t
    )


hide :
    FadeProperties msg
    -> model
    -> (model -> FadeProperties msg -> model)
    -> ( model, Cmd msg )
hide fp model setter =
    let
        (FadeProperties props) =
            fp
    in
    ( setter model <| FadeProperties { props | animationReady = True, hidden = True }
    , Cmd.none
    )


afterShow :
    FadeProperties msg
    -> model
    -> (model -> FadeProperties msg -> model)
    -> ( model, Cmd msg )
afterShow fp model setter =
    let
        (FadeProperties props) =
            fp
    in
    ( setter model <| FadeProperties { props | animationReady = True, hidden = False }
    , Cmd.none
    )


hideEnd :
    FadeProperties msg
    -> model
    -> (model -> FadeProperties msg -> model)
    -> ( model, Cmd msg )
hideEnd fp model setter =
    let
        (FadeProperties props) =
            fp
    in
    ( if props.hidden then
        setter model <| FadeProperties { props | animationReady = False, hidden = True }

      else
        model
    , Cmd.none
    )


transition : FadeProperties msg -> H.Attribute msg
transition (FadeProperties props) =
    A.style "transition" <| "opacity " ++ props.duration ++ " ease-in-out"


ariaHidden : FadeProperties msg -> H.Attribute msg
ariaHidden (FadeProperties props) =
    A.attribute "aria-hidden" <|
        if props.hidden then
            "true"

        else
            "false"


attrHidden : FadeProperties msg -> List (H.Attribute msg)
attrHidden (FadeProperties props) =
    Util.buildList
        [ SingleIf (not props.animationReady) (\_ -> A.style "display" "none")
        , Single <|
            A.style "opacity" <|
                if props.hidden then
                    "0"

                else
                    "1"
        ]
