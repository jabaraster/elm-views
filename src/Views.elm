module Views exposing
    ( IconKind(..)
    , InputArg
    , SelectedFile
    , ViewElement(..)
    , backdrop
    , build
    , concatClass
    , defaultInputArg
    , defaultTextAreaArg
    , dialog
    , dialogConfirmation
    , icon
    , iconButton
    , iconButtonText
    , iconS
    , imageSelector
    , input
    , input2
    , inputUnderLine
    , oneColumn
    , oneColumnNoTBMargin
    , oneColumnNoTopMargin
    , select
    , submitter
    , switch
    , textArea
    , twoColumns
    )

{-| view components.


# view components

@docs IconKind
@docs InputArg
@docs SelectedFile
@docs ViewElement
@docs backdrop
@docs build
@docs concatClass
@docs defaultInputArg
@docs defaultTextAreaArg
@docs dialog
@docs dialogConfirmation
@docs icon
@docs iconButton
@docs iconButtonText
@docs iconS
@docs imageSelector
@docs input
@docs input2
@docs inputUnderLine
@docs oneColumn
@docs oneColumnNoTBMargin
@docs oneColumnNoTopMargin
@docs select
@docs submitter
@docs switch
@docs textArea
@docs twoColumns

-}

import Bulma.Classes as B
import Bulma.Helpers
import Css exposing (..)
import Css.Transitions as Transition
import File exposing (File)
import Html.Styled as H
    exposing
        ( Attribute
        , Html
        , a
        , button
        , div
        , footer
        , h2
        , header
        , i
        , img
        , label
        , li
        , node
        , span
        , styled
        , text
        , textarea
        , ul
        )
import Html.Styled.Attributes as A exposing (..)
import Html.Styled.Events exposing (..)
import Html.Styled.Lazy exposing (..)
import Loading


{-| backdrop. ex) modal dialog back.
|
-}
backdrop : List (Attribute msg) -> List (Html msg) -> Html msg
backdrop =
    styled div
        [ backgroundColor (rgba 0 0 0 0.5)
        , position fixed
        , Css.width (vw 100)
        , Css.height (vh 100)
        , top zero
        , left zero
        , zIndex (int 100)
        ]


{-| 'input' function arguments.
|
-}
type alias InputArg msg =
    { value : String
    , label : String
    , placeholder : String
    , type_ : String
    , attributes : List (Attribute msg)
    }


{-| default input arg.
|
-}
defaultInputArg : InputArg msg
defaultInputArg =
    { value = ""
    , label = ""
    , placeholder = ""
    , type_ = "text"
    , attributes = []
    }


defaultInputWrapperClass : Attribute msg
defaultInputWrapperClass =
    class "default-input-wrapper"


{-| toggle button.
|
-}
switch : { value : Bool, onClick : msg } -> Html msg
switch arg =
    let
        height =
            24

        animeMs =
            150

        ( bg, pos ) =
            if arg.value then
                ( rgba 100 230 100 1, left (px <| height - 4) )

            else
                ( rgba 155 155 155 1, left (px -1) )
    in
    div
        [ css [ margin2 (px 6) (px 6), display inlineBlock ]
        , onClick arg.onClick
        ]
        [ div
            [ css
                [ backgroundColor bg
                , Css.width (px <| height * 1.8)
                , Css.height (px height)
                , borderRadius2 (px <| height / 2) (px <| height / 2)
                , cursor pointer
                , border3 (px 1) solid (rgba 0 0 0 0)
                , Transition.transition [ Transition.backgroundColor animeMs ]
                ]
            ]
            [ div
                [ css
                    [ Css.width (px height)
                    , Css.height (px height)
                    , position relative
                    , top (px -1)
                    , pos
                    , backgroundColor (rgba 255 255 255 1)
                    , borderRadius (pct 100)
                    , border3 (px 1) solid (rgba 0 0 0 0.3)
                    , boxSizing borderBox
                    , Transition.transition [ Transition.left animeMs ]
                    ]
                ]
                []
            ]
        ]


type alias Input2RequiredArg msg =
    { label : String
    , value : String
    , errors : List String
    , onInput : String -> msg
    }


type alias Input2OptionalArg msg =
    { type_ : String
    , disabled : Bool
    , customizer : List (Html msg) -> Html msg
    }


defaultInput2OptionalArg : Input2OptionalArg msg
defaultInput2OptionalArg =
    { type_ = "text"
    , disabled = False
    , customizer = div [ css [ display inlineBlock, Css.width (pct 100) ] ]
    }


{-| input component.
|
-}
input2 :
    Input2RequiredArg msg
    -> (Input2OptionalArg msg -> Input2OptionalArg msg)
    -> Html msg
input2 requiredArg optionalBuilder =
    let
        optionalArg =
            optionalBuilder defaultInput2OptionalArg

        inputElem =
            [ H.input
                ([ class "default-input"
                 , type_ optionalArg.type_
                 , placeholder "dummy"
                 , value requiredArg.value
                 , onInput requiredArg.onInput
                 , A.disabled optionalArg.disabled
                 ]
                    ++ (if List.isEmpty requiredArg.errors then
                            []

                        else
                            [ class "error" ]
                       )
                    ++ (if optionalArg.type_ == "date" then
                            [ css [ maxWidth (px 400) ] ]

                        else
                            []
                       )
                )
                []
            , H.label [ class "default-input-label" ] [ text requiredArg.label ]
            ]
    in
    div [ defaultInputWrapperClass ]
        [ optionalArg.customizer inputElem
        , errors requiredArg.errors
        ]


errors : List String -> Html msg
errors es =
    ul [ class "error" ] <| List.map (\et -> li [] [ text et ]) es


{-| input component.
|
-}
input : (InputArg msg -> InputArg msg) -> (String -> msg) -> Html msg
input argBuilder handler =
    let
        arg =
            argBuilder defaultInputArg

        attrs_ =
            [ class B.input
            , type_ arg.type_
            , placeholder arg.placeholder
            , value arg.value
            , onInput handler
            ]
                ++ arg.attributes
    in
    div []
        [ label [] [ text arg.label ]
        , H.input attrs_ []
        ]


type alias TextAreaArg msg =
    { value : String
    , label : String
    , placeholder : String
    , lineHeight : Int
    , attributes : List (Attribute msg)
    }


{-| default textarea arg.
-}
defaultTextAreaArg : TextAreaArg msg
defaultTextAreaArg =
    { value = ""
    , label = ""
    , placeholder = ""
    , lineHeight = 6
    , attributes = []
    }


{-| textarea component.
|
-}
textArea :
    (TextAreaArg msg -> TextAreaArg msg)
    -> (String -> msg)
    -> Html msg
textArea argBuilder handler =
    let
        arg =
            argBuilder defaultTextAreaArg

        attrs_ =
            [ placeholder arg.placeholder
            , class B.textarea
            , value arg.value
            , onInput handler
            ]
                ++ arg.attributes
    in
    div []
        [ label [] [ text arg.label ]
        , textarea attrs_ []
        ]


{-| underlined input component.
|
-}
inputUnderLine : List (Attribute msg) -> List (Html msg) -> Html msg
inputUnderLine attrs =
    let
        tag =
            styled H.input
                [ maxWidth inherit
                , borderStyle none
                , borderRadius zero
                , borderBottom3 (px 1) solid (rgba 0 0 0 0.5)
                , boxShadow none
                ]
    in
    tag <| class B.input :: attrs


{-| only one column row.
|
-}
oneColumn : Html msg -> Html msg
oneColumn tag =
    div [ class B.columns ] [ div [ class B.column ] [ tag ] ]


{-| one column no top margin.
|
-}
oneColumnNoTopMargin : Html msg -> Html msg
oneColumnNoTopMargin tag =
    div [ class B.columns, css [ marginTop zero ] ] [ div [ class B.column ] [ tag ] ]


{-| one column no top and bottom margin.
|
-}
oneColumnNoTBMargin : Html msg -> Html msg
oneColumnNoTBMargin tag =
    div [ class B.columns, css [ marginTop zero, marginBottom zero ] ] [ div [ class B.column ] [ tag ] ]


{-| two columns row.
|
-}
twoColumns : Html msg -> Html msg -> Html msg
twoColumns tag1 tag2 =
    div [ class B.columns ]
        [ div [ class B.column ] [ tag1 ]
        , div [ class B.column ] [ tag2 ]
        ]


{-| select component.
|
-}
select :
    { value : Maybe a
    , values : List a
    , valueToString : a -> String
    , valueToLabel : a -> String
    , handler : String -> msg
    , attributes : List (Attribute msg)
    }
    -> Html msg
select { value, values, valueToString, valueToLabel, handler, attributes } =
    H.select
        (attributes
            ++ [ onInput handler
               , A.value <| Maybe.withDefault "" <| Maybe.map valueToString value
               , class B.input
               ]
        )
    <|
        List.map
            (\optionValue ->
                let
                    valueS =
                        Maybe.withDefault "" <| Maybe.map valueToString value

                    optionValueS =
                        valueToString optionValue
                in
                H.option
                    [ A.value optionValueS
                    , selected <| valueS == optionValueS
                    ]
                    [ text <| valueToLabel optionValue ]
            )
            values


{-| concat class attributes.
|
-}
concatClass : List String -> Attribute msg
concatClass =
    A.fromUnstyled << Bulma.Helpers.classList


{-| icon kind. cf) font awesome
|
-}
type IconKind
    = Redo
    | Plus
    | Minus
    | Filter
    | Search
    | Pen
    | Trash
    | ArrowRight
    | Check
    | Save
    | Download
    | Eye
    | User
    | Home
    | Globe
    | ExternalLinkAlt
    | Image
    | Bars
    | WindowClose


{-| icon kind to name.
|
-}
iconName : IconKind -> String
iconName kind =
    case kind of
        Redo ->
            "redo"

        Plus ->
            "plus"

        Minus ->
            "minus"

        Filter ->
            "filter"

        Search ->
            "search"

        Pen ->
            "pen"

        Trash ->
            "trash"

        Check ->
            "check"

        ArrowRight ->
            "arrow-right"

        Save ->
            "save"

        Download ->
            "download"

        Eye ->
            "eye"

        User ->
            "user"

        Home ->
            "home"

        Globe ->
            "globe"

        ExternalLinkAlt ->
            "external-link-alt"

        Image ->
            "image"

        Bars ->
            "bars"

        WindowClose ->
            "window-close"


{-| only icon.
|
-}
icon : IconKind -> Html msg
icon s =
    iconS (iconName s)


{-| small icon.
|
-}
iconS : String -> Html msg
iconS s =
    span [ class B.icon ]
        [ i [ class <| "fas fa-" ++ s ] []
        ]


{-| button with icon.
|
-}
iconButton : IconKind -> List (Attribute msg) -> Html msg
iconButton kind attrs =
    iconCore (iconName kind) "" attrs


{-| button with icon and text.
|
-}
iconButtonText : IconKind -> String -> List (Attribute msg) -> Html msg
iconButtonText kind innerText attrs =
    iconCore (iconName kind) innerText attrs


{-| icon generator.
|
-}
iconCore : String -> String -> List (Attribute msg) -> Html msg
iconCore name innerText attrs =
    let
        attrs_ =
            css [ borderRadius (pct 50) ]
                -- B.isRoundedよりこちらの方がきれいな丸になる
                :: class B.button
                :: type_ B.button
                :: attrs
    in
    button attrs_
        [ iconS name
        , text innerText
        ]


{-| loading button.
|
-}
submitter : msg -> Bool -> String -> Html msg
submitter handler loading labelText =
    H.button
        [ type_ B.button
        , class B.button
        , onClick handler
        , A.disabled loading
        ]
        [ if loading then
            H.fromUnstyled <| Loading.render Loading.DoubleBounce Loading.defaultConfig Loading.On

          else
            text labelText
        ]


{-| 'build' function arguments.
-}
type ViewElement msg
    = Tag (Html msg)
    | Tags (List (Html msg))
    | Empty


{-| complexity tag builder.
-}
build : List (ViewElement msg) -> List (Html msg)
build =
    List.foldr
        (\elem list ->
            case elem of
                Tag a ->
                    a :: list

                Tags a ->
                    a ++ list

                Empty ->
                    list
        )
        []


{-| selected file struct.
-}
type alias SelectedFile =
    { file : Maybe File
    , url : String
    }


{-| image selector.
|
-}
imageSelector : msg -> Maybe SelectedFile -> Html msg
imageSelector handler mSelected =
    div
        [ css
            [ position relative
            , Css.minHeight (px 196)
            ]
        ]
    <|
        iconButton Image
            [ onClick handler
            , css
                [ display block
                , position absolute
                , top (px 2)
                , left (px 2)
                , zIndex (int 100)
                ]
            ]
            :: (case mSelected of
                    Nothing ->
                        []

                    Just file ->
                        [ img
                            [ src file.url
                            , css
                                [ position absolute
                                , top (px 2)
                                , left (px 2)
                                , maxHeight (calc (pct 100) minus (px 4))
                                , maxWidth (calc (pct 100) minus (px 4))
                                , border3 (px 1) solid (rgba 150 150 150 0.3)
                                ]
                            ]
                            []
                        ]
               )


{-| simple ok/cancel dialog.
|
-}
dialogConfirmation :
    { title : String
    , cancel : msg
    , ok : msg
    , body : List (Html msg)
    }
    -> Html msg
dialogConfirmation { title, cancel, ok, body } =
    dialog
        { body = body
        , headerElements = [ h2 [] [ text title ] ]
        , footerElements =
            [ button [ onClick cancel, css [ marginRight (px 2) ] ] [ text "キャンセル" ]
            , button [ class B.isDanger, onClick ok ] [ icon Check, span [] [ text "OK" ] ]
            ]
        }


{-| modal dialog by <dialog> tag.
-}
dialog :
    { body : List (Html msg)
    , headerElements : List (Html msg)
    , footerElements : List (Html msg)
    }
    -> Html msg
dialog { body, headerElements, footerElements } =
    node "modal-dialog"
        [ attribute "open" "" ]
        [ node "dialog"
            [ class "dialog" ]
            [ div [ class "modal-filler" ] []
            , header [ class "header" ] headerElements
            , div [ class "contents" ] body
            , footer [ class "footer" ] footerElements
            ]
        ]
