module Grid exposing (..)

import Bulma.Classes as B
import Css exposing (..)
import Html.Styled as Html exposing (Attribute, Html, button, div, span, text)
import Html.Styled.Attributes as Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Maybe.Extra as Maybe
import Styles
import Views exposing (IconKind(..))


type alias PagingParam =
    { limit : Int
    , nextToken : Maybe String
    }


type alias ColumnMeta row msg =
    { widthClass : String
    , label : String
    , html : row -> Html msg
    , clickable : Bool
    }


type alias FetchParam msg =
    { pagingParam : PagingParam
    , fetchCountList : List Int
    , changeFetchCount : String -> msg
    , onLoadAddition : msg
    }


type alias FilterParam msg =
    { filterText : String
    , changeFilterText : String -> msg
    }


type alias ViewParam row msg =
    { leftTopContent : List (Html msg)
    , onRowClick : row -> msg
    }


grid :
    List (ColumnMeta row msg)
    -> FetchParam msg
    -> FilterParam msg
    -> ViewParam row msg
    -> List row
    -> List (Html msg)
grid columns fetchP filterP viewP datas =
    viewTop
        { leftTopContent = viewP.leftTopContent
        , changeFilterText = filterP.changeFilterText
        , filterText = filterP.filterText
        }
        :: viewHeader columns
        :: List.map (viewRow columns viewP.onRowClick) datas
        ++ [ viewFooter fetchP ]


viewFooter : FetchParam msg -> Html msg
viewFooter fetchP =
    div [ class B.level ]
        [ div [ class B.levelLeft ] []
        , div [ class B.levelRight ]
            [ div [ class B.levelItem ]
                [ Views.select
                    { value = Just fetchP.pagingParam.limit
                    , values = fetchP.fetchCountList
                    , valueToString = String.fromInt
                    , valueToLabel = String.fromInt
                    , handler = fetchP.changeFetchCount
                    , attributes = []
                    }
                ]
            , div [ class B.levelItem ]
                [ button
                    [ type_ "button"
                    , class B.button
                    , onClick fetchP.onLoadAddition
                    , Attributes.disabled <| Maybe.isNothing fetchP.pagingParam.nextToken
                    ]
                    [ Views.icon Download, span [] [ text "更に読み込む" ] ]
                ]
            ]
        ]


viewTop :
    { leftTopContent : List (Html msg)
    , changeFilterText : String -> msg
    , filterText : String
    }
    -> Html msg
viewTop { leftTopContent, changeFilterText, filterText } =
    div [ class B.level, css [ marginTop (px 10) ] ]
        [ div [ class B.levelLeft ] leftTopContent
        , div [ class B.levelRight, css [ Css.width (pct 40) ] ]
            [ div [ css [ Css.width (pct 100) ] ]
                [ span [ css [ fontSize (pct 150) ] ] [ Views.icon Filter ]
                , Views.inputUnderLine
                    [ onInput changeFilterText
                    , value filterText
                    , css [ display inline, Css.width (pct 90) ]
                    ]
                    []
                ]
            ]
        ]


viewRow :
    List (ColumnMeta row msg)
    -> (row -> msg)
    -> row
    -> Html msg
viewRow columns onRowClick row =
    div (css [ cursor pointer ] :: rowAttrs) <|
        List.map
            (\column ->
                div
                    ([ class B.column, class column.widthClass ]
                        ++ (if column.clickable then
                                [ onClick (onRowClick row) ]

                            else
                                []
                           )
                    )
                    [ column.html row ]
            )
            columns


rowAttrs : List (Attribute msg)
rowAttrs =
    [ class B.columns
    , css
        [ borderBottom3 (px 1) solid (rgba 30 30 30 0.3)
        , hover [ backgroundColor (rgba 255 0 0 0.1) ]
        ]
    ]


viewHeader : List (ColumnMeta row msg) -> Html msg
viewHeader columns =
    div (css Styles.th :: rowAttrs) <|
        List.map
            (\column ->
                div
                    [ class B.column
                    , class column.widthClass
                    , css
                        [ fontSize (rem 0.8)
                        , fontWeight bold
                        , color (rgb 150 150 150)
                        ]
                    ]
                    [ text column.label ]
            )
            columns
            ++ [ div [ class B.column, class B.is2 ] [] ]
