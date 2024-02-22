module DataTable exposing
    ( ColumnMeta
    , EditorVisible(..)
    , rowControlViews
    , table
    )

import Bulma.Classes as B
import Css exposing (..)
import Grid
import Html.Styled as Html exposing (Html, button, div, span, text)
import Html.Styled.Attributes as Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Loading
import Views exposing (IconKind(..))


type EditorVisible
    = Hide
    | ShowForNew
    | ShowForUpdate


type alias ColumnMeta row msg =
    { widthClass : String
    , label : String
    , html : row -> Html msg
    }


type alias FetchParam msg =
    Grid.FetchParam msg


type alias FilterParam msg =
    Grid.FilterParam msg


type alias EditorParam row msg =
    { showEditorForNew : msg
    , showEditorForUpdate : row -> msg
    , editorVisible : EditorVisible
    , editor :
        ()
        -> List (Html msg) -- Elmは遅延実行がないので無駄な関数実行を防ぐ.
    , hideEditor : msg
    }


type alias UpdateParam row msg =
    { confirmDelete : row -> msg
    , checkInputForNew : Bool
    , clickSave : msg
    , deleteDialogVisible : Bool
    , deleteDialog : () -> List (Html msg)
    , deleteCancel : msg
    , deleteOk : msg
    , loading : Bool
    }


table :
    List (ColumnMeta row msg)
    -> FetchParam msg
    -> FilterParam msg
    -> EditorParam row msg
    -> UpdateParam row msg
    -> (row -> String)
    -> List row
    -> Html msg
table columns fetchP filterP editorP updateP rowId datas =
    div [] <|
        Grid.grid
            (List.map
                (\c ->
                    { widthClass = c.widthClass
                    , label = c.label
                    , html = c.html
                    , clickable = True
                    }
                )
                columns
                ++ [ { widthClass = B.is2
                     , label = ""
                     , html =
                        \row ->
                            div []
                                [ Views.iconButton Pen [ onClick <| editorP.showEditorForUpdate row, css [ border zero ] ]
                                , Views.iconButton Trash [ onClick <| updateP.confirmDelete row, class B.isDanger, class B.isInverted, css [ border zero ] ]
                                ]
                     , clickable = False
                     }
                   ]
            )
            fetchP
            filterP
            { leftTopContent =
                [ Views.iconButton Plus
                    [ class B.isDark
                    , onClick editorP.showEditorForNew
                    ]
                ]
            , onRowClick = editorP.showEditorForUpdate
            , rowId = rowId
            }
            datas
            ++ (case editorP.editorVisible of
                    Hide ->
                        []

                    _ ->
                        let
                            cc =
                                [ marginTop (rem 0.8), Css.height (rem (4 - 0.8 * 2)) ]
                        in
                        [ Views.dialog
                            { body = editorP.editor ()
                            , headerElements = []
                            , footerElements =
                                [ button
                                    [ type_ "button"
                                    , Views.concatClass [ B.button, B.isLink ]
                                    , Attributes.disabled <| not <| updateP.checkInputForNew
                                    , onClick updateP.clickSave
                                    , css <| [ marginLeft (rem 1), marginRight (px 4) ] ++ cc
                                    ]
                                    (if updateP.loading then
                                        [ Html.fromUnstyled <| Loading.render Loading.DoubleBounce Loading.defaultConfig Loading.On ]

                                     else
                                        [ Views.icon Save
                                        , span [] [ text "保存" ]
                                        ]
                                    )
                                , button
                                    [ type_ "button"
                                    , Views.concatClass [ B.button ]
                                    , onClick editorP.hideEditor
                                    , css cc
                                    ]
                                    [ text "キャンセル" ]
                                ]
                            }
                        ]
               )
            ++ (if updateP.deleteDialogVisible then
                    [ Views.dialogConfirmation
                        { title = "削除の確認"
                        , body = updateP.deleteDialog ()
                        , cancel = updateP.deleteCancel
                        , ok = updateP.deleteOk
                        }
                    ]

                else
                    []
               )


rowControlViews :
    { showEditorForNew : msg
    , showEditorForUpdate : row -> msg
    , confirmDelete : row -> msg
    }
    ->
        { new : Html msg
        , update : row -> Html msg
        , delete : row -> Html msg
        }
rowControlViews { showEditorForNew, showEditorForUpdate, confirmDelete } =
    { new =
        Views.iconButton Plus
            [ class B.isDark
            , onClick showEditorForNew
            ]
    , update = \row -> Views.iconButton Pen [ onClick <| showEditorForUpdate row, css [ border zero ] ]
    , delete = \row -> Views.iconButton Trash [ onClick <| confirmDelete row, class B.isDanger, class B.isInverted, css [ border zero ] ]
    }
