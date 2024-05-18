module Util exposing (ListElement(..), buildList, removeIf)

{-| utility.

@docs removeIf
@docs ListElement
@docs buildList

-}



{-| remove list element.
|
-}
removeIf : (a -> Bool) -> List a -> List a
removeIf pred =
    List.filter (not << pred)


{-| list concat utility.
|
-}
type ListElement a
    = Single a
    | SingleIf Bool (() -> a)
    | Plural (List a)
    | PluralIf Bool (() -> List a)
    | If Bool (() -> ListElement a) (() -> ListElement a)
    | Empty


{-| list concat utility.
|
-}
buildList : List (ListElement a) -> List a
buildList elems =
    List.foldr
        (\elem acc ->
            case elem of
                Single a ->
                    a :: acc

                SingleIf b a ->
                    if b then
                        a () :: acc

                    else
                        acc

                Plural m ->
                    m ++ acc

                PluralIf b a ->
                    if b then
                        a () ++ acc

                    else
                        acc

                If b thenOpe elseOpe ->
                    let
                        es =
                            if b then
                                buildList [ thenOpe () ]

                            else
                                buildList [ elseOpe () ]
                    in
                    es ++ acc

                Empty ->
                    acc
        )
        []
        elems
