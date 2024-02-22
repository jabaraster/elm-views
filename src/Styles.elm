module Styles exposing
    ( label
    , th
    )

{-| common styles.

@docs label
@docs th

-}

import Css exposing (..)


{-| common th style.
-}
th : List Style
th =
    [ fontSize (rem 0.8)
    , fontWeight bold
    , color (rgb 150 150 150)
    ]


{-| common label style.
-}
label : List Style
label =
    [ fontSize (pct 90)
    , fontWeight bolder
    , color (rgb 90 150 90)
    , display block
    , marginTop (em 0.5)
    ]
