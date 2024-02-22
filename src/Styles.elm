module Styles exposing (..)

import Css exposing (..)


th : List Style
th =
    [ fontSize (rem 0.8)
    , fontWeight bold
    , color (rgb 150 150 150)
    ]


label : List Style
label =
    [ fontSize (pct 90)
    , fontWeight bolder
    , color (rgb 90 150 90)
    , display block
    , marginTop (em 0.5)
    ]
