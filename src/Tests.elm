{-| The css [calc](https://developer.mozilla.org/en/docs/Web/CSS/calc) function.

    almostPct100 =
        calc (pct 100) minus (px 2)

    -- calc(100vh - (2px + 2rem))
    screenMinusBorderAndFooter =
        calc (vh 100) minus (calc (px 2) plus (rem 2))

    myWidth =
        width almostPct100

    myHeight =
        height screenMinusBorderAndFooter

Using \* and / with calc isn't supported. Use arithmetics from elm instead.

-}
calc : Calc compatibleA -> CalcExpression -> Calc compatibleB -> CalculatedLength
calc firstExpr expression secondExpr =
    let
        withoutCalcStr l =
            if String.startsWith "calc(" l.value then
                String.dropLeft 4 l.value

            else
                l.value

        calcs =
            withoutCalcStr firstExpr
                ++ " "
                ++ calcExpressionToString expression
                ++ " "
                ++ withoutCalcStr secondExpr

        value =
            cssFunction "calc" [ calcs ]
    in
    { value = value
    , length = Compatible
    , lengthOrAuto = Compatible
    , lengthOrNumber = Compatible
    , lengthOrNone = Compatible
    , lengthOrMinMaxDimension = Compatible
    , lengthOrNoneOrMinMaxDimension = Compatible
    , textIndent = Compatible
    , flexBasis = Compatible
    , lengthOrNumberOrAutoOrNoneOrContent = Compatible
    , fontSize = Compatible
    , lengthOrAutoOrCoverOrContain = Compatible
    , lineHeight = Compatible
    , calc = Compatible
    }


{-| Use with calc to add lengths together

    >>> calc (pct 100) plus (px 2)
    calc (100% + 2px)

-}
plus : CalcExpression
plus =
    Addition


{-| Use with calc to subtract lengths from eachother

    >>> calc (pct 100) minus (px 2)
    calc (100% - 2px)

-}
minus : CalcExpression
minus =
    Subtraction


combineLengths :
    (Float -> Float -> Float)
    -> { r | numericValue : Float, unitLabel : String, value : String }
    -> { r | numericValue : Float, unitLabel : String, value : String }
    -> { r | numericValue : Float, unitLabel : String, value : String }
combineLengths operation firstLength secondLength =
    let
        numericValue =
            operation firstLength.numericValue secondLength.numericValue

        value =
            String.fromFloat numericValue ++ firstLength.unitLabel
    in
    { firstLength | value = value, numericValue = numericValue }


{-| <https://developer.mozilla.org/en-US/docs/Web/CSS/length>
-}
type alias LengthOrAuto compatible =
    { compatible | value : String, lengthOrAuto : Compatible }


{-| <https://developer.mozilla.org/en-US/docs/Web/CSS/length>
-}
type alias LengthOrNoneOrMinMaxDimension compatible =
    { compatible | value : String, lengthOrNoneOrMinMaxDimension : Compatible }


{-| <https://developer.mozilla.org/en-US/docs/Web/CSS/length>
-}
type alias LengthOrMinMaxDimension compatible =
    { compatible | value : String, lengthOrMinMaxDimension : Compatible }


{-| <https://developer.mozilla.org/en-US/docs/Web/CSS/length>
-}
type alias LengthOrNone compatible =
    { compatible | value : String, lengthOrNone : Compatible }


{-| <https://developer.mozilla.org/en-US/docs/Web/CSS/length>
-}
type alias LengthOrNumber compatible =
    { compatible | value : String, lengthOrNumber : Compatible }


{-| -}
type alias ExplicitLength units =
    Css.Internal.ExplicitLength units


{-| <https://developer.mozilla.org/en-US/docs/Web/CSS/transform#Values>
-}
type alias Transform compatible =
    { compatible | value : String, transform : Compatible }


{-| <https://developer.mozilla.org/en-US/docs/Web/CSS/angle>
-}
type alias Angle compatible =
    { compatible | value : String, angle : Compatible }


{-| <https://developer.mozilla.org/en-US/docs/Web/CSS/linear-gradient#Values>
-}
type alias AngleOrDirection compatible =
    { compatible | value : String, angleOrDirection : Compatible }


{-| <https://developer.mozilla.org/en-US/docs/Web/CSS/text-decoration-style#Values>
-}
type alias TextDecorationStyle compatible =
    { compatible | value : String, textDecorationStyle : Compatible }


{-| <https://developer.mozilla.org/en-US/docs/Web/CSS/text-emphasis-color#Values>
-}
type alias TextEmphasisColor compatible =
    { compatible | value : String, textDecorationStyle : Compatible }


{-| <https://developer.mozilla.org/en-US/docs/Web/CSS/position#Values>
-}
type alias Position compatible =
    { compatible | value : String, position : Compatible }


{-| <https://developer.mozilla.org/en-US/docs/Web/CSS/border-style#Values>
-}
type alias BorderStyle compatible =
    { compatible | value : String, borderStyle : Compatible }


{-| <https://developer.mozilla.org/en-US/docs/Web/CSS/border-collapse>
-}
type alias BorderCollapse compatible =
    { compatible | value : String, borderCollapse : Compatible }


{-| <https://developer.mozilla.org/en-US/docs/Web/CSS/transform-box#Values>
-}
type alias TransformBox compatible =
    { compatible | value : String, transformBox : Compatible }


{-| <https://developer.mozilla.org/en-US/docs/Web/CSS/text-orientation#Values>
-}
type alias TextOrientation compatible =
    { compatible | value : String, textOrientation : Compatible }


{-| <https://developer.mozilla.org/en-US/docs/Web/CSS/transform-style#Values>
-}
type alias TransformStyle compatible =
    { compatible | value : String, transformStyle : Compatible }


{-| <https://developer.mozilla.org/en-US/docs/Web/CSS/text-indent#Values>
-}
type alias TextIndent compatible =
    { compatible | value : String, textIndent : Compatible }


{-| <https://developer.mozilla.org/en-US/docs/Web/CSS/text-overflow#Values>
-}
type alias TextOverflow compatible =
    { compatible | value : String, textOverflow : Compatible }


{-| <https://developer.mozilla.org/en-US/docs/Web/CSS/text-transform#Values>
-}
type alias TextTransform compatible =
    { compatible | value : String, textTransform : Compatible }


{-| <https://developer.mozilla.org/en-US/docs/Web/CSS/text-rendering#Values>
-}
type alias TextRendering compatible =
    { compatible | value : String, textRendering : Compatible }


{-| <https://www.microsoft.com/typography/otspec/featurelist.htm>
-}
type alias FeatureTagValue compatible =
    { compatible | value : String, featureTagValue : Compatible }


{-| Because `left` and `right` are both common properties and common values
in CSS (e.g. `left: 5px` with `position: absolute` and `text-align: left`),
we implement it as a property (for the `left: 5px` case) and allow it to
be used as a value as well. When being used as a value, we call it, expect
that it will return the desired String as its key, and use that as our value.
(See `getOverloadedProperty`. Note that `VerticalAlign` follows a similar pattern.)
-}
type alias TextAlign a b =
    Length a b -> Style


{-| Because `top` and `bottom` are both common properties and common values
in CSS (e.g. `top: 5px` with `position: absolute` and `vertical-align: top`),
we implement it as a property (for the `top: 5px` case) and allow it to
be used as a value as well. When being used as a value, we call it, expect
that it will return the desired String as its key, and use that as our value.
(See `getOverloadedProperty`. Note that `TextAlign` follows a similar pattern.)
-}
type alias VerticalAlign a b =
    Length a b -> Style


{-| <https://developer.mozilla.org/en-US/docs/Web/CSS/cursor#Values>
-}
type alias Cursor compatible =
    { compatible | value : String, cursor : Compatible }


{-| <https://developer.mozilla.org/en-US/docs/Web/CSS/outline#Values>
-}
type alias Outline compatible =
    { compatible | value : String, outline : Compatible }



-- Properties --


{-| An [`all`](https://developer.mozilla.org/en-US/docs/Web/CSS/all) property.
-}
all : All compatible -> Style
all =
    prop1 "all"


{-| Transforms the given property by adding !important to the end of its
declaration.
-}
important : Style -> Style
important =
    Preprocess.mapProperties makeImportant


makeImportant : Property -> Property
makeImportant (Property str) =
    if String.endsWith " !important" (String.toLower str) then
        Property str

    else
        Property (str ++ " !important")


{-| A [`ColorValue`](#ColorValue) that does not have `red`, `green`, or `blue`
values.
-}
type alias NonMixable =
    {}


{-| A [`transparent`](https://developer.mozilla.org/en-US/docs/Web/CSS/color_value#transparent_keyword) color.
-}
transparent : ColorValue NonMixable
transparent =
    { value = "transparent"
    , color = Compatible
    }


{-| The [`currentColor`](https://developer.mozilla.org/en-US/docs/Web/CSS/color_value#currentColor_keyword)
value.
-}
currentColor : ColorValue NonMixable
currentColor =
    { value = "currentColor"
    , color = Compatible
    }


{-| This can represent:
a `visible` [`visibility`](https://developer.mozilla.org/en-US/docs/Web/CSS/visibility#Values),
a `visible` [`overflow`](https://developer.mozilla.org/en-US/docs/Web/CSS/overflow#Values), or
a `visible` [`pointer-events`](https://developer.mozilla.org/en-US/docs/Web/CSS/pointer-events) value.
-}
visible :
    { value : String
    , overflow : Compatible
    , visibility : Compatible
    , pointerEvents : Compatible
    }
visible =
    { value = "visible"
    , overflow = Compatible
    , visibility = Compatible
    , pointerEvents = Compatible
    }


{-| The `scroll` [`overflow`](https://developer.mozilla.org/en-US/docs/Web/CSS/overflow#Values) value.
This can also represent a `scroll` [`background-attachment`](https://developer.mozilla.org/en-US/docs/Web/CSS/background-attachment) value.
It can also be used in the overflow-block and oveflow-line media features.
-}
scroll :
    { value : String
    , scroll : Compatible
    , overflow : Compatible
    , backgroundAttachment : Compatible
    , blockAxisOverflow : Compatible
    , inlineAxisOverflow : Compatible
    }
scroll =
    { value = "scroll"
    , scroll = Compatible
    , overflow = Compatible
    , backgroundAttachment = Compatible
    , blockAxisOverflow = Compatible
    , inlineAxisOverflow = Compatible
    }


{-| The `break-word` value for the [`overflow-wrap`](https://developer.mozilla.org/en/docs/Web/CSS/overflow-wrap#Values) property.
-}
breakWord : Wrap {}
breakWord =
    { value = "break-word"
    , overflowWrap = Compatible
    }


{-| The `both` value for the [`resize`](https://developer.mozilla.org/en/docs/Web/CSS/resize#Values) property.
-}
both : Resize {}
both =
    { value = "both"
    , resize = Compatible
    }


{-| The `horizontal` value for the [`resize`](https://developer.mozilla.org/en/docs/Web/CSS/resize#Values) property.
-}
horizontal : Resize {}
horizontal =
    { value = "horizontal"
    , resize = Compatible
    }


{-| The `vertical` value for the [`resize`](https://developer.mozilla.org/en/docs/Web/CSS/resize#Values) property.
-}
vertical : Resize {}
vertical =
    { value = "vertical"
    , resize = Compatible
    }


{-| The `multiply` [`blend-mode`](https://developer.mozilla.org/en-US/docs/Web/CSS/blend-mode#multiply).
-}
multiply : BackgroundBlendMode compatible
multiply =
    prop1 "multiply"


{-| The `screen` [`blend-mode`](https://developer.mozilla.org/en-US/docs/Web/CSS/blend-mode#screen).
-}
screenBlendMode : BackgroundBlendMode compatible
screenBlendMode =
    prop1 "screen"
