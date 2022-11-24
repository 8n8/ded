cssFunction : String -> List String -> String
cssFunction funcName args =
    funcName
        ++ "("
        ++ String.join "," args
        ++ ")"


{-| -}
type alias Compatible =
    Structure.Compatible


{-| -}
type alias Value compatible =
    { compatible | value : String }


{-| -}
type alias All compatible =
    { compatible | value : String, all : Compatible }


{-| -}
type alias Number compatible =
    { compatible | value : String, number : Compatible }


{-| -}
type alias NumberOrInfinite compatible =
    { compatible | value : String, numberOrInfinite : Compatible }


{-| -}
type alias Infinite =
    { value : String
    , numberOrInfinite : Compatible
    }


{-| -}
type alias None compatible =
    { compatible | value : String, none : Compatible }


{-| -}
type alias MinMaxDimension compatible =
    { compatible
        | value : String
        , minMaxDimension : Compatible
        , lengthOrMinMaxDimension : Compatible
        , lengthOrNoneOrMinMaxDimension : Compatible
    }



{- FONTS -}


{-| -}
type alias ImportType compatible =
    { compatible | value : String, import_ : Compatible }


type alias FontFace compatible =
    { compatible | value : String, fontFace : Compatible }


{-| A font family
-}
type alias FontFamily compatible =
    { compatible | value : String, fontFamily : Compatible }


{-| A font size
-}
type alias FontSize compatible =
    { compatible | value : String, fontSize : Compatible }


{-| -}
type alias FontStyle compatible =
    { compatible | value : String, fontStyle : Compatible }


{-| -}
type alias FontStyleOrFeatureTagValue compatible =
    { compatible | value : String, fontStyle : Compatible, featureTagValue : Compatible }


{-| -}
type alias FontWeight compatible =
    { compatible | value : String, fontWeight : Compatible }


{-| -}
type alias FontVariant compatible =
    { compatible | value : String, fontVariant : Compatible }


{-| -}
type alias FontVariantLigatures compatible =
    { compatible
        | value : String
        , fontVariant : Compatible
        , fontVariantLigatures : Compatible
    }


{-| -}
type alias FontVariantCaps compatible =
    { compatible
        | value : String
        , fontVariant : Compatible
        , fontVariantCaps : Compatible
    }


{-| -}
type alias FontVariantNumeric compatible =
    { compatible
        | value : String
        , fontVariant : Compatible
        , fontVariantNumeric : Compatible
    }


{-| -}
type alias LineHeight compatible =
    { compatible | value : String, lineHeight : Compatible }


{-| <https://developer.mozilla.org/en-US/docs/Web/CSS/visibility#Values>
-}
type alias Visibility compatible =
    { compatible | value : String, visibility : Compatible }


{-| <https://developer.mozilla.org/en-US/docs/Web/CSS/text-decoration-line#Values>
-}
type alias TextDecorationLine compatible =
    { compatible | value : String, textDecorationLine : Compatible }


{-| <https://developer.mozilla.org/en-US/docs/Web/CSS/box-sizing#Values>
-}
type alias BoxSizing compatible =
    { compatible | value : String, boxSizing : Compatible }


{-| <https://developer.mozilla.org/en-US/docs/Web/CSS/overflow#Values>
-}
type alias Overflow compatible =
    { compatible | value : String, overflow : Compatible }


{-| <https://developer.mozilla.org/en-US/docs/Web/CSS/overflow-wrap#Values>
-}
type alias Wrap compatible =
    { compatible | value : String, overflowWrap : Compatible }


{-| <https://developer.mozilla.org/en/docs/Web/CSS/resize#Values>
-}
type alias Resize compatible =
    { compatible | value : String, resize : Compatible }


{-| <https://developer.mozilla.org/en-US/docs/Web/CSS/flex#Values>
-}
type alias LengthOrNumberOrAutoOrNoneOrContent compatible =
    { compatible | value : String, lengthOrNumberOrAutoOrNoneOrContent : Compatible }


{-| <https://developer.mozilla.org/en-US/docs/Web/CSS/z-index>
-}
type alias IntOrAuto compatible =
    { compatible | value : String, intOrAuto : Compatible }


{-| <https://developer.mozilla.org/en-US/docs/Web/CSS/flex-basis#Values>
-}
type alias FlexBasis compatible =
    { compatible | value : String, flexBasis : Compatible }


{-| <https://developer.mozilla.org/en-US/docs/Web/CSS/flex-wrap#Values>
-}
type alias FlexWrap compatible =
    { compatible | value : String, flexWrap : Compatible }


{-| <https://developer.mozilla.org/en-US/docs/Web/CSS/flex-direction#Values>
-}
type alias FlexDirection compatible =
    { compatible | value : String, flexDirection : Compatible }


{-| -}
type alias FlexDirectionOrWrap compatible =
    { compatible | value : String, flexDirectionOrWrap : Compatible }


{-| <https://developer.mozilla.org/en-US/docs/Web/CSS/align-items#Values>
-}
type alias AlignItems a b =
    Length a b -> Style


{-| <https://developer.mozilla.org/en-US/docs/Web/CSS/align-self#Values>
-}
type alias AlignSelf a b =
    Length a b -> Style


{-| <https://developer.mozilla.org/en-US/docs/Web/CSS/justify-content#Values>
-}
type alias JustifyContent a b =
    Length a b -> Style


{-| <https://developer.mozilla.org/en-US/docs/Web/CSS/display#Values>
-}
type alias Display compatible =
    { compatible | value : String, display : Compatible }


{-| <https://developer.mozilla.org/en-US/docs/Web/CSS/pointer-events#Values>
-}
type alias PointerEvents compatible =
    { compatible | value : String, pointerEvents : Compatible }


{-| <https://developer.mozilla.org/en-US/docs/Web/CSS/list-style-type#Values>
-}
type alias ListStyleType compatible =
    { compatible | value : String, listStyleType : Compatible }


{-| <https://developer.mozilla.org/en-US/docs/Web/CSS/list-style-position#Values>
-}
type alias ListStylePosition compatible =
    { compatible | value : String, listStylePosition : Compatible }


{-| <https://developer.mozilla.org/en-US/docs/Web/CSS/list-style#Values>
-}
type alias ListStyle compatible =
    { compatible | value : String, listStyleTypeOrPositionOrImage : Compatible }


{-| <https://developer.mozilla.org/en-US/docs/Web/CSS/white-space#Values>
-}
type alias WhiteSpace compatible =
    { compatible | value : String, whiteSpace : Compatible }


{-| <https://developer.mozilla.org/en-US/docs/Web/CSS/color#Values>
-}
type alias ColorValue compatible =
    Css.Internal.ColorValue compatible


colorValueForOverloadedProperty : ColorValue NonMixable
colorValueForOverloadedProperty =
    transparent


{-| -}
type alias Color =
    ColorValue { red : Int, green : Int, blue : Int, alpha : Float }


{-| <https://developer.mozilla.org/en-US/docs/Web/CSS/background-repeat#repeat-style>
-}
type alias BackgroundRepeat compatible =
    { compatible | value : String, backgroundRepeat : Compatible, backgroundRepeatShorthand : Compatible }


{-| -}
type alias BackgroundRepeatShorthand compatible =
    { compatible | value : String, backgroundRepeatShorthand : Compatible }


{-| <https://developer.mozilla.org/en-US/docs/Web/CSS/background-attachment>
-}
type alias BackgroundAttachment compatible =
    { compatible | value : String, backgroundAttachment : Compatible }


{-| <https://developer.mozilla.org/en-US/docs/Web/CSS/background-position>
-}
type alias BackgroundPosition compatible =
    { compatible | value : String, backgroundPosition : Compatible }


{-| Because `color` is both a common propertie and common value
in CSS (e.g. `color: red` with and `background-blend-mode: color`),
we implement it as a property (for the `color: red` case) and allow it to
be used as a value as well. When being used as a value, we call it, expect
that it will return the desired String as its key, and use that as our value.
(See `getOverloadedProperty`. Note that `VerticalAlign`.)
-}
type alias BackgroundBlendMode a =
    ColorValue a -> Style


{-| <https://developer.mozilla.org/en-US/docs/Web/CSS/background-clip>
-}
type alias BackgroundClip compatible =
    { compatible | value : String, backgroundClip : Compatible }


{-| <https://developer.mozilla.org/en-US/docs/Web/CSS/background-origin>
-}
type alias BackgroundOrigin compatible =
    BackgroundClip compatible


{-| <https://developer.mozilla.org/en-US/docs/Web/CSS/background-image>
-}
type alias BackgroundImage compatible =
    { compatible | value : String, backgroundImage : Compatible }


{-| <https://developer.mozilla.org/en-US/docs/Web/CSS/background-size>
-}
type alias LengthOrAutoOrCoverOrContain compatible =
    Css.Internal.LengthOrAutoOrCoverOrContain compatible


{-| <https://developer.mozilla.org/en-US/docs/Web/CSS/length>
-}
type alias Length compatible units =
    Css.Internal.Length compatible units


{-| <https://developer.mozilla.org/en/docs/Web/CSS/calc>
-}
type alias Calc compatible =
    { compatible
        | value : String
        , calc : Compatible
    }


{-| <https://css-tricks.com/a-complete-guide-to-calc-in-css/>
-}
type alias CalculatedLength =
    { value : String
    , length : Compatible
    , lengthOrAuto : Compatible
    , lengthOrNumber : Compatible
    , lengthOrNone : Compatible
    , lengthOrMinMaxDimension : Compatible
    , lengthOrNoneOrMinMaxDimension : Compatible
    , lineHeight : Compatible
    , textIndent : Compatible
    , flexBasis : Compatible
    , lengthOrNumberOrAutoOrNoneOrContent : Compatible
    , fontSize : Compatible
    , lengthOrAutoOrCoverOrContain : Compatible
    , calc : Compatible
    }


{-| -}
type CalcExpression
    = Addition
    | Subtraction


calcExpressionToString : CalcExpression -> String
calcExpressionToString expression =
    case expression of
        Addition ->
            "+"

        Subtraction ->
            "-"


{-| <https://developer.mozilla.org/en-US/docs/Web/CSS/touch-action?v=control#Values>
-}
type alias TouchAction compatible =
    { compatible | value : String, touchAction : Compatible }


{-| <https://developer.mozilla.org/en-US/docs/Web/CSS/table-layout?v=control#Values>
-}
type alias TableLayout compatible =
    { compatible | value : String, tableLayout : Compatible }


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


{-| The `overlay` [`blend-mode`](https://developer.mozilla.org/en-US/docs/Web/CSS/blend-mode#overlay).
-}
overlay : BackgroundBlendMode compatible
overlay =
    prop1 "overlay"


{-| The `darken` [`blend-mode`](https://developer.mozilla.org/en-US/docs/Web/CSS/blend-mode#darken).
-}
darken : BackgroundBlendMode compatible
darken =
    prop1 "darken"


{-| The `lighten` [`blend-mode`](https://developer.mozilla.org/en-US/docs/Web/CSS/blend-mode#lighten).
-}
lighten : BackgroundBlendMode compatible
lighten =
    prop1 "lighten"


{-| The `color-dodge` [`blend-mode`](https://developer.mozilla.org/en-US/docs/Web/CSS/blend-mode#color-dodge).
-}
colorDodge : BackgroundBlendMode compatible
colorDodge =
    prop1 "color-dodge"


{-| The `color-burn` [`blend-mode`](https://developer.mozilla.org/en-US/docs/Web/CSS/blend-mode#color-burn).
-}
colorBurn : BackgroundBlendMode compatible
colorBurn =
    prop1 "color-burn"


{-| The `hard-light` [`blend-mode`](https://developer.mozilla.org/en-US/docs/Web/CSS/blend-mode#hard-light).
-}
hardLight : BackgroundBlendMode compatible
hardLight =
    prop1 "hard-light"


{-| The `soft-light` [`blend-mode`](https://developer.mozilla.org/en-US/docs/Web/CSS/blend-mode#soft-light).
-}
softLight : BackgroundBlendMode compatible
softLight =
    prop1 "soft-light"


{-| The `difference` [`blend-mode`](https://developer.mozilla.org/en-US/docs/Web/CSS/blend-mode#difference).
-}
difference : BackgroundBlendMode compatible
difference =
    prop1 "difference"


{-| The `exclusion` [`blend-mode`](https://developer.mozilla.org/en-US/docs/Web/CSS/blend-mode#exclusion).
-}
exclusion : BackgroundBlendMode compatible
exclusion =
    prop1 "exclusion"


{-| The `hue` [`blend-mode`](https://developer.mozilla.org/en-US/docs/Web/CSS/blend-mode#hue).
-}
hue : BackgroundBlendMode compatible
hue =
    prop1 "hue"


{-| The `saturation` [`blend-mode`](https://developer.mozilla.org/en-US/docs/Web/CSS/blend-mode#saturation).
-}
saturation : BackgroundBlendMode compatible
saturation =
    prop1 "saturation"


{-| The `luminosity` [`blend-mode`](https://developer.mozilla.org/en-US/docs/Web/CSS/blend-mode#luminosity).
-}
luminosity : BackgroundBlendMode compatible
luminosity =
    prop1 "luminosity"


{-| The `padding-box` [`background-clip`](https://developer.mozilla.org/en-US/docs/Web/CSS/background-clip) value.
-}
paddingBox : BackgroundClip {}
paddingBox =
    { value = "padding-box"
    , backgroundClip = Compatible
    }


{-| The `url` [`background-image`](https://developer.mozilla.org/en-US/docs/Web/CSS/background-image) value.
-}
url : String -> BackgroundImage {}
url urlValue =
    { value = "url(" ++ urlValue ++ ")"
    , backgroundImage = Compatible
    }


{-| The `cover` [`background-size`](https://developer.mozilla.org/en-US/docs/Web/CSS/background-size) value.
-}
cover :
    { value : String
    , lengthOrAutoOrCoverOrContain : Compatible
    }
cover =
    { value = "cover"
    , lengthOrAutoOrCoverOrContain = Compatible
    }


{-| The `contain` [`background-size`](https://developer.mozilla.org/en-US/docs/Web/CSS/background-size) value.
-}
contain :
    { value : String
    , lengthOrAutoOrCoverOrContain : Compatible
    }
contain =
    { value = "contain"
    , lengthOrAutoOrCoverOrContain = Compatible
    }


{-| `hidden` [`overflow`](https://developer.mozilla.org/en-US/docs/Web/CSS/overflow#Values) value.

This can also represent a `hidden` [border style](https://developer.mozilla.org/en-US/docs/Web/CSS/border-style#Values),
as well as a `hidden` [`visibility`](https://developer.mozilla.org/en-US/docs/Web/CSS/visibility#Values).

-}
hidden : Overflow (BorderStyle (Visibility {}))
hidden =
    { value = "hidden"
    , overflow = Compatible
    , borderStyle = Compatible
    , visibility = Compatible
    }


{-| -}
type alias BasicProperty =
    { value : String
    , all : Compatible
    , alignItems : Compatible
    , borderStyle : Compatible
    , boxSizing : Compatible
    , color : Compatible
    , cursor : Compatible
    , display : Compatible
    , flexBasis : Compatible
    , flexWrap : Compatible
    , flexDirection : Compatible
    , flexDirectionOrWrap : Compatible
    , justifyContent : Compatible
    , none : Compatible
    , number : Compatible
    , keyframes : Compatible
    , outline : Compatible
    , overflow : Compatible
    , pointerEvents : Compatible
    , visibility : Compatible
    , textDecorationLine : Compatible
    , textRendering : Compatible
    , textIndent : Compatible
    , textDecorationStyle : Compatible
    , textTransform : Compatible
    , length : Compatible
    , lengthOrAuto : Compatible
    , lengthOrNone : Compatible
    , lengthOrNumber : Compatible
    , lengthOrMinMaxDimension : Compatible
    , lengthOrNoneOrMinMaxDimension : Compatible
    , lengthOrNumberOrAutoOrNoneOrContent : Compatible
    , lineHeight : Compatible
    , listStyleType : Compatible
    , listStylePosition : Compatible
    , listStyleTypeOrPositionOrImage : Compatible
    , fontFamily : Compatible
    , fontSize : Compatible
    , fontStyle : Compatible
    , fontWeight : Compatible
    , fontVariant : Compatible
    , units : IncompatibleUnits
    , numericValue : Float
    , unitLabel : String
    , backgroundRepeat : Compatible
    , backgroundRepeatShorthand : Compatible
    , backgroundAttachment : Compatible
    , backgroundBlendMode : Compatible
    , backgroundOrigin : Compatible
    , backgroundImage : Compatible
    , lengthOrAutoOrCoverOrContain : Compatible
    , intOrAuto : Compatible
    , touchAction : Compatible
    , whiteSpace : Compatible
    , tableLayout : Compatible
    }


{-| The [`unset`](https://developer.mozilla.org/en-US/docs/Web/CSS/unset) value.
Any CSS property can be set to this value.
-}
unset : BasicProperty
unset =
    { initial | value = "unset" }


{-| The [`inherit`](https://developer.mozilla.org/en-US/docs/Web/CSS/inherit) value.
Any CSS property can be set to this value.
-}
inherit : BasicProperty
inherit =
    { initial | value = "inherit" }


{-| The [`initial`](https://developer.mozilla.org/en-US/docs/Web/CSS/initial) value.
Any CSS property can be set to this value.
-}
initial : BasicProperty
initial =
    { value = "initial"
    , overflow = Compatible
    , visibility = Compatible
    , none = Compatible
    , number = Compatible
    , textDecorationLine = Compatible
    , textRendering = Compatible
    , textIndent = Compatible
    , textDecorationStyle = Compatible
    , textTransform = Compatible
    , borderStyle = Compatible
    , boxSizing = Compatible
    , color = Compatible
    , cursor = Compatible
    , display = Compatible
    , keyframes = Compatible
    , all = Compatible
    , alignItems = Compatible
    , justifyContent = Compatible
    , length = Compatible
    , lengthOrAuto = Compatible
    , lengthOrNone = Compatible
    , lengthOrNumber = Compatible
    , lengthOrMinMaxDimension = Compatible
    , lengthOrNoneOrMinMaxDimension = Compatible
    , listStyleType = Compatible
    , lineHeight = Compatible
    , listStylePosition = Compatible
    , listStyleTypeOrPositionOrImage = Compatible
    , flexBasis = Compatible
    , flexWrap = Compatible
    , flexDirection = Compatible
    , flexDirectionOrWrap = Compatible
    , lengthOrNumberOrAutoOrNoneOrContent = Compatible
    , fontFamily = Compatible
    , fontSize = Compatible
    , fontStyle = Compatible
    , fontWeight = Compatible
    , fontVariant = Compatible
    , outline = Compatible
    , pointerEvents = Compatible
    , units = Css.Internal.IncompatibleUnits
    , numericValue = 0
    , unitLabel = ""
    , backgroundRepeat = Compatible
    , backgroundRepeatShorthand = Compatible
    , backgroundAttachment = Compatible
    , backgroundBlendMode = Compatible
    , backgroundOrigin = Compatible
    , backgroundImage = Compatible
    , lengthOrAutoOrCoverOrContain = Compatible
    , intOrAuto = Compatible
    , touchAction = Compatible
    , whiteSpace = Compatible
    , tableLayout = Compatible
    }


{-| [RGB color value](https://developer.mozilla.org/en-US/docs/Web/CSS/color_value#rgb)
in functional notation.
-}
rgb : Int -> Int -> Int -> Color
rgb r g b =
    { value = cssFunction "rgb" (List.map String.fromInt [ r, g, b ])
    , color = Compatible
    , red = r
    , green = g
    , blue = b
    , alpha = 1
    }


{-| [RGBA color value](https://developer.mozilla.org/en-US/docs/Web/CSS/color_value#rgba).
-}
rgba : Int -> Int -> Int -> Float -> Color
rgba r g b alpha =
    { value = cssFunction "rgba" (List.map String.fromInt [ r, g, b ] ++ [ String.fromFloat alpha ])
    , color = Compatible
    , red = r
    , green = g
    , blue = b
    , alpha = alpha
    }


{-| [HSL color value](https://developer.mozilla.org/en-US/docs/Web/CSS/color_value#hsl)
`s` and `l` values are expressed as a number between 0 and 1 and are converted
to the appropriate percentage at compile-time
-}
hsl : Float -> Float -> Float -> Color
hsl hueVal saturationVal lightnessVal =
    let
        valuesList =
            [ String.fromFloat hueVal
            , numericalPercentageToString saturationVal
            , numericalPercentageToString lightnessVal
            ]

        value =
            cssFunction "hsl" valuesList
    in
    hslaToRgba value hueVal saturationVal lightnessVal 1


{-| [HSLA color value](https://developer.mozilla.org/en-US/docs/Web/CSS/color_value#hsla)
`s` and `l` values are expressed as a number between 0 and 1 and are converted
to the appropriate percentage at compile-time
-}
hsla : Float -> Float -> Float -> Float -> Color
hsla hueVal saturationVal lightnessVal alpha =
    let
        valuesList =
            [ String.fromFloat hueVal
            , numericalPercentageToString saturationVal
            , numericalPercentageToString lightnessVal
            , String.fromFloat alpha
            ]

        value =
            cssFunction "hsla" valuesList
    in
    hslaToRgba value hueVal saturationVal lightnessVal alpha


{-| [RGB color value](https://developer.mozilla.org/en-US/docs/Web/CSS/color_value#rgb)
in hexadecimal notation. You can optionally include `#` as the first character,
for benefits like syntax highlighting in editors, ease of copy/pasting from
tools which express these as e.g. `#abcdef0`, etc.
-}
hex : String -> Color
hex str =
    let
        withoutHash =
            if String.startsWith "#" str then
                String.dropLeft 1 str

            else
                str
    in
    case String.toList withoutHash of
        [ r, g, b ] ->
            validHex str ( r, r ) ( g, g ) ( b, b ) ( 'f', 'f' )

        [ r, g, b, a ] ->
            validHex str ( r, r ) ( g, g ) ( b, b ) ( a, a )

        [ r1, r2, g1, g2, b1, b2 ] ->
            validHex str ( r1, r2 ) ( g1, g2 ) ( b1, b2 ) ( 'f', 'f' )

        [ r1, r2, g1, g2, b1, b2, a1, a2 ] ->
            validHex str ( r1, r2 ) ( g1, g2 ) ( b1, b2 ) ( a1, a2 )

        _ ->
            erroneousHex str


validHex : String -> ( Char, Char ) -> ( Char, Char ) -> ( Char, Char ) -> ( Char, Char ) -> Color
validHex str ( r1, r2 ) ( g1, g2 ) ( b1, b2 ) ( a1, a2 ) =
    let
        toResult =
            String.fromList >> String.toLower >> Hex.fromString

        results =
            ( ( toResult [ r1, r2 ]
              , toResult [ g1, g2 ]
              )
            , ( toResult [ b1, b2 ]
              , toResult [ a1, a2 ]
              )
            )
    in
    case results of
        ( ( Ok red, Ok green ), ( Ok blue, Ok alpha ) ) ->
            { value = withPrecedingHash str
            , color = Compatible
            , red = red
            , green = green
            , blue = blue
            , alpha = toFloat alpha / 255
            }

        _ ->
            erroneousHex str


withPrecedingHash : String -> String
withPrecedingHash str =
    if String.startsWith "#" str then
        str

    else
        String.cons '#' str


{-| Not to be confused with Thelonious Monk or Hieronymus Bosch.
-}
erroneousHex : String -> Color
erroneousHex str =
    { value = withPrecedingHash str
    , color = Compatible
    , red = 0
    , green = 0
    , blue = 0
    , alpha = 1
    }


hslaToRgba : String -> Float -> Float -> Float -> Float -> Color
hslaToRgba value hueVal saturationVal lightness hslAlpha =
    let
        ( red, green, blue ) =
            hslToRgb (degreesToRadians hueVal) saturationVal lightness
    in
    { value = value
    , color = Compatible
    , red = floor red
    , green = floor green
    , blue = floor blue
    , alpha = hslAlpha
    }


{-| Backported from <https://github.com/elm-lang/core/blob/5.1.1/src/Color.elm>
-}
hslToRgb : Float -> Float -> Float -> ( Float, Float, Float )
hslToRgb hueVal saturationVal lightness =
    let
        chroma =
            (1 - abs (2 * lightness - 1)) * saturationVal

        normHue =
            hueVal / degrees 60

        x =
            chroma * (1 - abs (fmod normHue 2 - 1))

        ( r, g, b ) =
            if normHue < 0 then
                ( 0, 0, 0 )

            else if normHue < 1 then
                ( chroma, x, 0 )

            else if normHue < 2 then
                ( x, chroma, 0 )

            else if normHue < 3 then
                ( 0, chroma, x )

            else if normHue < 4 then
                ( 0, x, chroma )

            else if normHue < 5 then
                ( x, 0, chroma )

            else if normHue < 6 then
                ( chroma, 0, x )

            else
                ( 0, 0, 0 )

        m =
            lightness - chroma / 2
    in
    ( r + m, g + m, b + m )


{-| Backported from <https://github.com/elm-lang/core/blob/5.1.1/src/Color.elm>
-}
fmod : Float -> Int -> Float
fmod f n =
    let
        integer =
            floor f
    in
    toFloat (modBy n integer) + f - toFloat integer


degreesToRadians : Float -> Float
degreesToRadians degrees =
    degrees * 180 / pi



{- TEXT-RENDERING -}


{-| `optimizeSpeed` [`text-rendering`](https://developer.mozilla.org/en-US/docs/Web/CSS/text-rendering#Values) value
-}
optimizeSpeed : TextRendering {}
optimizeSpeed =
    { value = "optimizeSpeed"
    , textRendering = Compatible
    }


{-| `optimizeLegibility` [`text-rendering`](https://developer.mozilla.org/en-US/docs/Web/CSS/text-rendering#Values) value
-}
optimizeLegibility : TextRendering {}
optimizeLegibility =
    { value = "optimizeLegibility"
    , textRendering = Compatible
    }


{-| `geometricPrecision` [`text-rendering`](https://developer.mozilla.org/en-US/docs/Web/CSS/text-rendering#Values) value
-}
geometricPrecision : TextRendering {}
geometricPrecision =
    { value = "geometricPrecision"
    , textRendering = Compatible
    }



{- TEXT-INDENT -}


{-| `hanging` [`hanging`](https://developer.mozilla.org/en-US/docs/Web/CSS/hanging#Values) value
-}
hanging : TextIndent {}
hanging =
    { value = "hanging"
    , textIndent = Compatible
    }


{-| `each-line` [`text-indent`](https://developer.mozilla.org/en-US/docs/Web/CSS/text-indent#Values) value
-}
eachLine : TextIndent {}
eachLine =
    { value = "each-line"
    , textIndent = Compatible
    }



{- TEXT-ORIENTATION -}


{-| `mixed` [`text-orientation`](https://developer.mozilla.org/en-US/docs/Web/CSS/text-orientation#Values) value
-}
mixed : TextOrientation {}
mixed =
    { value = "mixed"
    , textOrientation = Compatible
    }


{-| `upright` [`text-orientation`](https://developer.mozilla.org/en-US/docs/Web/CSS/text-orientation#Values) value
-}
upright : TextOrientation {}
upright =
    { value = "upright"
    , textOrientation = Compatible
    }


{-| `sideways` [`text-orientation`](https://developer.mozilla.org/en-US/docs/Web/CSS/text-orientationEValues) value
-}
sideways : TextOrientation {}
sideways =
    { value = "sideways"
    , textOrientation = Compatible
    }



{- TEXT-TRANSFORM -}


{-| `capitalize` [`text-transform`](https://developer.mozilla.org/en-US/docs/Web/CSS/text-transform#Values) value
-}
capitalize : TextTransform {}
capitalize =
    { value = "capitalize"
    , textTransform = Compatible
    }


{-| `uppercase` [`text-transform`](https://developer.mozilla.org/en-US/docs/Web/CSS/text-transform#Values) value
-}
uppercase : TextTransform {}
uppercase =
    { value = "uppercase"
    , textTransform = Compatible
    }


{-| `lowercase` [`text-transform`](https://developer.mozilla.org/en-US/docs/Web/CSS/text-transform#Values) value
-}
lowercase : TextTransform {}
lowercase =
    { value = "lowercase"
    , textTransform = Compatible
    }


{-| `full-width` [`text-transform`](https://developer.mozilla.org/en-US/docs/Web/CSS/text-transform#Values) value
-}
fullWidth : TextTransform {}
fullWidth =
    { value = "full-width"
    , textTransform = Compatible
    }


{-| `ellipsis` [`text-overflow`](https://developer.mozilla.org/en-US/docs/Web/CSS/text-overflow#Values) value
-}
ellipsis : TextOverflow {}
ellipsis =
    { value = "ellipsis"
    , textOverflow = Compatible
    }


{-| `clip` [`text-overflow`](https://developer.mozilla.org/en-US/docs/Web/CSS/text-overflow#Values) value
-}
clip : TextOverflow {}
clip =
    { value = "clip"
    , textOverflow = Compatible
    }



{- BORDER STYLE -}


{-| A `wavy` [text decoration style](https://developer.mozilla.org/en-US/docs/Web/CSS/text-decoration-style#Values).
-}
wavy : TextDecorationStyle {}
wavy =
    { value = "wavy"
    , textDecorationStyle = Compatible
    }


{-| A `dotted` [border style](https://developer.mozilla.org/en-US/docs/Web/CSS/border-style#Values).
-}
dotted : BorderStyle (TextDecorationStyle {})
dotted =
    { value = "dotted"
    , borderStyle = Compatible
    , textDecorationStyle = Compatible
    }


{-| A `dashed` [border style](https://developer.mozilla.org/en-US/docs/Web/CSS/border-style#Values).
-}
dashed : BorderStyle (TextDecorationStyle {})
dashed =
    { value = "dashed"
    , borderStyle = Compatible
    , textDecorationStyle = Compatible
    }


{-| A `solid` [border style](https://developer.mozilla.org/en-US/docs/Web/CSS/border-style#Values).
-}
solid : BorderStyle (TextDecorationStyle {})
solid =
    { value = "solid"
    , borderStyle = Compatible
    , textDecorationStyle = Compatible
    }


{-| A `double` [border style](https://developer.mozilla.org/en-US/docs/Web/CSS/border-style#Values).
-}
double : BorderStyle (TextDecorationStyle {})
double =
    { value = "double"
    , borderStyle = Compatible
    , textDecorationStyle = Compatible
    }


{-| A `groove` [border style](https://developer.mozilla.org/en-US/docs/Web/CSS/border-style#Values).
-}
groove : BorderStyle {}
groove =
    { value = "groove"
    , borderStyle = Compatible
    }


{-| A `ridge` [border style](https://developer.mozilla.org/en-US/docs/Web/CSS/border-style#Values).
-}
ridge : BorderStyle {}
ridge =
    { value = "ridge"
    , borderStyle = Compatible
    }


{-| An `inset` [border style](https://developer.mozilla.org/en-US/docs/Web/CSS/border-style#Values).
-}
inset : BorderStyle {}
inset =
    { value = "inset"
    , borderStyle = Compatible
    }


{-| An `outset` [border style](https://developer.mozilla.org/en-US/docs/Web/CSS/border-style#Values).
-}
outset : BorderStyle {}
outset =
    { value = "outset"
    , borderStyle = Compatible
    }



{- BORDER COLLAPSE -}


{-| A `separate` [border-collapse](https://developer.mozilla.org/en-US/docs/Web/CSS/border-collapse#Values).
-}
separate : BorderCollapse {}
separate =
    { value = "separate"
    , borderCollapse = Compatible
    }


{-| A `collapse` [border-collapse](https://developer.mozilla.org/en-US/docs/Web/CSS/border-collapse#Values).
This can also represent a `collapse` [`visibility`](https://developer.mozilla.org/en-US/docs/Web/CSS/visibility#Values).
-}
collapse : BorderCollapse (Visibility {})
collapse =
    { value = "collapse"
    , borderCollapse = Compatible
    , visibility = Compatible
    }



{- ALIGNMENTS -}


{-| `center` [alignment](https://developer.mozilla.org/en-US/docs/Web/CSS/text-align).
Can also be used with flex-box's align-items and justify-content properties to apply the value of center
-}
center : TextAlign a b
center =
    prop1 "center"


{-| `justify` [alignment](https://developer.mozilla.org/en-US/docs/Web/CSS/text-align).
-}
justify : TextAlign a b
justify =
    prop1 "justify"


{-| `justify-all` [alignment](https://developer.mozilla.org/en-US/docs/Web/CSS/text-align).
-}
justifyAll : TextAlign a b
justifyAll =
    prop1 "justify-all"


{-| `start` [alignment](https://developer.mozilla.org/en-US/docs/Web/CSS/text-align).
-}
start : TextAlign a b
start =
    prop1 "start"


{-| `end` [alignment](https://developer.mozilla.org/en-US/docs/Web/CSS/text-align).
-}
end : TextAlign a b
end =
    prop1 "end"


{-| `match-parent` [alignment](https://developer.mozilla.org/en-US/docs/Web/CSS/text-align).
-}
matchParent : TextAlign a b
matchParent =
    prop1 "match-parent"


{-| `true` [alignment](https://developer.mozilla.org/en-US/docs/Web/CSS/text-align).
-}
true : TextAlign a b
true =
    prop1 "true"



{- LENGTHS -}


{-| Convenience length value that compiles to 0 with no units.

    css [ padding zero ]

...compiles to:

    padding: 0;

-}
zero :
    { value : String
    , length : Compatible
    , lengthOrNumber : Compatible
    , lengthOrNone : Compatible
    , lengthOrAuto : Compatible
    , lengthOrMinMaxDimension : Compatible
    , lengthOrNoneOrMinMaxDimension : Compatible
    , number : Compatible
    , outline : Compatible
    , units : UnitlessInteger
    , unitLabel : String
    , numericValue : Float
    , lengthOrAutoOrCoverOrContain : Compatible
    }
zero =
    { value = "0"
    , length = Compatible
    , lengthOrNumber = Compatible
    , lengthOrNone = Compatible
    , lengthOrAuto = Compatible
    , lengthOrMinMaxDimension = Compatible
    , lengthOrNoneOrMinMaxDimension = Compatible
    , number = Compatible
    , outline = Compatible
    , units = UnitlessInteger
    , unitLabel = ""
    , numericValue = 0
    , lengthOrAutoOrCoverOrContain = Compatible
    }


{-| A CSS [time](https://developer.mozilla.org/en-US/docs/Web/CSS/time) duration.

The spec says that a unitless [zero](#zero) is not allowed for these. You must
specify either [`ms`](#ms) or [`sec`](#sec)!

-}
type alias Duration compatible =
    { compatible
        | value : String
        , duration : Compatible
    }


{-| A [duration](#Duration) in seconds.
-}
sec : Float -> Duration {}
sec amount =
    { value = String.fromFloat amount ++ "s"
    , duration = Compatible
    }


{-| A [duration](#Duration) in milliseconds.
-}
ms : Float -> Duration {}
ms amount =
    { value = String.fromFloat amount ++ "ms"
    , duration = Compatible
    }


{-| [`pct`](https://developer.mozilla.org/en-US/docs/Web/CSS/length#pct) units.
-}
type alias Pct =
    ExplicitLength PercentageUnits


{-| [`pct`](https://developer.mozilla.org/en-US/docs/Web/CSS/length#pct) units.
-}
pct : Float -> Pct
pct =
    lengthConverter PercentageUnits "%"
