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


type PercentageUnits
    = PercentageUnits


{-| [`em`](https://developer.mozilla.org/en-US/docs/Web/CSS/length#em) units.
-}
type alias Em =
    ExplicitLength EmUnits


{-| [`em`](https://developer.mozilla.org/en-US/docs/Web/CSS/length#em) units.
-}
em : Float -> Em
em =
    lengthConverter EmUnits "em"


type EmUnits
    = EmUnits


{-| [`ex`](https://developer.mozilla.org/en-US/docs/Web/CSS/length#ex) units.
-}
type alias Ex =
    ExplicitLength ExUnits


{-| [`ex`](https://developer.mozilla.org/en-US/docs/Web/CSS/length#ex) units.
-}
ex : Float -> Ex
ex =
    lengthConverter ExUnits "ex"


type ExUnits
    = ExUnits


{-| [`ch`](https://developer.mozilla.org/en-US/docs/Web/CSS/length#ch) units.
-}
type alias Ch =
    ExplicitLength ChUnits


{-| [`ch`](https://developer.mozilla.org/en-US/docs/Web/CSS/length#ch) units.
-}
ch : Float -> Ch
ch =
    lengthConverter ChUnits "ch"


type ChUnits
    = ChUnits


{-| [`rem`](https://developer.mozilla.org/en-US/docs/Web/CSS/length#rem) units.
-}
type alias Rem =
    ExplicitLength RemUnits


{-| [`rem`](https://developer.mozilla.org/en-US/docs/Web/CSS/length#rem) units.
-}
rem : Float -> Rem
rem =
    lengthConverter RemUnits "rem"


type RemUnits
    = RemUnits


{-| [`vh`](https://developer.mozilla.org/en-US/docs/Web/CSS/length#vh) units.
-}
type alias Vh =
    ExplicitLength VhUnits


{-| [`vh`](https://developer.mozilla.org/en-US/docs/Web/CSS/length#vh) units.
-}
vh : Float -> Vh
vh =
    lengthConverter VhUnits "vh"


type VhUnits
    = VhUnits


{-| [`vw`](https://developer.mozilla.org/en-US/docs/Web/CSS/length#vw) units.
-}
type alias Vw =
    ExplicitLength VwUnits


{-| [`vw`](https://developer.mozilla.org/en-US/docs/Web/CSS/length#vw) units.
-}
vw : Float -> Vw
vw =
    lengthConverter VwUnits "vw"


type VwUnits
    = VwUnits


{-| [`vmin`](https://developer.mozilla.org/en-US/docs/Web/CSS/length#vmin) units.
-}
type alias Vmin =
    ExplicitLength VMinUnits


{-| [`vmin`](https://developer.mozilla.org/en-US/docs/Web/CSS/length#vmin) units.
-}
vmin : Float -> Vmin
vmin =
    lengthConverter VMinUnits "vmin"


type VMinUnits
    = VMinUnits


{-| [`vmax`](https://developer.mozilla.org/en-US/docs/Web/CSS/length#vmax) units.
-}
type alias Vmax =
    ExplicitLength VMaxUnits


{-| [`vmax`](https://developer.mozilla.org/en-US/docs/Web/CSS/length#vmax) units.
-}
vmax : Float -> Vmax
vmax =
    lengthConverter VMaxUnits "vmax"


type VMaxUnits
    = VMaxUnits


{-| [`px`](https://developer.mozilla.org/en-US/docs/Web/CSS/length#px) units.
-}
type alias Px =
    ExplicitLength PxUnits


{-| [`px`](https://developer.mozilla.org/en-US/docs/Web/CSS/length#px) units.
-}
px : Float -> Px
px =
    lengthConverter PxUnits "px"


type PxUnits
    = PxUnits


{-| [`mm`](https://developer.mozilla.org/en-US/docs/Web/CSS/length#mm) units.
-}
type alias Mm =
    ExplicitLength MMUnits


{-| [`mm`](https://developer.mozilla.org/en-US/docs/Web/CSS/length#mm) units.
-}
mm : Float -> Mm
mm =
    lengthConverter MMUnits "mm"


type MMUnits
    = MMUnits


{-| [`cm`](https://developer.mozilla.org/en-US/docs/Web/CSS/length#cm) units.
-}
type alias Cm =
    ExplicitLength CMUnits


{-| [`cm`](https://developer.mozilla.org/en-US/docs/Web/CSS/length#cm) units.
-}
cm : Float -> Cm
cm =
    lengthConverter CMUnits "cm"


type CMUnits
    = CMUnits


{-| [`in`](https://developer.mozilla.org/en-US/docs/Web/CSS/length#in) units.
-}
type alias In =
    ExplicitLength InchUnits


{-| [`in`](https://developer.mozilla.org/en-US/docs/Web/CSS/length#in) units.

(This is `inches` instead of `in` because `in` is a reserved keyword in Elm.)

-}
inches : Float -> In
inches =
    lengthConverter InchUnits "in"


type InchUnits
    = InchUnits


{-| [`pt`](https://developer.mozilla.org/en-US/docs/Web/CSS/length#pt) units.
-}
type alias Pt =
    ExplicitLength PtUnits


{-| [`pt`](https://developer.mozilla.org/en-US/docs/Web/CSS/length#pt) units.
-}
pt : Float -> Pt
pt =
    lengthConverter PtUnits "pt"


type PtUnits
    = PtUnits


{-| [`pc`](https://developer.mozilla.org/en-US/docs/Web/CSS/length#pc) units.
-}
type alias Pc =
    ExplicitLength PcUnits


{-| [`pc`](https://developer.mozilla.org/en-US/docs/Web/CSS/length#pc) units.
-}
pc : Float -> Pc
pc =
    lengthConverter PcUnits "pc"


type PcUnits
    = PcUnits


{-| A unitless integer. Useful with properties like [`borderImageOutset`](#borderImageOutset)
which accept either length units or unitless numbers for some properties.
-}
int : Int -> IntOrAuto (Number (LengthOrNumberOrAutoOrNoneOrContent (LengthOrNumber (FontWeight (NumberOrInfinite { numericValue : Float, unitLabel : String, units : UnitlessInteger })))))
int val =
    { value = String.fromInt val
    , lengthOrNumber = Compatible
    , number = Compatible
    , numberOrInfinite = Compatible
    , fontWeight = Compatible
    , lengthOrNumberOrAutoOrNoneOrContent = Compatible
    , intOrAuto = Compatible
    , numericValue = toFloat val
    , unitLabel = ""
    , units = UnitlessInteger
    }


type UnitlessInteger
    = UnitlessInteger


{-| The [`infinite`](https://developer.mozilla.org/en-US/docs/Web/CSS/animation-iteration-count#Values) value.
-}
infinite : Infinite
infinite =
    { value = "infinite"
    , numberOrInfinite = Compatible
    }


{-| A unitless number. Useful with properties like [`flexGrow`](#flexGrow)
which accept unitless numbers.
-}
num : Float -> LengthOrNumberOrAutoOrNoneOrContent (Number (LengthOrNumber (NumberOrInfinite (LineHeight { numericValue : Float, unitLabel : String, units : UnitlessFloat }))))
num val =
    { value = String.fromFloat val
    , lengthOrNumber = Compatible
    , number = Compatible
    , numberOrInfinite = Compatible
    , lengthOrNumberOrAutoOrNoneOrContent = Compatible
    , lineHeight = Compatible
    , numericValue = val
    , unitLabel = ""
    , units = UnitlessFloat
    }


type UnitlessFloat
    = UnitlessFloat


{-| -}
type alias IncompatibleUnits =
    Css.Internal.IncompatibleUnits



{- ANGLES -}


angleConverter : String -> Float -> AngleOrDirection (Angle {})
angleConverter suffix angleVal =
    { value = String.fromFloat angleVal ++ suffix
    , angle = Compatible
    , angleOrDirection = Compatible
    }


{-| [`deg`](https://developer.mozilla.org/en-US/docs/Web/CSS/angle#deg) units.
-}
deg : Float -> AngleOrDirection (Angle {})
deg =
    angleConverter "deg"


{-| [`grad`](https://developer.mozilla.org/en-US/docs/Web/CSS/angle#grad) units.
-}
grad : Float -> AngleOrDirection (Angle {})
grad =
    angleConverter "grad"


{-| [`rad`](https://developer.mozilla.org/en-US/docs/Web/CSS/angle#rad) units.
-}
rad : Float -> AngleOrDirection (Angle {})
rad =
    angleConverter "rad"


{-| [`turn`](https://developer.mozilla.org/en-US/docs/Web/CSS/angle#tr) units.
-}
turn : Float -> AngleOrDirection (Angle {})
turn =
    angleConverter "turn"



{- TRANSFORMS -}


{-| The [`matrix()`](https://developer.mozilla.org/en-US/docs/Web/CSS/transform-function#matrix()) transform-function.

    transform (matrix 0.5 1 1.5 2 2.5 3)

-}
matrix : Float -> Float -> Float -> Float -> Float -> Float -> Transform {}
matrix a b c d tx ty =
    { value = cssFunction "matrix" (List.map String.fromFloat [ a, b, c, d, tx, ty ])
    , transform = Compatible
    }


{-| The [`matrix3d()`](https://developer.mozilla.org/en-US/docs/Web/CSS/transform-function#matrix3d()) transform-function.

    transform (matrix3d 0.5 1 1.5 2 2.5 3 0.5 1 1.5 2 2.5 3 0.5 1 1.5 2 2.5 3 0.5 1 1.5 2 2.5 3)

-}
matrix3d : Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Transform {}
matrix3d a1 a2 a3 a4 b1 b2 b3 b4 c1 c2 c3 c4 d1 d2 d3 d4 =
    { value = cssFunction "matrix3d" (List.map String.fromFloat [ a1, a2, a3, a4, b1, b2, b3, b4, c1, c2, c3, c4, d1, d2, d3, d4 ])
    , transform = Compatible
    }


{-| The [`perspective()`](https://developer.mozilla.org/en-US/docs/Web/CSS/transform-function#perspective()) transform-function.

     transform (perspective 0.5)

-}
perspective : Float -> Transform {}
perspective l =
    { value = cssFunction "perspective" [ String.fromFloat l ]
    , transform = Compatible
    }


{-| The [`rotate`](https://developer.mozilla.org/en-US/docs/Web/CSS/transform-function#rotate()) transform-function.

     transform (rotate (deg 90))

-}
rotate : Angle compatible -> Transform {}
rotate { value } =
    { value = cssFunction "rotate" [ value ]
    , transform = Compatible
    }


{-| The [`rotateX`](https://developer.mozilla.org/en-US/docs/Web/CSS/transform-function#rotateX()) transform-function.

     transform (rotateX (deg 90))

-}
rotateX : Angle compatible -> Transform {}
rotateX { value } =
    { value = cssFunction "rotateX" [ value ]
    , transform = Compatible
    }


{-| The [`rotateY`](https://developer.mozilla.org/en-US/docs/Web/CSS/transform-function#rotateY()) transform-function.

     transform (rotateY (deg 90))

-}
rotateY : Angle compatible -> Transform {}
rotateY { value } =
    { value = cssFunction "rotateY" [ value ]
    , transform = Compatible
    }


{-| The [`rotateZ`](https://developer.mozilla.org/en-US/docs/Web/CSS/transform-function#rotateZ()) transform-function.

     transform (rotateZ (deg 90))

-}
rotateZ : Angle compatible -> Transform {}
rotateZ { value } =
    { value = cssFunction "rotateZ" [ value ]
    , transform = Compatible
    }


{-| The [`rotate3d`](https://developer.mozilla.org/en-US/docs/Web/CSS/transform-function#rotate3d()) transform-function.

     transform (rotate3d 1 1 1 (deg 90))

-}
rotate3d : Float -> Float -> Float -> Angle compatible -> Transform {}
rotate3d x y z { value } =
    let
        coordsAsStrings =
            List.map String.fromFloat [ x, y, z ]
    in
    { value = cssFunction "rotate3d" (coordsAsStrings ++ [ value ])
    , transform = Compatible
    }


{-| The [`scale`](https://developer.mozilla.org/en-US/docs/Web/CSS/transform-function#scale()) transform-function.

     transform (scale 0.5)
     transform (scale2 0.5 0.7)

-}
scale : Float -> Transform {}
scale x =
    { value = cssFunction "scale" [ String.fromFloat x ]
    , transform = Compatible
    }


{-| The [`scale`](https://developer.mozilla.org/en-US/docs/Web/CSS/transform-function#scale()) transform-function.

     transform (scale 0.5)
     transform (scale2 0.5 0.7)

-}
scale2 : Float -> Float -> Transform {}
scale2 x y =
    { value = cssFunction "scale" (List.map String.fromFloat [ x, y ])
    , transform = Compatible
    }


{-| The [`scaleX`](https://developer.mozilla.org/en-US/docs/Web/CSS/transform-function#scaleX()) transform-function.

     transform (scaleX 0.5)

-}
scaleX : Float -> Transform {}
scaleX x =
    { value = cssFunction "scaleX" [ String.fromFloat x ]
    , transform = Compatible
    }


{-| The [`scaleY`](https://developer.mozilla.org/en-US/docs/Web/CSS/transform-function#scaleY()) transform-function.

     transform (scaleY 0.5)

-}
scaleY : Float -> Transform {}
scaleY y =
    { value = cssFunction "scaleY" [ String.fromFloat y ]
    , transform = Compatible
    }


{-| The [`scale3d`](https://developer.mozilla.org/en-US/docs/Web/CSS/transform-function#scale3d()) transform-function.

     transform (scale3d 0.5 0.5 1)

-}
scale3d : Float -> Float -> Float -> Transform {}
scale3d x y z =
    { value = cssFunction "scale3d" (List.map String.fromFloat [ x, y, z ])
    , transform = Compatible
    }


{-| The [`skew`](https://developer.mozilla.org/en-US/docs/Web/CSS/transform-function#skew()) transform-function.

     transform (skew (deg 90))
     transform (skew2 (deg 90) (deg 45))

-}
skew : Angle compatible -> Transform {}
skew { value } =
    { value = cssFunction "skew" [ value ]
    , transform = Compatible
    }


{-| The [`skew`](https://developer.mozilla.org/en-US/docs/Web/CSS/transform-function#skew()) transform-function.

     transform (skew (deg 90))
     transform (skew2 (deg 90) (deg 45))

-}
skew2 : Angle compatibleA -> Angle compatibleB -> Transform {}
skew2 ax ay =
    { value = cssFunction "skew" [ ax.value, ay.value ]
    , transform = Compatible
    }


{-| The [`skewX`](https://developer.mozilla.org/en-US/docs/Web/CSS/transform-function#skewX()) transform-function.

     transform (skewX (deg 90))

-}
skewX : Angle compatible -> Transform {}
skewX { value } =
    { value = cssFunction "skewX" [ value ]
    , transform = Compatible
    }


{-| The [`skewY`](https://developer.mozilla.org/en-US/docs/Web/CSS/transform-function#skewY()) transform-function.

    transform (skewY (deg 90))

-}
skewY : Angle compatible -> Transform {}
skewY { value } =
    { value = cssFunction "skewY" [ value ]
    , transform = Compatible
    }


{-| The [`translate`](https://developer.mozilla.org/en-US/docs/Web/CSS/transform-function#translate()) transform-function.

    transform (translate (px 100))

    transform (translate2 (px 100) (pct -45))

-}
translate : Length compatible units -> Transform {}
translate { value } =
    { value = cssFunction "translate" [ value ]
    , transform = Compatible
    }


{-| The [`translate`](https://developer.mozilla.org/en-US/docs/Web/CSS/transform-function#translate()) transform-function.

    transform (translate (px 100))

    transform (translate2 (px 100) (pct -45))

-}
translate2 : Length compatibleA unitsA -> Length compatibleB unitsB -> Transform {}
translate2 tx ty =
    { value = cssFunction "translate" [ tx.value, ty.value ]
    , transform = Compatible
    }


{-| The [`translateX`](https://developer.mozilla.org/en-US/docs/Web/CSS/transform-function#translateX()) transform-function.

    transform (translateX (px 100))

-}
translateX : Length compatible units -> Transform {}
translateX { value } =
    { value = cssFunction "translateX" [ value ]
    , transform = Compatible
    }


{-| The [`translateY`](https://developer.mozilla.org/en-US/docs/Web/CSS/transform-function#translateY()) transform-function.

    transform (translateY (px 100))

-}
translateY : Length compatible units -> Transform {}
translateY { value } =
    { value = cssFunction "translateY" [ value ]
    , transform = Compatible
    }


{-| The [`translateZ`](https://developer.mozilla.org/en-US/docs/Web/CSS/transform-function#translateZ()) transform-function.

    transform (translateZ (px 100))

-}
translateZ : Length compatible units -> Transform {}
translateZ { value } =
    { value = cssFunction "translateZ" [ value ]
    , transform = Compatible
    }


{-| The [`translateX`](https://developer.mozilla.org/en-US/docs/Web/CSS/transform-function#translateX()) transform-function.

    transform (translate3d (px 100) (px 100) (px 100))

-}
translate3d : Length compatibleA unitsA -> Length compatibleB unitsB -> Length compatibleC unitsC -> Transform {}
translate3d tx ty tz =
    { value = cssFunction "translate3d" [ tx.value, ty.value, tz.value ]
    , transform = Compatible
    }


{-| See [`keyframes`](Css-Animations#keyframes) in the [`Css.Animations`](Css-Animations) module.
-}
animationName : Keyframes compatible -> Style
animationName arg =
    if arg.value == "none" || arg.value == "inherit" || arg.value == "unset" || arg.value == "initial" then
        prop1 "animation-name" arg

    else
        Preprocess.WithKeyframes arg.value


{-| An [`animation-delay`](https://developer.mozilla.org/en-US/docs/Web/CSS/animation-delay) property.
-}
animationDelay : Duration compatible -> Style
animationDelay arg =
    prop1 "animation-delay" arg


{-| An [`animation-duration`](https://developer.mozilla.org/en-US/docs/Web/CSS/animation-duration)
) property.
-}
animationDuration : Duration compatible -> Style
animationDuration arg =
    prop1 "animation-duration" arg


{-| An [`animation-iteration-count`](https://developer.mozilla.org/en-US/docs/Web/CSS/animation-iteration-count)
) property.
-}
animationIterationCount : NumberOrInfinite compatible -> Style
animationIterationCount arg =
    prop1 "animation-iteration-count" arg


{-| Sets [`transform`](https://developer.mozilla.org/en-US/docs/Web/CSS/transform)
with a series of transform-functions. If an empty list is provided, the CSS
output will be `none`, as if to state directly that the set of transforms
applied to the current selector is empty:

    transforms [] -- transform: none;

In the case that only one transform is needed, the `transform` function may be
used instead for convenience. `transform` expects exactly one transform-function
and cannot be passed `none`:

    transform (matrix 1 2 3 4 5 6) -- transform: matrix(1, 2, 3, 4, 5, 6);

If a collection of transforms is needed, use the `transforms` function with a
populated list:

    transforms [ perspective 1, scale2 1 1.4 ]

-}
transforms : List (Transform compatible) -> Style
transforms =
    prop1 "transform" << valuesOrNone


{-| Sets [`transform`](https://developer.mozilla.org/en-US/docs/Web/CSS/transform)
with a single explicit transform value. If you need to set the transform
property to `none`, use the `transforms` function with an empty list. See
`transforms` for more details.

    transform (scaleX 1.4)

-}
transform : Transform compatible -> Style
transform only =
    transforms [ only ]


{-| The `fill-box` value for the [`transform-box`](https://developer.mozilla.org/en-US/docs/Web/CSS/transform-box) property.
-}
fillBox : TransformBox {}
fillBox =
    { value = "fill-box"
    , transformBox = Compatible
    }


{-| The `content-box` value for the [`box-sizing`](https://developer.mozilla.org/en-US/docs/Web/CSS/box-sizing) property.
Can also be used as `content-box` value for the [`background-clip`](https://developer.mozilla.org/en-US/docs/Web/CSS/background-clip) property.
-}
contentBox : BoxSizing (BackgroundClip {})
contentBox =
    { value = "content-box"
    , boxSizing = Compatible
    , backgroundClip = Compatible
    }


{-| The `border-box` value for the [`box-sizing`](https://developer.mozilla.org/en-US/docs/Web/CSS/box-sizing) property.
Can also be used as `border-box` value for the [`background-clip`](https://developer.mozilla.org/en-US/docs/Web/CSS/background-clip) property.
-}
borderBox : BoxSizing (BackgroundClip {})
borderBox =
    { value = "border-box"
    , boxSizing = Compatible
    , backgroundClip = Compatible
    }


{-| The `view-box` value for the [`transform-box`](https://developer.mozilla.org/en-US/docs/Web/CSS/transform-box) property.
-}
viewBox : TransformBox {}
viewBox =
    { value = "view-box"
    , transformBox = Compatible
    }


{-| The [`transform-box`](https://developer.mozilla.org/en-US/docs/Web/CSS/transform-box) property.
-}
transformBox : TransformBox compatible -> Style
transformBox =
    prop1 "transform-box"


{-| Sets [`box-sizing`](https://developer.mozilla.org/en-US/docs/Web/CSS/box-sizing)

    boxSizing borderBox

-}
boxSizing : BoxSizing compatible -> Style
boxSizing =
    prop1 "box-sizing"


{-| The `preserve-3d` value for the [`transform-style`](https://developer.mozilla.org/en-US/docs/Web/CSS/transform-style) property.
-}
preserve3d : TransformStyle {}
preserve3d =
    { value = "preserve-3d"
    , transformStyle = Compatible
    }


{-| The `flat` value for the [`transform-style`](https://developer.mozilla.org/en-US/docs/Web/CSS/transform-style) property.
-}
flat : TransformStyle {}
flat =
    { value = "flat"
    , transformStyle = Compatible
    }


{-| The [`transform-style`](https://developer.mozilla.org/en-US/docs/Web/CSS/transform-style) property.
-}
transformStyle : TransformStyle compatible -> Style
transformStyle =
    prop1 "transform-style"



{- LIST STYLE POSITION -}


{-| The [`list-style-position`](https://developer.mozilla.org/en-US/docs/Web/CSS/list-style-position) property.
-}
listStylePosition : ListStylePosition compatible -> Style
listStylePosition =
    prop1 "list-style-position"


{-| -}
inside : ListStyle (ListStylePosition {})
inside =
    { value = "inside"
    , listStylePosition = Compatible
    , listStyleTypeOrPositionOrImage = Compatible
    }


{-| -}
outside : ListStyle (ListStylePosition {})
outside =
    { value = "outside"
    , listStylePosition = Compatible
    , listStyleTypeOrPositionOrImage = Compatible
    }



{- LIST STYLE TYPE -}


{-| The [`list-style-type`](https://developer.mozilla.org/en-US/docs/Web/CSS/list-style-type) property.
-}
listStyleType : ListStyleType compatible -> Style
listStyleType =
    prop1 "list-style-type"


{-| -}
disc : ListStyle (ListStyleType {})
disc =
    { value = "disc"
    , listStyleType = Compatible
    , listStyleTypeOrPositionOrImage = Compatible
    }


{-| -}
circle : ListStyle (ListStyleType {})
circle =
    { value = "circle"
    , listStyleType = Compatible
    , listStyleTypeOrPositionOrImage = Compatible
    }


{-| -}
square : ListStyle (ListStyleType {})
square =
    { value = "square"
    , listStyleType = Compatible
    , listStyleTypeOrPositionOrImage = Compatible
    }


{-| -}
decimal : ListStyle (ListStyleType {})
decimal =
    { value = "decimal"
    , listStyleType = Compatible
    , listStyleTypeOrPositionOrImage = Compatible
    }


{-| -}
decimalLeadingZero : ListStyle (ListStyleType {})
decimalLeadingZero =
    { value = "decimal-leading-zero"
    , listStyleType = Compatible
    , listStyleTypeOrPositionOrImage = Compatible
    }


{-| -}
lowerRoman : ListStyle (ListStyleType {})
lowerRoman =
    { value = "lower-roman"
    , listStyleType = Compatible
    , listStyleTypeOrPositionOrImage = Compatible
    }


{-| -}
upperRoman : ListStyle (ListStyleType {})
upperRoman =
    { value = "upper-roman"
    , listStyleType = Compatible
    , listStyleTypeOrPositionOrImage = Compatible
    }


{-| -}
lowerGreek : ListStyle (ListStyleType {})
lowerGreek =
    { value = "lower-greek"
    , listStyleType = Compatible
    , listStyleTypeOrPositionOrImage = Compatible
    }


{-| -}
upperGreek : ListStyle (ListStyleType {})
upperGreek =
    { value = "upper-greek"
    , listStyleType = Compatible
    , listStyleTypeOrPositionOrImage = Compatible
    }


{-| -}
lowerAlpha : ListStyle (ListStyleType {})
lowerAlpha =
    { value = "lower-alpha"
    , listStyleType = Compatible
    , listStyleTypeOrPositionOrImage = Compatible
    }


{-| -}
upperAlpha : ListStyle (ListStyleType {})
upperAlpha =
    { value = "upper-alpha"
    , listStyleType = Compatible
    , listStyleTypeOrPositionOrImage = Compatible
    }


{-| -}
lowerLatin : ListStyle (ListStyleType {})
lowerLatin =
    { value = "lower-latin"
    , listStyleType = Compatible
    , listStyleTypeOrPositionOrImage = Compatible
    }


{-| -}
upperLatin : ListStyle (ListStyleType {})
upperLatin =
    { value = "upper-latin"
    , listStyleType = Compatible
    , listStyleTypeOrPositionOrImage = Compatible
    }


{-| -}
arabicIndic : ListStyle (ListStyleType {})
arabicIndic =
    { value = "arabic-indic"
    , listStyleType = Compatible
    , listStyleTypeOrPositionOrImage = Compatible
    }


{-| -}
armenian : ListStyle (ListStyleType {})
armenian =
    { value = "armenian"
    , listStyleType = Compatible
    , listStyleTypeOrPositionOrImage = Compatible
    }


{-| -}
bengali : ListStyle (ListStyleType {})
bengali =
    { value = "bengali"
    , listStyleType = Compatible
    , listStyleTypeOrPositionOrImage = Compatible
    }


{-| -}
cjkEarthlyBranch : ListStyle (ListStyleType {})
cjkEarthlyBranch =
    { value = "cjk-earthly-branch"
    , listStyleType = Compatible
    , listStyleTypeOrPositionOrImage = Compatible
    }


{-| -}
cjkHeavenlyStem : ListStyle (ListStyleType {})
cjkHeavenlyStem =
    { value = "cjk-heavenly-stem"
    , listStyleType = Compatible
    , listStyleTypeOrPositionOrImage = Compatible
    }


{-| -}
devanagari : ListStyle (ListStyleType {})
devanagari =
    { value = "devanagari"
    , listStyleType = Compatible
    , listStyleTypeOrPositionOrImage = Compatible
    }


{-| -}
georgian : ListStyle (ListStyleType {})
georgian =
    { value = "georgian"
    , listStyleType = Compatible
    , listStyleTypeOrPositionOrImage = Compatible
    }


{-| -}
gujarati : ListStyle (ListStyleType {})
gujarati =
    { value = "gujarati"
    , listStyleType = Compatible
    , listStyleTypeOrPositionOrImage = Compatible
    }


{-| -}
gurmukhi : ListStyle (ListStyleType {})
gurmukhi =
    { value = "gurmukhi"
    , listStyleType = Compatible
    , listStyleTypeOrPositionOrImage = Compatible
    }


{-| -}
kannada : ListStyle (ListStyleType {})
kannada =
    { value = "kannada"
    , listStyleType = Compatible
    , listStyleTypeOrPositionOrImage = Compatible
    }


{-| -}
khmer : ListStyle (ListStyleType {})
khmer =
    { value = "khmer"
    , listStyleType = Compatible
    , listStyleTypeOrPositionOrImage = Compatible
    }


{-| -}
lao : ListStyle (ListStyleType {})
lao =
    { value = "lao"
    , listStyleType = Compatible
    , listStyleTypeOrPositionOrImage = Compatible
    }


{-| -}
malayalam : ListStyle (ListStyleType {})
malayalam =
    { value = "malayalam"
    , listStyleType = Compatible
    , listStyleTypeOrPositionOrImage = Compatible
    }


{-| -}
myanmar : ListStyle (ListStyleType {})
myanmar =
    { value = "myanmar"
    , listStyleType = Compatible
    , listStyleTypeOrPositionOrImage = Compatible
    }


{-| -}
oriya : ListStyle (ListStyleType {})
oriya =
    { value = "oriya"
    , listStyleType = Compatible
    , listStyleTypeOrPositionOrImage = Compatible
    }


{-| -}
telugu : ListStyle (ListStyleType {})
telugu =
    { value = "telugu"
    , listStyleType = Compatible
    , listStyleTypeOrPositionOrImage = Compatible
    }


{-| -}
thai : ListStyle (ListStyleType {})
thai =
    { value = "thai"
    , listStyleType = Compatible
    , listStyleTypeOrPositionOrImage = Compatible
    }



{- LIST STYLE SHORTHAND -}


{-| The [`list-style`](https://developer.mozilla.org/en-US/docs/Web/CSS/list-style) shorthand property.
-}
listStyle : ListStyle compatible -> Style
listStyle =
    prop1 "list-style"


{-| The [`list-style`](https://developer.mozilla.org/en-US/docs/Web/CSS/list-style) shorthand property.
-}
listStyle2 : ListStyle compatible1 -> ListStyle compatible2 -> Style
listStyle2 =
    prop2 "list-style"


{-| The [`list-style`](https://developer.mozilla.org/en-US/docs/Web/CSS/list-style) shorthand property.
-}
listStyle3 : ListStyle compatible1 -> ListStyle compatible2 -> ListStyle compatible3 -> Style
listStyle3 =
    prop3 "list-style"



{- FLEX BOX -}


{-| Sets [`flex`](https://developer.mozilla.org/en-US/docs/Web/CSS/flex) property.

flex (none | content | auto | (int 1) | (px 10))
flex2 (int 1) ((int 1) | (px 10 ))
flex3 (int 1) (int 1) ((int 1) | (px 10))

-}
flex : LengthOrNumberOrAutoOrNoneOrContent compatible -> Style
flex =
    prop1 "flex"


{-| Sets [`flex`](https://developer.mozilla.org/en-US/docs/Web/CSS/flex) property.

flex (none | content | auto | (int 1) | (px 10))
flex2 (int 1) ((int 1) | (px 10 ))
flex3 (int 1) (int 1) ((int 1) | (px 10))

-}
flex2 : Number compatibleA -> LengthOrNumber compatibleB -> Style
flex2 =
    prop2 "flex"


{-| Sets [`flex`](https://developer.mozilla.org/en-US/docs/Web/CSS/flex) property.

flex (none | content | auto | (int 1) | (px 10))
flex2 (int 1) ((int 1) | (px 10 ))
flex3 (int 1) (int 1) ((int 1) | (px 10))

-}
flex3 : Number compatibleA -> Number compatibleB -> LengthOrNumber compatibleC -> Style
flex3 =
    prop3 "flex"


{-| Sets [`flex-basis`](https://developer.mozilla.org/en-US/docs/Web/CSS/flex-basis) property.

flex (none | content | auto | (int 1) | (px 10))
flex2 (int 1) ((int 1) | (px 10 ))
flex3 (int 1) (int 1) ((int 1) | (px 10))

-}
flexBasis : FlexBasis compatible -> Style
flexBasis =
    prop1 "flex-basis"


{-| Sets [`flex-grow`](https://developer.mozilla.org/en-US/docs/Web/CSS/flex-grow) property.
-}
flexGrow : Number compatible -> Style
flexGrow =
    prop1 "flex-grow"


{-| Sets [`flex-shrink`](https://developer.mozilla.org/en-US/docs/Web/CSS/flex-shrink) property.
-}
flexShrink : Number compatible -> Style
flexShrink =
    prop1 "flex-shrink"


{-| Sets [`flex-wrap`](https://developer.mozilla.org/en-US/docs/Web/CSS/flex-wrap) property.
-}
flexWrap : FlexWrap compatible -> Style
flexWrap =
    prop1 "flex-wrap"


{-| Sets [`flex-direction`](https://developer.mozilla.org/en-US/docs/Web/CSS/flex-direction) property.
-}
flexDirection : FlexDirection compatible -> Style
flexDirection =
    prop1 "flex-direction"


{-| Sets [`flex-flow`](https://developer.mozilla.org/en-US/docs/Web/CSS/flex-flow) property.

flexFlow1 (wrap | wrapReverse | noWrap)
flexFlow2 (wrap | wrapReverse | noWrap) (row | column | rowReverse | columnReverse)

Or vice versa, order is not important for flex-flow

-}
flexFlow1 : FlexDirectionOrWrap compatible -> Style
flexFlow1 =
    prop1 "flex-flow"


{-| Sets [`flex-flow`](https://developer.mozilla.org/en-US/docs/Web/CSS/flex-flow) property.

flexFlow1 (wrap | wrapReverse | noWrap)
flexFlow2 (wrap | wrapReverse | noWrap) (row | column | rowReverse | columnReverse)

Or vice versa, order is not important for flex-flow

-}
flexFlow2 : FlexDirectionOrWrap compatibleA -> FlexDirectionOrWrap compatibleB -> Style
flexFlow2 =
    prop2 "flex-flow"


{-| Sets [`align-items`](https://developer.mozilla.org/en-US/docs/Web/CSS/align-items).

**Note:** `auto` is not currently supported here. If you need to set this property to `auto`,
use this workaround:

property "align-items" "auto"

If this is annoying, please file an issue, so adding support for "auto"
can be prioritized!

-}
alignItems : (ExplicitLength IncompatibleUnits -> Style) -> Style
alignItems fn =
    getOverloadedProperty "alignItems" "align-items" (fn lengthForOverloadedProperty)


{-| Sets [`align-self`](https://developer.mozilla.org/en-US/docs/Web/CSS/align-self).

**Note:** `auto` is not currently supported here. If you need to set this property to `auto`,
use this workaround:

property "align-self" "auto"

If this is annoying, please file an issue, so adding support for "auto"
can be prioritized!

-}
alignSelf : (ExplicitLength IncompatibleUnits -> Style) -> Style
alignSelf fn =
    getOverloadedProperty "alignSelf" "align-self" (fn lengthForOverloadedProperty)


{-| Sets [`justify-content`](https://developer.mozilla.org/en-US/docs/Web/CSS/justify-content).
-}
justifyContent : (ExplicitLength IncompatibleUnits -> Style) -> Style
justifyContent fn =
    getOverloadedProperty "justifyContent" "justify-content" (fn lengthForOverloadedProperty)


{-| Sets [`order`](https://developer.mozilla.org/en-US/docs/Web/CSS/order) property.
-}
order : Number compatible -> Style
order =
    prop1 "order"


{-| The [`content`](https://developer.mozilla.org/en-US/docs/Web/CSS/flex-basis#Values) value for the
flex-basis property.
-}
content : LengthOrNumberOrAutoOrNoneOrContent (FlexBasis {})
content =
    { value = "content"
    , flexBasis = Compatible
    , lengthOrNumberOrAutoOrNoneOrContent = Compatible
    }


{-| The[`wrap`](https://developer.mozilla.org/en-US/docs/Web/CSS/flex-wrap#Values) value for the
flex-wrap property.
-}
wrap : FlexDirectionOrWrap (FlexWrap {})
wrap =
    { value = "wrap"
    , flexWrap = Compatible
    , flexDirectionOrWrap = Compatible
    }


{-| The[`wrap-reverse`](https://developer.mozilla.org/en-US/docs/Web/CSS/flex-wrap#Values) value for the
flex-wrap property.
-}
wrapReverse : FlexDirectionOrWrap (FlexWrap {})
wrapReverse =
    { wrap | value = "wrap-reverse" }


{-| The[`flex-start`](https://developer.mozilla.org/en-US/docs/Web/CSS/align-items#Values) value for the
align-items property.
Can also be used with flex-box's justify-content property to apply the value of flex-start.
-}
flexStart : AlignItems a b
flexStart =
    prop1 "flex-start"


{-| The[`flex-end`](https://developer.mozilla.org/en-US/docs/Web/CSS/align-items#Values) value for the
align-items property.
Can also be used with flex-box's justify-content property to apply the value of flex-end.
-}
flexEnd : AlignItems a b
flexEnd =
    prop1 "flex-end"


{-| The[`space-around`](https://developer.mozilla.org/en-US/docs/Web/CSS/justify-content#Values) value for the
justify-content property.
-}
spaceAround : JustifyContent a b
spaceAround =
    prop1 "space-around"


{-| The[`space-between`](https://developer.mozilla.org/en-US/docs/Web/CSS/justify-content#Values) value for the
justify-content property.
-}
spaceBetween : JustifyContent a b
spaceBetween =
    prop1 "space-between"


{-| The[`stretch`](https://developer.mozilla.org/en-US/docs/Web/CSS/align-items#Values) value for the
align-items property.
-}
stretch : AlignItems a b
stretch =
    prop1 "stretch"


{-| The[`row`](<https://developer.mozilla.org/en-US/docs/Web/CSS/flex-direction> #Values) value for the
flex-direction property.
-}
row : FlexDirectionOrWrap (FlexDirection {})
row =
    { value = "row"
    , flexDirection = Compatible
    , flexDirectionOrWrap = Compatible
    }


{-| The[`row-reverse`](<https://developer.mozilla.org/en-US/docs/Web/CSS/flex-direction> #Values) value for the
flex-direction property.
-}
rowReverse : FlexDirectionOrWrap (FlexDirection {})
rowReverse =
    { row | value = "row-reverse" }


{-| The[`column`](<https://developer.mozilla.org/en-US/docs/Web/CSS/flex-direction> #Values) value for the
flex-direction property.
-}
column : FlexDirectionOrWrap (FlexDirection {})
column =
    { row | value = "column" }


{-| The[`column-reverse`](<https://developer.mozilla.org/en-US/docs/Web/CSS/flex-direction> #Values) value for the
flex-direction property.
-}
columnReverse : FlexDirectionOrWrap (FlexDirection {})
columnReverse =
    { row | value = "column-reverse" }



{- TEXT DECORATION LINES -}


{-| An [`underline`](https://developer.mozilla.org/en-US/docs/Web/CSS/text-decoration-line#Value)
text decoration line.
-}
underline : TextDecorationLine {}
underline =
    { value = "underline"
    , textDecorationLine = Compatible
    }


{-| An [`overline`](https://developer.mozilla.org/en-US/docs/Web/CSS/text-decoration-line#Value)
text decoration line.
-}
overline : TextDecorationLine {}
overline =
    { value = "overline"
    , textDecorationLine = Compatible
    }


{-| A [`line-through`](https://developer.mozilla.org/en-US/docs/Web/CSS/text-decoration-line#Value)
text decoration line.
-}
lineThrough : TextDecorationLine {}
lineThrough =
    { value = "line-through"
    , textDecorationLine = Compatible
    }



{- BACKGROUND -}


{-| The `repeat-x` [`background-repeat`](https://developer.mozilla.org/en-US/docs/Web/CSS/background-repeat) value.
-}
repeatX : BackgroundRepeatShorthand {}
repeatX =
    { value = "repeat-x"
    , backgroundRepeatShorthand = Compatible
    }


{-| The `repeat-y` [`background-repeat`](https://developer.mozilla.org/en-US/docs/Web/CSS/background-repeat) value.
-}
repeatY : BackgroundRepeatShorthand {}
repeatY =
    { value = "repeat-y"
    , backgroundRepeatShorthand = Compatible
    }


{-| The `repeat` [`background-repeat`](https://developer.mozilla.org/en-US/docs/Web/CSS/background-repeat) value.
-}
repeat : BackgroundRepeat {}
repeat =
    { value = "repeat"
    , backgroundRepeat = Compatible
    , backgroundRepeatShorthand = Compatible
    }


{-| The `space` [`background-repeat`](https://developer.mozilla.org/en-US/docs/Web/CSS/background-repeat) value.
-}
space : BackgroundRepeat {}
space =
    { value = "space"
    , backgroundRepeat = Compatible
    , backgroundRepeatShorthand = Compatible
    }


{-| The `round` [`background-repeat`](https://developer.mozilla.org/en-US/docs/Web/CSS/background-repeat) value.
-}
round : BackgroundRepeat {}
round =
    { value = "round"
    , backgroundRepeat = Compatible
    , backgroundRepeatShorthand = Compatible
    }


{-| The `no-repeat` [`background-repeat`](https://developer.mozilla.org/en-US/docs/Web/CSS/background-repeat) value.
-}
noRepeat : BackgroundRepeat {}
noRepeat =
    { value = "no-repeat"
    , backgroundRepeat = Compatible
    , backgroundRepeatShorthand = Compatible
    }


{-| The `local` [`background-attachment`](https://developer.mozilla.org/en-US/docs/Web/CSS/background-attachment) value.
-}
local : BackgroundAttachment {}
local =
    { value = "local"
    , backgroundAttachment = Compatible
    }



{- LINEAR GRADIENT -}


{-| <https://developer.mozilla.org/en-US/docs/Web/CSS/linear-gradient#Values>
-}
type alias ColorStop compatibleA compatibleB unit =
    ( ColorValue compatibleA, Maybe (Length compatibleB unit) )


{-| Sets [`linear-gradient`](https://developer.mozilla.org/en-US/docs/Web/CSS/linear-gradient)

    linearGradient (stop2 red <| pct 75) (stop <| hex "222") []

    linearGradient (stop red) (stop <| hex "222") [ stop green, stop blue ]

-}
linearGradient :
    ColorStop compatibleA compatibleB unit
    -> ColorStop compatibleA compatibleB unit
    -> List (ColorStop compatibleA compatibleB unit)
    -> BackgroundImage (ListStyle {})
linearGradient firstStop secondStop otherStops =
    -- TODO we should make this more permissive, e.g. compatibleA/compatibleB/compatibleC/compatibleD
    -- the only reason it isn't is that we happen to be using collectStops like this.
    -- We should just not use collectStops. Same with linearGradient2
    { value =
        ([ firstStop, secondStop ] ++ otherStops)
            |> collectStops
            |> cssFunction "linear-gradient"
    , backgroundImage = Compatible
    , listStyleTypeOrPositionOrImage = Compatible
    }


{-| Sets [`linear-gradient`](https://developer.mozilla.org/en-US/docs/Web/CSS/linear-gradient)

    linearGradient2 toBottomLeft (stop2 red <| pct 75) (stop <| hex "222") []

    linearGradient2 toTop (stop red) (stop <| hex "222") [ stop green, stop blue ]

-}
linearGradient2 :
    AngleOrDirection compatible
    -> ColorStop compatibleA compatibleB unit
    -> ColorStop compatibleA compatibleB unit
    -> List (ColorStop compatibleA compatibleB unit)
    -> BackgroundImage (ListStyle {})
linearGradient2 direction firstStop secondStop otherStops =
    { value =
        ([ firstStop, secondStop ] ++ otherStops)
            |> collectStops
            |> (::) direction.value
            |> cssFunction "linear-gradient"
    , backgroundImage = Compatible
    , listStyleTypeOrPositionOrImage = Compatible
    }


collectStops : List (ColorStop compatibleA compatibleB unit) -> List String
collectStops =
    List.map <|
        \( c, len ) ->
            len
                |> Maybe.map (String.cons ' ' << .value)
                |> Maybe.withDefault ""
                |> String.append c.value


{-| [`ColorStop`](https://developer.mozilla.org/en-US/docs/Web/CSS/linear-gradient#Values)
-}
stop : ColorValue compatibleA -> ColorStop compatibleA compatibleB unit
stop c =
    ( c, Nothing )


{-| [`ColorStop`](https://developer.mozilla.org/en-US/docs/Web/CSS/linear-gradient#Values)
-}
stop2 : ColorValue compatibleA -> Length compatibleB unit -> ColorStop compatibleA compatibleB unit
stop2 c len =
    ( c, Just len )


{-| Sets the direction to [`top`](https://developer.mozilla.org/en-US/docs/Web/CSS/linear-gradient#Values)
-}
toTop : AngleOrDirection {}
toTop =
    { value = "to top"
    , angleOrDirection = Compatible
    }


{-| Sets the direction to [`top right`](https://developer.mozilla.org/en-US/docs/Web/CSS/linear-gradient#Values)
-}
toTopRight : AngleOrDirection {}
toTopRight =
    { value = "to top right"
    , angleOrDirection = Compatible
    }


{-| Sets the direction to [`right`](https://developer.mozilla.org/en-US/docs/Web/CSS/linear-gradient#Values)
-}
toRight : AngleOrDirection {}
toRight =
    { value = "to right"
    , angleOrDirection = Compatible
    }


{-| Sets the direction to [`bottom right`](https://developer.mozilla.org/en-US/docs/Web/CSS/linear-gradient#Values)
-}
toBottomRight : AngleOrDirection {}
toBottomRight =
    { value = "to bottom right"
    , angleOrDirection = Compatible
    }


{-| Sets the direction to [`bottom`](https://developer.mozilla.org/en-US/docs/Web/CSS/linear-gradient#Values)
-}
toBottom : AngleOrDirection {}
toBottom =
    { value = "to bottom"
    , angleOrDirection = Compatible
    }


{-| Sets the direction to [`bottom left`](https://developer.mozilla.org/en-US/docs/Web/CSS/linear-gradient#Values)
-}
toBottomLeft : AngleOrDirection {}
toBottomLeft =
    { value = "to bottom left"
    , angleOrDirection = Compatible
    }


{-| Sets the direction to [`left`](https://developer.mozilla.org/en-US/docs/Web/CSS/linear-gradient#Values)
-}
toLeft : AngleOrDirection {}
toLeft =
    { value = "to left"
    , angleOrDirection = Compatible
    }


{-| Sets the direction to [`top left`](https://developer.mozilla.org/en-US/docs/Web/CSS/linear-gradient#Values)
-}
toTopLeft : AngleOrDirection {}
toTopLeft =
    { value = "to top left"
    , angleOrDirection = Compatible
    }



{- BORDERS -}


{-| -}
block : Display {}
block =
    { value = "block"
    , display = Compatible
    }


{-| -}
inlineBlock : Display {}
inlineBlock =
    { value = "inline-block"
    , display = Compatible
    }


{-| Sets the display style to [`inline-flex`](https://developer.mozilla.org/en-US/docs/Web/CSS/display#Values)
-}
inlineFlex : Display {}
inlineFlex =
    { value = "inline-flex"
    , display = Compatible
    }


{-| -}
inline : Display {}
inline =
    { value = "inline"
    , display = Compatible
    }


{-| Sets the display style to [`table`](https://developer.mozilla.org/en-US/docs/Web/CSS/display#Values)
-}
table : Display {}
table =
    { value = "table"
    , display = Compatible
    }


{-| Sets the display style to [`inline-table`](https://developer.mozilla.org/en-US/docs/Web/CSS/display#Values)
-}
inlineTable : Display {}
inlineTable =
    { value = "inline-table"
    , display = Compatible
    }


{-| Sets the display style to [`table-row`](https://developer.mozilla.org/en-US/docs/Web/CSS/display#Values)
-}
tableRow : Display {}
tableRow =
    { value = "table-row"
    , display = Compatible
    }


{-| Sets the display style to [`table-cell`](https://developer.mozilla.org/en-US/docs/Web/CSS/display#Values)
-}
tableCell : Display {}
tableCell =
    { value = "table-cell"
    , display = Compatible
    }


{-| Sets the display style to [`table-column`](https://developer.mozilla.org/en-US/docs/Web/CSS/display#Values)
-}
tableColumn : Display {}
tableColumn =
    { value = "table-column"
    , display = Compatible
    }


{-| Sets the display style to [`table-caption`](https://developer.mozilla.org/en-US/docs/Web/CSS/display#Values)
-}
tableCaption : Display {}
tableCaption =
    { value = "table-caption"
    , display = Compatible
    }


{-| Sets the display style to [`table-row-group`](https://developer.mozilla.org/en-US/docs/Web/CSS/display#Values)
-}
tableRowGroup : Display {}
tableRowGroup =
    { value = "table-row-group"
    , display = Compatible
    }


{-| Sets the display style to [`table-column-group`](https://developer.mozilla.org/en-US/docs/Web/CSS/display#Values)
-}
tableColumnGroup : Display {}
tableColumnGroup =
    { value = "table-column-group"
    , display = Compatible
    }


{-| Sets the display style to [`table-header-group`](https://developer.mozilla.org/en-US/docs/Web/CSS/display#Values)
-}
tableHeaderGroup : Display {}
tableHeaderGroup =
    { value = "table-header-group"
    , display = Compatible
    }


{-| Sets the display style to [`table-footer-group`](https://developer.mozilla.org/en-US/docs/Web/CSS/display#Values)
-}
tableFooterGroup : Display {}
tableFooterGroup =
    { value = "table-footer-group"
    , display = Compatible
    }


{-| -}
listItem : Display {}
listItem =
    { value = "list-item"
    , display = Compatible
    }


{-| -}
inlineListItem : Display {}
inlineListItem =
    { value = "inline-list-item"
    , display = Compatible
    }
