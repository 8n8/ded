module Css exposing
    ( Style, batch
    , property, flex, flex2, flex3, medium, alignSelf, alignItems, justifyContent, order, flexDirection, flexFlow1, flexFlow2, flexWrap, flexBasis, flexGrow, flexShrink, transformStyle, transformBox, transform, transforms, currentColor, underline, overline, lineThrough, textOrientation, textDecoration, textDecoration2, textDecoration3, textDecorations, textDecorations2, textDecorations3, textDecorationLine, textDecorationLines, textDecorationStyle, textEmphasisColor, capitalize, uppercase, lowercase, fullWidth, hanging, eachLine, textIndent, textIndent2, textIndent3, clip, ellipsis, textOverflow, optimizeSpeed, optimizeLegibility, geometricPrecision, textRendering, textTransform, textAlign, textAlignLast, left, right, center, justify, justifyAll, start, end, matchParent, true, verticalAlign, display, opacity, minContent, maxContent, fitContent, fillAvailable, width, minWidth, maxWidth, height, minHeight, maxHeight, padding, padding2, padding3, padding4, paddingTop, paddingBottom, paddingRight, paddingLeft, pointerEvents, margin, margin2, margin3, margin4, marginTop, marginBottom, marginRight, marginLeft, marginBlockStart, marginBlockEnd, marginInlineStart, marginInlineEnd, boxSizing, overflow, overflowX, overflowY, overflowWrap, whiteSpace, backgroundColor, color, textShadow, textShadow2, textShadow3, textShadow4, boxShadow, boxShadow2, boxShadow3, boxShadow4, boxShadow5, boxShadow6, lineHeight, letterSpacing, fontFace, fontFamily, fontSize, fontStyle, fontWeight, fontVariant, fontVariant2, fontVariant3, fontVariantLigatures, fontVariantCaps, fontVariantNumeric, fontVariantNumeric2, fontVariantNumeric3, fontFamilies, fontVariantNumerics, fontFeatureSettings, fontFeatureSettingsList, cursor, outline, outline3, outlineColor, outlineWidth, outlineStyle, outlineOffset, zIndex, spaceAround, spaceBetween, resize, fill, touchAction, borderSpacing, borderSpacing2, visibility
    , Color, all, important, solid, transparent, rgb, rgba, hsl, hsla, hex
    , featureTag, featureTag2, featureOn, featureOff
    , borderCollapse, borderColor, borderColor2, borderColor3, borderColor4, borderBottomLeftRadius, borderBottomLeftRadius2, borderBottomRightRadius, borderBottomRightRadius2, borderTopLeftRadius, borderTopLeftRadius2, borderTopRightRadius, borderTopRightRadius2, borderRadius, borderRadius2, borderRadius3, borderRadius4, borderWidth, borderWidth2, borderWidth3, borderWidth4, borderBottomWidth, borderLeftWidth, borderRightWidth, borderTopWidth, borderBottomStyle, borderLeftStyle, borderRightStyle, borderTopStyle, borderStyle, borderBottomColor, borderLeftColor, borderRightColor, borderTopColor, borderBox, contentBox, border, border2, border3, borderTop, borderTop2, borderTop3, borderBottom, borderBottom2, borderBottom3, borderLeft, borderLeft2, borderLeft3, borderRight, borderRight2, borderRight3, borderImageOutset, borderImageOutset2, borderImageOutset3, borderImageOutset4, borderImageWidth, borderImageWidth2, borderImageWidth3, borderImageWidth4, scroll, visible, block, inlineBlock, inlineFlex, inline, none, auto, inherit, unset, initial, noWrap, top, static, fixed, sticky, relative, absolute, position, float, bottom, middle, baseline, sub, super, textTop, textBottom, hidden, wavy, dotted, dashed, double, groove, ridge, inset, outset, matrix, matrix3d, perspective, rotate3d, rotateX, rotateY, rotateZ, scale, scale2, scale3d, scaleX, scaleY, skew, skew2, skewX, skewY, translate, translate2, translate3d, translateX, translateY, translateZ, rotate, fillBox, viewBox, flat, preserve3d, content, wrapReverse, wrap, flexStart, flexEnd, stretch, row, rowReverse, column, columnReverse, serif, sansSerif, monospace, cursive, fantasy, xxSmall, xSmall, small, large, xLarge, xxLarge, smaller, larger, normal, italic, oblique, bold, lighter, bolder, smallCaps, allSmallCaps, petiteCaps, allPetiteCaps, unicase, titlingCaps, commonLigatures, noCommonLigatures, discretionaryLigatures, noDiscretionaryLigatures, historicalLigatures, noHistoricalLigatures, contextual, noContextual, liningNums, oldstyleNums, proportionalNums, tabularNums, diagonalFractions, stackedFractions, ordinal, slashedZero, default, pointer, crosshair, contextMenu, help, progress, wait, cell, text_, verticalText, cursorAlias, copy, move, noDrop, notAllowed, eResize, nResize, neResize, nwResize, sResize, seResize, swResize, wResize, ewResize, nsResize, neswResize, nwseResize, colResize, rowResize, allScroll, zoomIn, zoomOut, grab, grabbing, visiblePainted, visibleFill, visibleStroke, painted, stroke
    , Length, pct, px, em, pt, ex, ch, rem, vh, vw, vmin, vmax, mm, cm, inches, pc, int, num, zero, calc, plus, minus
    , Px, Em, Rem, Pct, Ex, Ch, Vh, Vw, Vmin, Vmax, Mm, Cm, In, Pt, Pc
    , deg, rad, grad, turn
    , Duration, sec, ms
    , pseudoClass, active, any, checked, disabled, empty, enabled, first, firstChild, firstOfType, fullscreen, focus, hover, visited, indeterminate, invalid, lang, lastChild, lastOfType, link, nthChild, nthLastChild, nthLastOfType, nthOfType, onlyChild, onlyOfType, optional, outOfRange, readWrite, required, root, scope, target, valid
    , pseudoElement, after, before, firstLetter, firstLine, selection
    , src_
    , qt
    , listStyleType, disc, circle, square, decimal, decimalLeadingZero, lowerRoman, upperRoman, lowerGreek, lowerAlpha, lowerLatin, upperAlpha, upperLatin, arabicIndic, armenian, bengali, cjkEarthlyBranch, cjkHeavenlyStem, devanagari, georgian, gujarati, gurmukhi, kannada, khmer, lao, malayalam, myanmar, oriya, telugu, thai
    , listStylePosition, inside, outside
    , listStyle, listStyle2, listStyle3
    , linearGradient, linearGradient2, stop, stop2, toBottom, toBottomLeft, toBottomRight, toLeft, toRight, toTop, toTopLeft, toTopRight
    , AlignItems, All, Angle, AngleOrDirection, BackgroundAttachment, BackgroundBlendMode, BackgroundClip, BackgroundImage, BackgroundOrigin, BackgroundRepeat, BackgroundRepeatShorthand, BasicProperty, BorderCollapse, BorderStyle, BoxSizing, Calc, CalculatedLength, CalcExpression, Cursor, Display, ExplicitLength, FeatureTagValue, FlexBasis, FlexDirection, FlexDirectionOrWrap, FlexWrap, FontFamily, FontStyle, FontStyleOrFeatureTagValue, FontVariant, FontVariantCaps, FontVariantLigatures, FontVariantNumeric, FontWeight, ImportType, IncompatibleUnits, JustifyContent, LengthOrAuto, LengthOrAutoOrCoverOrContain, LengthOrMinMaxDimension, LengthOrNone, LengthOrNoneOrMinMaxDimension, LengthOrNumber, LengthOrNumberOrAutoOrNoneOrContent, ListStyle, ListStylePosition, ListStyleType, MinMaxDimension, NonMixable, None, Number, Infinite, NumberOrInfinite, Outline, Overflow, Visibility, Position, Resize, TableLayout, TextDecorationLine, TextDecorationStyle, TextIndent, TextOrientation, TextOverflow, TextRendering, TextTransform, TouchAction, Transform, TransformBox, TransformStyle, Value, VerticalAlign, WhiteSpace, Wrap, pre, preLine, preWrap, infinite
    , url, vertical, tableRowGroup, tableRow, tableLayout, tableHeaderGroup, tableFooterGroup, tableColumnGroup, tableCell, tableColumn, tableCaption, table, space, softLight, separate, screenBlendMode, saturation, round, repeatY, repeatX, repeat, pointerEventsFill, pointerEventsAll
    , Compatible
    , backgroundAttachment
    , backgroundBlendMode
    , backgroundClip
    , backgroundImage
    , backgroundOrigin
    , backgroundPosition
    , backgroundPosition2
    , backgroundRepeat
    , backgroundRepeat2
    , backgroundSize
    , backgroundSize2
    , both
    , breakWord
    , collapse
    , colorBurn
    , colorDodge
    , contain
    , cover
    , darken
    , difference
    , displayFlex
    , exclusion
    , hardLight
    , horizontal
    , hue
    , inlineListItem
    , inlineTable
    , lighten
    , listItem
    , local
    , luminosity
    , manipulation
    , multiply
    , noRepeat
    , overlay
    , paddingBox
    , panDown
    , panLeft
    , panRight
    , panUp
    , panX
    , panY
    , pinchZoom
    , animationName
    , animationDelay
    , animationDuration
    , animationIterationCount
    , FontSize, ColorValue, ColorStop, IntOrAuto
    , thin, thick, blink
    )


import Css.Animations exposing (Keyframes)
import Css.Internal exposing (getOverloadedProperty, lengthConverter, lengthForOverloadedProperty)
import Css.Preprocess as Preprocess exposing (Style, unwrapSnippet)
import Css.String
import Css.Structure as Structure exposing (..)
import Hex
import String


{-| -}
type alias Style =
    Preprocess.Style



{- Length -}


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
