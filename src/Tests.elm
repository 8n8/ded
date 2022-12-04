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

{-| Define CSS styles in Elm.

    import Css exposing (..)
    import Html
    import Html.Styled exposing (..)
    import Html.Styled.Attributes exposing (css, href, src)
    import Html.Styled.Events exposing (onClick)

    {-| A logo image, with inline styles that change on hover.
    -}
    logo : Html msg
    logo =
        img
            [ src "logo.png"
            , css
                [ display inlineBlock
                , padding (px 20)
                , border3 (px 5) solid (rgb 120 120 120)
                , hover
                    [ borderColor theme.primary
                    , borderRadius (px 10)
                    ]
                ]
            ]
            []

    {-| A plain old record holding a couple of theme colors.
    -}
    theme : { secondary : Color, primary : Color }
    theme =
        { primary = hex "55af6a"
        , secondary = rgb 250 240 230
        }

    {-| A reusable button which has some styles pre-applied to it.
    -}
    btn : List (Attribute msg) -> List (Html msg) -> Html msg
    btn =
        styled button
            [ margin (px 12)
            , color (rgb 250 250 250)
            , hover
                [ backgroundColor theme.primary
                , textDecoration underline
                ]
            ]

    {-| A reusable style. Css.batch combines multiple styles into one, much
    like mixins in CSS preprocessors.
    -}
    paragraphFont : Style
    paragraphFont =
        Css.batch
            [ fontFamilies [ "Palatino Linotype", "Georgia", "serif" ]
            , fontSize (px 16)
            , fontWeight normal
            ]

    {-| Css.property lets you define custom properties, using strings as their values.
    -}
    legacyBorderRadius : String -> Style
    legacyBorderRadius amount =
        Css.batch
            [ property "-moz-border-radius" amount
            , property "-webkit-border-top-left-radius" amount
            , property "-webkit-border-top-right-radius" amount
            , property "-webkit-border-bottom-right-radius" amount
            , property "-webkit-border-bottom-left-radius" amount
            , property "border-radius" amount
            ]

    view : Model -> Html Msg
    view model =
        nav []
            [ img [ src "assets/backdrop.jpg", css [ width (pct 100) ] ] []
            , btn [ onClick DoSomething ] [ text "Click me!" ]
            ]

    main : Program Never Model Msg
    main =
        Html.beginnerProgram
            { view = view >> toUnstyled
            , update = update
            , model = initialModel
            }

_See [`examples/readme/`](https://github.com/rtfeldman/elm-css/blob/master/examples/readme) to play around with this example._

The [`css`](http://package.elm-lang.org/packages/rtfeldman/elm-css/latest/Html-Styled-Attributes#css)
function accepts a list of [`Style`](http://package.elm-lang.org/packages/rtfeldman/elm-css/latest/Html-Styled-Attributes#css)
values which roughly correspond to [CSS properties](https://developer.mozilla.org/en-US/docs/Web/CSS/Reference).

    css
        [ display inlineBlock
        , padding (px 20)
        , border3 (px 5) solid (rgb 120 120 120)
        , hover
            [ borderColor theme.primary
            , borderRadius (px 10)
            ]
        ]

Let's take a look at some of these declarations.

    display inlineBlock

This compiles to the CSS declaration `display: inline-block;` -

[_Kebab-case_](https://en.wikipedia.org/wiki/Letter_case#Special_case_styles) CSS names become [_camelCase_](https://en.wikipedia.org/wiki/Camel_case) names in elm-css.

The [`Css.display`](#display) function only accepts values that are compatible
with the CSS `display` property, such as [`inlineBlock`](#inlineBlock), [`flex`](#flex), [`none`](#none), [`inherit`](#inherit), etc.
If you try to pass `display` an invalid value such as [`pointer`](#pointer), it will not compile!

    padding (px 20)

This compiles to the CSS declaration `padding: 20px;`

Values with units such as [`px`](#px), [`em`](#em), and [`rem`](#rem) are implemented as functions.
The [`num`](#num) function compiles to unitless numbers; for example, `flexGrow (num 1)` compiles to `flex-grow: 1;`.

[`zero`](#zero) is compatible with declarations that either do or do not expect units, so you can write
`padding zero` instead of something like `padding (px 0)`. (`padding zero` compiles to `padding: 0;`.)

    border3 (px 5) solid (rgb 120 120 120)

The [`border3`](#border3) function shows a convention in elm-css: when a CSS property supports a variable number of arguments, as is the case with `border`, elm-css commonly provides multiple functions to support those alternatives. For example, [`border`](#border), [`border2`](#border2), and [`border3`](#border3).

    hover
        [ borderColor theme.primary
        , borderRadius (px 10)
        ]

CSS pseudo-classes like `:hover` are implemented as functions that take a list of declarations.

The above compiles to something like this:

    ._c7f8ba:hover {
        border-color: #55af6a;
        border-raidus: 10px;
    }

Where does that funky classname of `_c7f8ba` come from?

elm-css automatically generates this classname based on the declarations used, and
uses it to generate a `<style>` element which applies your styles to the page.

When you write this:

    button [ css [ borderRadius (px 10), hover [ textDecoration underline ] ] ]
        [ text "Reset" ]

The `button` is not a normal `Html` value from the `elm-lang/html` package, but
a [`Html.Styled`](Html-Styled) value which wraps a normal `Html` value and adds
styling information. Later, when you call [`toUnstyled`](Html-Styled#toUnstyled)
to convert this value to a normal `Html` value, it adds two elements to the DOM:

    <button class="_df8ab1">Reset<button>

    <style>
        ._df8ab1 {
            border-radius: 10px;
        }

        ._df8ab1:hover {
            text-decoration: underline;
        }
    </style>

To sum up what's happening here:

1.  When you define values using the `css` attribute, elm-css generates a classname and some style information.
2.  That classname gets added to the element receiving the attribute, and the style information gets stored in the `Html.Styled` value which wraps that element.
3.  Calling `toUnstyled` converts this `Html.Styled` value to a normal `Html` value which represents both the requested element as well as a `<style>` element

This is how the `css` attribute is able to support things like `hover` and media
queries.

If you give an element multiple `css` attributes, they will **not** stack. For
example, in this code, only the second `css` attribute will be used. The first
one will be ignored.

    button
        [ css [ backgroundColor (hex "FF0000") ]
        , css [ borderRadius (px 10), hover [ textDecoration underline ] ]
        ]
        [ text "Reset" ]

This code compiles to the following DOM structure:

    <button class="_df8ab1">Reset<button>

    <style>
        ._df8ab1 {
            border-radius: 10px;
        }

        ._df8ab1:hover {
            text-decoration: underline;
        }
    </style>

With the exception of [`Lazy`](http://package.elm-lang.org/packages/rtfeldman/elm-css/latest/Html-Styled-Lazy),
you can expect `toUnstyled` to create one `<style>` element **total**, not one
per element that uses `css`.

`toUnstyled` avoids generating excess `<style>` elements and CSS
declarations by building up a dictionary of styles you've passed to `css`.
It will use that dictionary to add a single `<style>` to the DOM with all the
appropriate classname declarations.


### Style Reuse

To reuse styles (like [mixins](http://sass-lang.com/guide#topic-6)
in other CSS systems) use [`Style`](http://package.elm-lang.org/packages/rtfeldman/elm-css/latest/Css#Style)
values.

    greenBorder : Style
    greenBorder =
        borderColor green

    bigBold : Style
    bigBold =
        Css.batch [ fontWeight bold, fontSize (px 48) ]

    view : Model -> Html Msg
    view model =
        button [ css [ bigBold, greenBorder ] ] [ text "Ok" ]

Because only one `css` attribute per element has any effect, you cannot reuse
styles after compiling them to attributes. Trying to reuse styles by using
multiple attributes will not not work:

    greenBorder : Attribute msg
    greenBorder =
        css [ borderColor green ]

    bigBold : Attribute msg
    bigBold =
        css [ fontWeight bold, fontSize (px 48) ]

    view : Model -> Html Msg
    view model =
        -- Doesn't work!
        button [ bigBold, greenBorder ] [ text "Ok" ]

In this case, the `bigBold` attribute will be completely ignored in favor of
the `greenBorder` attribute, because it came last in the attribute list. If you
want to mix and match styles, use `Style` values!


### Unsupported Properties

The CSS spec is, ahem, not small. `elm-css` covers a lot of it, but not all of
it. Some things are considered too experimental to support, usually because they
do not have enough browser support. Others haven't been added yet because, well,
we haven't gotten around to adding them!

If you need something that `elm-css` does not support right now, the
[`Css.property`](http://package.elm-lang.org/packages/rtfeldman/elm-css/latest/Css#property)
and [`Css.Global.selector`](http://package.elm-lang.org/packages/rtfeldman/elm-css/latest/Css-Global#selector)
functions let you define custom properties and selectors, respectively.


# Style

@docs Style, batch


# Properties

@docs property, flex, flex2, flex3, medium, alignSelf, alignItems, justifyContent, order, flexDirection, flexFlow1, flexFlow2, flexWrap, flexBasis, flexGrow, flexShrink, transformStyle, transformBox, transform, transforms, currentColor, underline, overline, lineThrough, textOrientation, textDecoration, textDecoration2, textDecoration3, textDecorations, textDecorations2, textDecorations3, textDecorationLine, textDecorationLines, textDecorationStyle, textEmphasisColor, capitalize, uppercase, lowercase, fullWidth, hanging, eachLine, textIndent, textIndent2, textIndent3, clip, ellipsis, textOverflow, optimizeSpeed, optimizeLegibility, geometricPrecision, textRendering, textTransform, textAlign, textAlignLast, left, right, center, justify, justifyAll, start, end, matchParent, true, verticalAlign, display, opacity, minContent, maxContent, fitContent, fillAvailable, width, minWidth, maxWidth, height, minHeight, maxHeight, padding, padding2, padding3, padding4, paddingTop, paddingBottom, paddingRight, paddingLeft, pointerEvents, margin, margin2, margin3, margin4, marginTop, marginBottom, marginRight, marginLeft, marginBlockStart, marginBlockEnd, marginInlineStart, marginInlineEnd, boxSizing, overflow, overflowX, overflowY, overflowWrap, whiteSpace, backgroundColor, color, textShadow, textShadow2, textShadow3, textShadow4, boxShadow, boxShadow2, boxShadow3, boxShadow4, boxShadow5, boxShadow6, lineHeight, letterSpacing, fontFace, fontFamily, fontSize, fontStyle, fontWeight, fontVariant, fontVariant2, fontVariant3, fontVariantLigatures, fontVariantCaps, fontVariantNumeric, fontVariantNumeric2, fontVariantNumeric3, fontFamilies, fontVariantNumerics, fontFeatureSettings, fontFeatureSettingsList, cursor, outline, outline3, outlineColor, outlineWidth, outlineStyle, outlineOffset, zIndex, spaceAround, spaceBetween, resize, fill, touchAction, borderSpacing, borderSpacing2, visibility


# Values


## Color values

@docs Color, all, important, solid, transparent, rgb, rgba, hsl, hsla, hex


## Font values

@docs featureTag, featureTag2, featureOn, featureOff


## Other values

@docs borderCollapse, borderColor, borderColor2, borderColor3, borderColor4, borderBottomLeftRadius, borderBottomLeftRadius2, borderBottomRightRadius, borderBottomRightRadius2, borderTopLeftRadius, borderTopLeftRadius2, borderTopRightRadius, borderTopRightRadius2, borderRadius, borderRadius2, borderRadius3, borderRadius4, borderWidth, borderWidth2, borderWidth3, borderWidth4, borderBottomWidth, borderLeftWidth, borderRightWidth, borderTopWidth, borderBottomStyle, borderLeftStyle, borderRightStyle, borderTopStyle, borderStyle, borderBottomColor, borderLeftColor, borderRightColor, borderTopColor, borderBox, contentBox, border, border2, border3, borderTop, borderTop2, borderTop3, borderBottom, borderBottom2, borderBottom3, borderLeft, borderLeft2, borderLeft3, borderRight, borderRight2, borderRight3, borderImageOutset, borderImageOutset2, borderImageOutset3, borderImageOutset4, borderImageWidth, borderImageWidth2, borderImageWidth3, borderImageWidth4, scroll, visible, block, inlineBlock, inlineFlex, inline, none, auto, inherit, unset, initial, noWrap, top, static, fixed, sticky, relative, absolute, position, float, bottom, middle, baseline, sub, super, textTop, textBottom, hidden, wavy, dotted, dashed, double, groove, ridge, inset, outset, matrix, matrix3d, perspective, rotate3d, rotateX, rotateY, rotateZ, scale, scale2, scale3d, scaleX, scaleY, skew, skew2, skewX, skewY, translate, translate2, translate3d, translateX, translateY, translateZ, rotate, fillBox, viewBox, flat, preserve3d, content, wrapReverse, wrap, flexStart, flexEnd, stretch, row, rowReverse, column, columnReverse, serif, sansSerif, monospace, cursive, fantasy, xxSmall, xSmall, small, large, xLarge, xxLarge, smaller, larger, normal, italic, oblique, bold, lighter, bolder, smallCaps, allSmallCaps, petiteCaps, allPetiteCaps, unicase, titlingCaps, commonLigatures, noCommonLigatures, discretionaryLigatures, noDiscretionaryLigatures, historicalLigatures, noHistoricalLigatures, contextual, noContextual, liningNums, oldstyleNums, proportionalNums, tabularNums, diagonalFractions, stackedFractions, ordinal, slashedZero, default, pointer, crosshair, contextMenu, help, progress, wait, cell, text_, verticalText, cursorAlias, copy, move, noDrop, notAllowed, eResize, nResize, neResize, nwResize, sResize, seResize, swResize, wResize, ewResize, nsResize, neswResize, nwseResize, colResize, rowResize, allScroll, zoomIn, zoomOut, grab, grabbing, visiblePainted, visibleFill, visibleStroke, painted, stroke


# Length

@docs Length, pct, px, em, pt, ex, ch, rem, vh, vw, vmin, vmax, mm, cm, inches, pc, int, num, zero, calc, plus, minus


# Length Units

@docs Px, Em, Rem, Pct, Ex, Ch, Vh, Vw, Vmin, Vmax, Mm, Cm, In, Pt, Pc


# Angle

@docs deg, rad, grad, turn


# Duration

@docs Duration, sec, ms


# Pseudo-Classes

@docs pseudoClass, active, any, checked, disabled, empty, enabled, first, firstChild, firstOfType, fullscreen, focus, hover, visited, indeterminate, invalid, lang, lastChild, lastOfType, link, nthChild, nthLastChild, nthLastOfType, nthOfType, onlyChild, onlyOfType, optional, outOfRange, readWrite, required, root, scope, target, valid


# Pseudo-Elements

@docs pseudoElement, after, before, firstLetter, firstLine, selection


# Source

@docs src_


# Quoting

@docs qt


# Misc

@docs listStyleType, disc, circle, square, decimal, decimalLeadingZero, lowerRoman, upperRoman, lowerGreek, lowerAlpha, lowerLatin, upperAlpha, upperLatin, arabicIndic, armenian, bengali, cjkEarthlyBranch, cjkHeavenlyStem, devanagari, georgian, gujarati, gurmukhi, kannada, khmer, lao, malayalam, myanmar, oriya, telugu, thai
@docs listStylePosition, inside, outside
@docs listStyle, listStyle2, listStyle3
@docs linearGradient, linearGradient2, stop, stop2, toBottom, toBottomLeft, toBottomRight, toLeft, toRight, toTop, toTopLeft, toTopRight

@docs AlignItems, All, Angle, AngleOrDirection, BackgroundAttachment, BackgroundBlendMode, BackgroundClip, BackgroundImage, BackgroundOrigin, BackgroundRepeat, BackgroundRepeatShorthand, BasicProperty, BorderCollapse, BorderStyle, BoxSizing, Calc, CalculatedLength, CalcExpression, Cursor, Display, ExplicitLength, FeatureTagValue, FlexBasis, FlexDirection, FlexDirectionOrWrap, FlexWrap, FontFamily, FontStyle, FontStyleOrFeatureTagValue, FontVariant, FontVariantCaps, FontVariantLigatures, FontVariantNumeric, FontWeight, ImportType, IncompatibleUnits, JustifyContent, LengthOrAuto, LengthOrAutoOrCoverOrContain, LengthOrMinMaxDimension, LengthOrNone, LengthOrNoneOrMinMaxDimension, LengthOrNumber, LengthOrNumberOrAutoOrNoneOrContent, ListStyle, ListStylePosition, ListStyleType, MinMaxDimension, NonMixable, None, Number, Infinite, NumberOrInfinite, Outline, Overflow, Visibility, Position, Resize, TableLayout, TextDecorationLine, TextDecorationStyle, TextIndent, TextOrientation, TextOverflow, TextRendering, TextTransform, TouchAction, Transform, TransformBox, TransformStyle, Value, VerticalAlign, WhiteSpace, Wrap, pre, preLine, preWrap, infinite

@docs url, vertical, tableRowGroup, tableRow, tableLayout, tableHeaderGroup, tableFooterGroup, tableColumnGroup, tableCell, tableColumn, tableCaption, table, space, softLight, separate, screenBlendMode, saturation, round, repeatY, repeatX, repeat, pointerEventsFill, pointerEventsAll

@docs Compatible
@docs backgroundAttachment
@docs backgroundBlendMode
@docs backgroundClip
@docs backgroundImage
@docs backgroundOrigin
@docs backgroundPosition
@docs backgroundPosition2
@docs backgroundRepeat
@docs backgroundRepeat2
@docs backgroundSize
@docs backgroundSize2
@docs both
@docs breakWord
@docs collapse
@docs colorBurn
@docs colorDodge
@docs contain
@docs cover
@docs darken
@docs difference
@docs displayFlex
@docs exclusion
@docs hardLight
@docs horizontal
@docs hue
@docs inlineListItem
@docs inlineTable
@docs lighten
@docs listItem
@docs local
@docs luminosity
@docs manipulation
@docs multiply
@docs noRepeat
@docs overlay
@docs paddingBox
@docs panDown
@docs panLeft
@docs panRight
@docs panUp
@docs panX
@docs panY
@docs pinchZoom
@docs animationName
@docs animationDelay
@docs animationDuration
@docs animationIterationCount


# Types

@docs FontSize, ColorValue, ColorStop, IntOrAuto


# Intentionally Unsupported

These are features you might expect to be in elm-css (because they are in the
CSS specification) but which have been omitted because their use is either
deprecated or discouraged.

@docs thin, thick, blink

-}

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
