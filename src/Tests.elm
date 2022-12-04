module Css exposing
    ( Style, batch
    , Color, all, important, solid, transparent, rgb, rgba, hsl, hsla, hex
    )

{-| Define CSS styles in Elm.

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


x =
    c "" []
