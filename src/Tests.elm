module Css exposing
    ( Style, batch
    , Color, all, important, solid, transparent, rgb, rgba, hsl, hsla, hex
    )

{-| Define CSS styles in Elm.

@docs Style, batch

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
