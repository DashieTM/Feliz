namespace Feliz

open Feliz.Styles

type IGradientAngle = interface end

type gradientAngle =
    static member inline deg (value: float) : IGradientAngle = unbox (string<float> value + "deg")
    static member inline grad (value: float) : IGradientAngle = unbox (string<float> value + "grad")
    static member inline rad (value: float) : IGradientAngle = unbox (string<float> value + "rad")
    static member inline turn (value: float) : IGradientAngle = unbox (string<float> value + "turn")

// Linear Types
type IGradientCorner = interface end

type gradientCorner =
    static member inline top: IGradientCorner = unbox "top"
    static member inline right: IGradientCorner = unbox "right"
    static member inline bottom: IGradientCorner = unbox "bottom"
    static member inline left: IGradientCorner = unbox "left"

type ILinearColorStop = interface end

type linearColorStop =
    static member inline colorStop (color: string) : ILinearColorStop = unbox color
    static member inline colorStop (color: string, stop: ICssUnit) : ILinearColorStop = unbox (color + " " + unbox stop)
    static member inline colorStop (color: string, stop: ICssUnit, additionalStop: ICssUnit) : ILinearColorStop =
        color + " " + unbox stop + " " + unbox additionalStop
        |> unbox

type ILinearColorStopAndHint = interface end

type linearColorStopAndHint =
    static member inline colorStop (color: string) : ILinearColorStopAndHint = unbox color
    static member inline colorStop (hint: ICssUnit, color: string) : ILinearColorStopAndHint =
        unbox (unbox hint + ", " + unbox color)
    static member inline colorStop (color: string, stop: ICssUnit) : ILinearColorStopAndHint =
        unbox (color + " " + unbox stop)
    static member inline colorStop (hint: ICssUnit, color: string, stop: ICssUnit) : ILinearColorStopAndHint =
        unbox (unbox hint + ", " + color + " " + unbox stop)
    static member inline colorStop (color: string, stop: ICssUnit, additionalStop: ICssUnit) : ILinearColorStopAndHint =
        unbox (color + " " + unbox stop + " " + unbox additionalStop)
    static member inline colorStop
        (hint: ICssUnit, color: string, stop: ICssUnit, additionalStop: ICssUnit)
        : ILinearColorStopAndHint
        =
        unbox hint
        + ", "
        + color
        + " "
        + unbox stop
        + " "
        + unbox additionalStop
        |> unbox

type ILinearGradientOptions = interface end

type linearGradientOptions =
    static member inline angle (angle: IGradientAngle) : ILinearGradientOptions = unbox angle
    static member inline corner (corner: IGradientCorner) : ILinearGradientOptions = unbox corner

// Radial Types
type IRadialShape = interface end

type radialShape =
    static member inline circle: IRadialShape = unbox "circle"
    static member inline ellipse: IRadialShape = unbox "ellipse"

type IRadialSize = interface end

type radialSize =
    static member inline closestCorner: IRadialSize = unbox "closest-corner"
    static member inline closestSide: IRadialSize = unbox "closest-side"
    static member inline farthestCorner: IRadialSize = unbox "farthest-corner"
    static member inline farthestSide: IRadialSize = unbox "farthest-side"
    static member inline length (length: ICssUnit) : IRadialSize = unbox length

// Conic Types
type IConicColorHintVariant = interface end

type conicColorHintVariant =
    static member inline angle (angle: IGradientAngle) : IConicColorHintVariant = unbox angle
    static member inline percentage (value: float) : IConicColorHintVariant = unbox (string<float> value + "%")

type IAngleColorStop = interface end

type angleColorStop =
    static member inline colorStop (color: string) : IAngleColorStop = unbox color
    static member inline colorStop (color: string, stop: ICssUnit) : IAngleColorStop = unbox (color + unbox stop)
    static member inline colorStop (color: string, stop: ICssUnit, additionalStop: ICssUnit) : IAngleColorStop =
        color + unbox stop + unbox additionalStop |> unbox

type IAngleColorHintAndStop = interface end

type angleColorHintAndStop =
    static member inline colorStop (color: string) : IAngleColorHintAndStop = unbox color
    static member inline colorStop (color: string, stop: ICssUnit) : IAngleColorHintAndStop = unbox (color + unbox stop)
    static member inline colorStop (color: string, stop: ICssUnit, additionalStop: ICssUnit) : IAngleColorHintAndStop =
        color + unbox stop + unbox additionalStop |> unbox
    static member inline colorStop (hint: IConicColorHintVariant, color: string) : IAngleColorHintAndStop =
        unbox hint + ", " + color |> unbox
    static member inline colorStop
        (hint: IConicColorHintVariant, color: string, stop: ICssUnit)
        : IAngleColorHintAndStop
        =
        unbox hint + ", " + color + " " + unbox stop |> unbox
    static member inline colorStop
        (hint: IConicColorHintVariant, color: string, stop: ICssUnit, additionalStop: ICssUnit)
        : IAngleColorHintAndStop
        =
        unbox hint
        + ", "
        + color
        + " "
        + unbox stop
        + " "
        + unbox additionalStop
        |> unbox

type IGradientPosition = interface end

type gradientPosition =
    static member inline top: IGradientPosition = unbox "at top"
    static member inline right: IGradientPosition = unbox "at right"
    static member inline center: IGradientPosition = unbox "at center"
    static member inline bottom: IGradientPosition = unbox "at bottom"
    static member inline left: IGradientPosition = unbox "at left"
    static member inline rightTop: IGradientPosition = unbox "at right top"
    static member inline rightCenter: IGradientPosition = unbox "at right center"
    static member inline rightBottom: IGradientPosition = unbox "at right bottom"
    static member inline centerTop: IGradientPosition = unbox "at center top"
    static member inline centerBottom: IGradientPosition = unbox "at center bottom"
    static member inline leftTop: IGradientPosition = unbox "at left top"
    static member inline leftCenter: IGradientPosition = unbox "at left center"
    static member inline leftBottom: IGradientPosition = unbox "at left bottom"

type IAngleAndPosition = interface end

type angleAndPosition =
    static member inline angle (angle: IGradientAngle) : IAngleAndPosition = unbox ("from " + unbox angle)
    static member inline position (position: IGradientPosition) : IAngleAndPosition = unbox position

type IRadialSizeAndOrShape = interface end

type radialSizeAndOrShape =
    static member inline shape (shape: IRadialShape) : IRadialSizeAndOrShape = unbox shape
    static member inline size (size: IRadialSize) : IRadialSizeAndOrShape = unbox size
    static member inline sizeAndShape (size: IRadialSize, shape: IRadialShape) : IRadialSizeAndOrShape =
        unbox size + " " + unbox shape |> unbox

type IRectangularColorSpace = interface end

type rectangularColorSpace =
    static member inline a98Rgb: IRectangularColorSpace = unbox "a98-rgb"
    static member inline displayP3: IRectangularColorSpace = unbox "display-p3"
    static member inline lab: IRectangularColorSpace = unbox "lab"
    static member inline okLab: IRectangularColorSpace = unbox "oklab"
    static member inline prophotoRgb: IRectangularColorSpace = unbox "prophoto-rgb"
    static member inline rec2020: IRectangularColorSpace = unbox "rec2020"
    static member inline srgb: IRectangularColorSpace = unbox "srgb"
    static member inline srgbLinear: IRectangularColorSpace = unbox "srgb-linear"
    static member inline xyz: IRectangularColorSpace = unbox "xyz"
    static member inline xyzD50: IRectangularColorSpace = unbox "xyz-d50"
    static member inline xyzD65: IRectangularColorSpace = unbox "xyz-d65"

type IPolarColorSpaceVariant = interface end

type polarColorSpaceVariant =
    static member inline hsl: IPolarColorSpaceVariant = unbox "hsl"
    static member inline hwb: IPolarColorSpaceVariant = unbox "hwb"
    static member inline lch: IPolarColorSpaceVariant = unbox "lch"
    static member inline okLch: IPolarColorSpaceVariant = unbox "oklch"

type IHueInterpolationMethod = interface end

type hueInterpolationMethod =
    static member inline shorter: IHueInterpolationMethod = unbox "shorter"
    static member inline longer: IHueInterpolationMethod = unbox "longer"
    static member inline increasing: IHueInterpolationMethod = unbox "increasing"
    static member inline decreasing: IHueInterpolationMethod = unbox "decreasing"

type IColorInterpolation = interface end

type colorInterpolation =
    static member inline polar (variant: IPolarColorSpaceVariant) : IColorInterpolation =
        "in " + unbox variant + " shorter hue" |> unbox
    static member inline polarWithInterpolation
        (variant: IPolarColorSpaceVariant, interpolation: IHueInterpolationMethod)
        : IColorInterpolation
        =
        "in " + unbox interpolation + unbox variant + " hue"
        |> unbox
    static member inline rectangular (colorSpace: IRectangularColorSpace) : IColorInterpolation = unbox colorSpace
