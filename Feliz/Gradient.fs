namespace Feliz

open Feliz.Styles

[<RequireQualifiedAccess>]
type GradientVariant =
    | Normal
    | Repeating

[<RequireQualifiedAccess>]
type GradientType =
    | Linear
    | Radial
    | Conic

[<RequireQualifiedAccess>]
module GradientVariant =
    let toString (gradientType: GradientType) (gradientVariant: GradientVariant) : string =
        let gradientStr =
            match gradientType with
            | GradientType.Linear -> "linear-gradient"
            | GradientType.Radial -> "radial-gradient"
            | GradientType.Conic -> "conic-gradient"
        let variantStr =
            match gradientVariant with
            | GradientVariant.Normal -> ""
            | GradientVariant.Repeating -> "repeating-"

        variantStr + gradientStr

[<RequireQualifiedAccess>]
type GradientCorner =
    | Top
    | Right
    | Bottom
    | Left

[<RequireQualifiedAccess>]
module GradientCorner =
    let toString (corner: GradientCorner) : string =
        let cornerStr = string<GradientCorner> corner
        "to " + cornerStr.ToLowerInvariant ()

[<RequireQualifiedAccess>]
type GradientAngle =
    | Deg of float
    | Grad of float
    | Turn of float

[<RequireQualifiedAccess>]
module GradientAngle =
    let toString (angle: GradientAngle) : string =
        match angle with
        | GradientAngle.Deg value -> string<float> value + "deg"
        | GradientAngle.Grad value -> string<float> value + "rad"
        | GradientAngle.Turn value -> string<float> value + "turn"

[<RequireQualifiedAccess>]
type GradientCornerOrAngle =
    | Corner of GradientCorner
    | Angle of GradientAngle

[<RequireQualifiedAccess>]
module GradientCornerOrAngle =
    let degree (value: float) : GradientCornerOrAngle =
        GradientAngle.Deg value |> GradientCornerOrAngle.Angle
    let radian (value: float) : GradientCornerOrAngle =
        GradientAngle.Grad value |> GradientCornerOrAngle.Angle
    let turn (value: float) : GradientCornerOrAngle =
        GradientAngle.Turn value |> GradientCornerOrAngle.Angle

    let top = GradientCornerOrAngle.Corner GradientCorner.Top
    let right = GradientCornerOrAngle.Corner GradientCorner.Right
    let bottom = GradientCornerOrAngle.Corner GradientCorner.Bottom
    let left = GradientCornerOrAngle.Corner GradientCorner.Left

    let toString (cornerOrAngle: GradientCornerOrAngle) : string =
        match cornerOrAngle with
        | GradientCornerOrAngle.Angle angle -> GradientAngle.toString angle
        | GradientCornerOrAngle.Corner corner -> GradientCorner.toString corner

[<RequireQualifiedAccess>]
type RadialGradientShape =
    | Circle
    | Ellipse

type ColorHintAndColorStop = {
    ColorHint: ICssUnit option
    ColorStop: string
}

[<RequireQualifiedAccess>]
module ColorHintAndColorStop =
    let init (hint: ICssUnit) (stop: string) : ColorHintAndColorStop =
        let colorHintAndColorStop: ColorHintAndColorStop = {
            ColorHint = Some hint
            ColorStop = stop
        }
        colorHintAndColorStop

    let initWithoutHint (stop: string) : ColorHintAndColorStop =
        let colorHintAndColorStop: ColorHintAndColorStop = { ColorHint = None; ColorStop = stop }
        colorHintAndColorStop

    let toString (hintAndStop: ColorHintAndColorStop) : string =
        hintAndStop.ColorHint
        |> Option.bind (fun value -> Some (" " + string<ICssUnit> value))
        |> Option.defaultValue ""
        |> (+) hintAndStop.ColorStop

type LinearGradient = {
    CornerOrAngle: GradientCornerOrAngle option
    FirstColorStop: string
    OtherColorStops: ColorHintAndColorStop array
}

type ILinearColorStop = interface end

type linearColorStop =
    static member inline colorStop (color: string) : ILinearColorStop = unbox color
    static member inline colorStop (color: string, stop: ICssUnit) : ILinearColorStop = unbox (color + " " + unbox stop)
    static member inline colorStop (hint: ICssUnit, color: string, stop: ICssUnit) : ILinearColorStop =
        unbox (unbox hint + ", " + color + " " + unbox stop)
    static member inline colorStop
        (hint: ICssUnit, color: string, stop: ICssUnit, additionalStop: ICssUnit)
        : ILinearColorStop
        =
        unbox hint
        + ", "
        + color
        + " "
        + unbox stop
        + " "
        + unbox additionalStop
        |> unbox

[<RequireQualifiedAccess>]
module LinearGradient =
    let init
        (cornerOrAngle: GradientCornerOrAngle)
        (firstColor: string)
        (otherColors: ColorHintAndColorStop array)
        : LinearGradient
        =
        let gradient: LinearGradient = {
            CornerOrAngle = Some cornerOrAngle
            FirstColorStop = firstColor
            OtherColorStops = otherColors
        }
        gradient

    let simpleGradient (firstColor: string) (secondColor: string) : LinearGradient =
        let props: LinearGradient = {
            CornerOrAngle = None
            FirstColorStop = firstColor
            OtherColorStops = [|
                ColorHintAndColorStop.init None secondColor
            |]
        }
        props

    let simpleGradientWithCornerOrAngle
        (cornerOrAngle: GradientCornerOrAngle)
        (firstColor: string)
        (secondColor: string)
        : LinearGradient
        =
        let props: LinearGradient = {
            CornerOrAngle = Some cornerOrAngle
            FirstColorStop = firstColor
            OtherColorStops = [|
                ColorHintAndColorStop.init None secondColor
            |]
        }
        props

    let colorGradient (colors: string array) : LinearGradient =
        let props: LinearGradient = {
            CornerOrAngle = None
            FirstColorStop = colors[0]
            OtherColorStops =
                Array.tail colors
                |> Array.map (ColorHintAndColorStop.init None)
        }
        props

    let toString (variant: GradientVariant) (props: LinearGradient) : string =
        let prefixes =
            props.CornerOrAngle
            |> Option.bind (GradientCornerOrAngle.toString >> Some)
            |> Option.bind (fun elem -> elem + ", " |> Some)
            |> Option.defaultValue ""

        let otherColorStops =
            props.OtherColorStops
            |> Array.map ColorHintAndColorStop.toString
            |> String.concat ", "

        let gradientVariantStr = GradientVariant.toString GradientType.Linear variant

        let gradientStr =
            gradientVariantStr
            + "("
            + prefixes
            + props.FirstColorStop
            + if otherColorStops.Length > 1 then ", " else ""
            + otherColorStops
            + ")"
        gradientStr

[<RequireQualifiedAccess>]
type RadialShape =
    | Circle
    | Ellipse

[<RequireQualifiedAccess>]
module RadialShape =
    let toString (radialShape: RadialShape) : string =
        string<RadialShape> radialShape
        |> (fun str -> str.ToLowerInvariant ())

[<RequireQualifiedAccess>]
type RadialExtent =
    | ClosestCorner
    | ClosestSide
    | FarthestCorner
    | FarthestSide

[<RequireQualifiedAccess>]
module RadialExtent =
    let toString (extent: RadialExtent) : string =
        match extent with
        | RadialExtent.ClosestCorner -> "closest-corner"
        | RadialExtent.ClosestSide -> "closest-side"
        | RadialExtent.FarthestCorner -> "farthest-corner"
        | RadialExtent.FarthestSide -> "farthest-side"

[<RequireQualifiedAccess>]
type RadialSize =
    | RadialExtent of RadialExtent
    | Length of length

[<RequireQualifiedAccess>]
module RadialSize =
    let toString (size: RadialSize) : string =
        match size with
        | RadialSize.Length len -> len.ToString ()
        | RadialSize.RadialExtent extent -> RadialExtent.toString extent

[<RequireQualifiedAccess>]
type RadialSizeAndOrShape =
    | Size of RadialSize
    | Shape of RadialShape
    | Both of RadialSize * RadialShape

[<RequireQualifiedAccess>]
module RadialSizeAndOrShape =
    let toString (sizeAndOrShape: RadialSizeAndOrShape) : string =
        match sizeAndOrShape with
        | RadialSizeAndOrShape.Size size -> RadialSize.toString size
        | RadialSizeAndOrShape.Shape shape -> RadialShape.toString shape
        | RadialSizeAndOrShape.Both (size, shape) -> RadialSize.toString size + " " + RadialShape.toString shape

[<RequireQualifiedAccess>]
type GradientHorizontalPosition =
    | Left
    | Center
    | Right

[<RequireQualifiedAccess>]
module GradientHorizontalPosition =
    let toString (position: GradientHorizontalPosition) : string =
        let str = string<GradientHorizontalPosition> position
        str.ToLowerInvariant ()

[<RequireQualifiedAccess>]
type GradientVerticalPosition =
    | Top
    | Center
    | Bottom

[<RequireQualifiedAccess>]
module GradientVerticalPosition =
    let toString (position: GradientVerticalPosition) : string =
        let str = string<GradientVerticalPosition> position
        str.ToLowerInvariant ()

[<RequireQualifiedAccess>]
type CombinedPosition =
    | Top
    | Right
    | Center
    | Left
    | Bottom

[<RequireQualifiedAccess>]
module CombinedPosition =
    let toString (position: CombinedPosition) : string =
        let str = string<CombinedPosition> position
        str.ToLowerInvariant ()

[<RequireQualifiedAccess>]
type GradientPosition =
    | Both of CombinedPosition
    | Separate of Horizontal: GradientHorizontalPosition * Vertical: GradientVerticalPosition
    | Length of length

[<RequireQualifiedAccess>]
module GradientPosition =
    let top = CombinedPosition.Top |> GradientPosition.Both
    let right = CombinedPosition.Right |> GradientPosition.Both
    let center = CombinedPosition.Center |> GradientPosition.Both
    let bottom = CombinedPosition.Bottom |> GradientPosition.Both
    let left = CombinedPosition.Left |> GradientPosition.Both

    let horizontalAndVertical (horizontal, vertical) : GradientPosition =
        GradientPosition.Separate (horizontal, vertical)

    let toString (position: GradientPosition) : string =
        let str =
            match position with
            | GradientPosition.Both both -> CombinedPosition.toString both
            | GradientPosition.Length len -> len.ToString ()
            | GradientPosition.Separate (horizontal, vertical) ->
                GradientHorizontalPosition.toString horizontal
                + " "
                + GradientVerticalPosition.toString vertical
        "at " + str

type RadialGradient = {
    SizeAndOrShape: RadialSizeAndOrShape option
    Position: GradientPosition option
    FirstColorStop: string
    OtherColorStops: ColorHintAndColorStop list
}

[<RequireQualifiedAccess>]
module RadialGradient =
    let simpleGradient (firstColor: string) (secondColor: string) : RadialGradient =
        let gradient: RadialGradient = {
            SizeAndOrShape = None
            Position = None
            FirstColorStop = firstColor
            OtherColorStops = [
                ColorHintAndColorStop.init None secondColor
            ]
        }
        gradient

    let simpleGradientWithPosition
        (position: GradientPosition)
        (firstColor: string)
        (secondColor: string)
        : RadialGradient
        =
        let gradient: RadialGradient = {
            SizeAndOrShape = None
            Position = Some position
            FirstColorStop = firstColor
            OtherColorStops = [
                ColorHintAndColorStop.init None secondColor
            ]
        }
        gradient

    let colorGradient (colors: string list) : RadialGradient =
        let props: RadialGradient = {
            SizeAndOrShape = None
            Position = None
            FirstColorStop = colors.Head
            OtherColorStops =
                List.tail colors
                |> List.map (ColorHintAndColorStop.init None)
        }
        props

    let toString (variant: GradientVariant) (gradient: RadialGradient) : string =
        let sizes =
            gradient.SizeAndOrShape
            |> Option.bind (RadialSizeAndOrShape.toString >> Some)
            |> Option.bind (fun elem -> elem + " " |> Some)
            |> Option.defaultValue ""

        let positions =
            gradient.Position
            |> Option.bind (GradientPosition.toString >> Some)
            |> Option.defaultValue ""

        let otherColorStops =
            gradient.OtherColorStops
            |> List.map ColorHintAndColorStop.toString
            |> String.concat ", "

        let propToColorDelimiter =
            match gradient.SizeAndOrShape, gradient.Position with
            | Some _, Some _
            | Some _, None
            | None, Some _ -> ", "
            | None, None -> ""

        let gradientVariantStr = GradientVariant.toString GradientType.Radial variant

        let gradientStr =
            gradientVariantStr
            + "("
            + sizes
            + positions
            + propToColorDelimiter
            + gradient.FirstColorStop
            + if otherColorStops.Length > 0 then ", " else ""
            + otherColorStops
            + ")"
        gradientStr

type ConicAngleAndPosition = {
    Angle: GradientAngle option
    Position: GradientPosition option
}

[<RequireQualifiedAccess>]
module ConicAngleAndPosition =
    let init (angle: GradientAngle) (position: GradientPosition) : ConicAngleAndPosition =
        let angleAndPosition: ConicAngleAndPosition = {
            Angle = Some angle
            Position = Some position
        }
        angleAndPosition

    let initWithAngle (angle: GradientAngle) : ConicAngleAndPosition =
        let angleAndPosition: ConicAngleAndPosition = { Angle = Some angle; Position = None }
        angleAndPosition

    let initWithPosition (position: GradientPosition) : ConicAngleAndPosition =
        let angleAndPosition: ConicAngleAndPosition = {
            Angle = None
            Position = Some position
        }
        angleAndPosition

    let toString (angleAndPosition: ConicAngleAndPosition) : string =
        let angle =
            angleAndPosition.Angle
            |> Option.bind (GradientAngle.toString >> Some)
            |> Option.bind (fun elem -> "from " + elem |> Some)
            |> Option.bind (fun elem ->
                if angleAndPosition.Angle.IsSome then
                    elem + " " |> Some
                else
                    None
            )
            |> Option.defaultValue ""

        let position =
            angleAndPosition.Position
            |> Option.bind (GradientPosition.toString >> Some)
            |> Option.defaultValue ""

        angle + position

[<RequireQualifiedAccess>]
type RectangularColorSpace =
    | A98Rgb
    | DisplayP3
    | Lab
    | OkLab
    | ProphotoRgb
    | Rec2020
    | Srgb
    | SrgbLinear
    | Xyz
    | XyzD50
    | XyzD65

[<RequireQualifiedAccess>]
module RectangularColorSpace =
    let toString (colorSpace: RectangularColorSpace) : string =
        match colorSpace with
        | RectangularColorSpace.A98Rgb -> "a98-rgb"
        | RectangularColorSpace.DisplayP3 -> "display-p3"
        | RectangularColorSpace.Lab -> "lab"
        | RectangularColorSpace.OkLab -> "oklab"
        | RectangularColorSpace.ProphotoRgb -> "prophoto-rgb"
        | RectangularColorSpace.Rec2020 -> "rec2020"
        | RectangularColorSpace.Srgb -> "srgb"
        | RectangularColorSpace.SrgbLinear -> "srgb-linear"
        | RectangularColorSpace.Xyz -> "xyz"
        | RectangularColorSpace.XyzD50 -> "xyz-d50"
        | RectangularColorSpace.XyzD65 -> "xyz-d65"

[<RequireQualifiedAccess>]
type PolarColorSpaceVariant =
    | Hsl
    | Hwb
    | Lch
    | OkLch

[<RequireQualifiedAccess>]
module PolarColorSpaceVariant =
    let toString (polarColorSpace: PolarColorSpaceVariant) : string =
        let str = string<PolarColorSpaceVariant> polarColorSpace
        str.ToLowerInvariant ()

[<RequireQualifiedAccess>]
type HueInterpolationMethod =
    | Shorter
    | Longer
    | Increasing
    | Decreasing

[<RequireQualifiedAccess>]
module HueInterpolationMethod =
    let toString (interpolationMethod: HueInterpolationMethod) : string =
        let str = string<HueInterpolationMethod> interpolationMethod
        str.ToLowerInvariant ()

type PolarColorSpace = {
    HueInterpolation: HueInterpolationMethod option
    Variant: PolarColorSpaceVariant
}

[<RequireQualifiedAccess>]
module PolarColorSpace =
    let withDefaultInterpolation (variant: PolarColorSpaceVariant) : PolarColorSpace =
        let polar: PolarColorSpace = {
            HueInterpolation = None
            Variant = variant
        }
        polar

    let toString (colorSpace: PolarColorSpace) : string =
        let hueMethod =
            colorSpace.HueInterpolation
            // Shorter is the default
            // https://developer.mozilla.org/en-US/docs/Web/CSS/hue-interpolation-method
            |> Option.defaultValue HueInterpolationMethod.Shorter
            |> HueInterpolationMethod.toString
        let polar = PolarColorSpaceVariant.toString colorSpace.Variant
        polar + hueMethod + " hue"

[<RequireQualifiedAccess>]
type ColorInterpolationMethod =
    | RectangularColorSpace of RectangularColorSpace
    | PolarColorSpace of PolarColorSpace

[<RequireQualifiedAccess>]
module ColorInterpolationMethod =
    let a98Rgb =
        ColorInterpolationMethod.RectangularColorSpace RectangularColorSpace.A98Rgb
    let displayP3 =
        ColorInterpolationMethod.RectangularColorSpace RectangularColorSpace.DisplayP3
    let lab = ColorInterpolationMethod.RectangularColorSpace RectangularColorSpace.Lab
    let okLab =
        ColorInterpolationMethod.RectangularColorSpace RectangularColorSpace.OkLab
    let prophotoRgb =
        ColorInterpolationMethod.RectangularColorSpace RectangularColorSpace.ProphotoRgb
    let rec2020 =
        ColorInterpolationMethod.RectangularColorSpace RectangularColorSpace.Rec2020
    let sRgb = ColorInterpolationMethod.RectangularColorSpace RectangularColorSpace.Srgb
    let sRgbLinear =
        ColorInterpolationMethod.RectangularColorSpace RectangularColorSpace.SrgbLinear
    let xyz = ColorInterpolationMethod.RectangularColorSpace RectangularColorSpace.Xyz
    let xyzD50 =
        ColorInterpolationMethod.RectangularColorSpace RectangularColorSpace.XyzD50
    let xyzD65 =
        ColorInterpolationMethod.RectangularColorSpace RectangularColorSpace.XyzD65

    let hsl =
        PolarColorSpace.withDefaultInterpolation PolarColorSpaceVariant.Hsl
        |> ColorInterpolationMethod.PolarColorSpace
    let hwb =
        PolarColorSpace.withDefaultInterpolation PolarColorSpaceVariant.Hwb
        |> ColorInterpolationMethod.PolarColorSpace
    let lch =
        PolarColorSpace.withDefaultInterpolation PolarColorSpaceVariant.Lch
        |> ColorInterpolationMethod.PolarColorSpace
    let okLch =
        PolarColorSpace.withDefaultInterpolation PolarColorSpaceVariant.OkLch
        |> ColorInterpolationMethod.PolarColorSpace

    let polarHue
        (polarVariant: PolarColorSpaceVariant)
        (hueInterpolationOpt: HueInterpolationMethod option)
        : ColorInterpolationMethod
        =
        let polar: PolarColorSpace = {
            Variant = polarVariant
            HueInterpolation = hueInterpolationOpt
        }
        ColorInterpolationMethod.PolarColorSpace polar

    let toString (interpolationMethod: ColorInterpolationMethod) : string =
        let prefix = "in "
        let colorSpace =
            match interpolationMethod with
            | ColorInterpolationMethod.PolarColorSpace colorSpace -> PolarColorSpace.toString colorSpace
            | ColorInterpolationMethod.RectangularColorSpace colorSpace -> RectangularColorSpace.toString colorSpace
        prefix + colorSpace

[<RequireQualifiedAccess>]
type ConicGradientPropVariant =
    | AnglesAndPositions of ConicAngleAndPosition
    | ColorInterpolation of ColorInterpolationMethod

[<RequireQualifiedAccess>]
module ConicGradientPropVariant =
    let toString (variant: ConicGradientPropVariant) : string =
        match variant with
        | ConicGradientPropVariant.AnglesAndPositions anglesAndPositions ->
            ConicAngleAndPosition.toString anglesAndPositions
        | ConicGradientPropVariant.ColorInterpolation colorInterpolation ->
            ColorInterpolationMethod.toString colorInterpolation

[<RequireQualifiedAccess>]
type ConicColorHintVariant =
    | Percentage of float
    | Angle of GradientAngle

[<RequireQualifiedAccess>]
module ConicColorHintVariant =
    let percentage (value: float) : ConicColorHintVariant = ConicColorHintVariant.Percentage value

    let degree (value: float) : ConicColorHintVariant =
        GradientAngle.Deg value |> ConicColorHintVariant.Angle

    let radian (value: float) : ConicColorHintVariant =
        GradientAngle.Grad value |> ConicColorHintVariant.Angle

    let turn (value: float) : ConicColorHintVariant =
        GradientAngle.Turn value |> ConicColorHintVariant.Angle

    let toString (variant: ConicColorHintVariant) : string =
        match variant with
        | ConicColorHintVariant.Angle angle -> GradientAngle.toString angle
        | ConicColorHintVariant.Percentage percentage ->
            let percentageStr = string<float> percentage
            percentageStr + "%"

type ConicColorHintAndColorStop = {
    ColorHint: ConicColorHintVariant option
    ColorStop: string
}

[<RequireQualifiedAccess>]
module ConicColorHintAndColorStop =
    let init (hintOpt: ConicColorHintVariant option) (stop: string) : ConicColorHintAndColorStop =
        let colorHintAndColorStop: ConicColorHintAndColorStop = {
            ColorHint = hintOpt
            ColorStop = stop
        }
        colorHintAndColorStop

    let toString (hintAndStop: ConicColorHintAndColorStop) : string =
        hintAndStop.ColorHint
        |> Option.bind (fun value -> Some (" " + ConicColorHintVariant.toString value))
        |> Option.defaultValue ""
        |> (+) hintAndStop.ColorStop

type ConicGradient = {
    AngleAndPosition: ConicAngleAndPosition option
    ColorInterpolation: ColorInterpolationMethod option
    FirstColorStop: string
    OtherColorStops: ConicColorHintAndColorStop list
}

[<RequireQualifiedAccess>]
module ConicGradient =
    let simpleGradient (firstColor: string) (secondColor: string) : ConicGradient =
        let gradient: ConicGradient = {
            AngleAndPosition = None
            ColorInterpolation = None
            FirstColorStop = firstColor
            OtherColorStops = [
                ConicColorHintAndColorStop.init None secondColor
            ]
        }
        gradient

    let simpleGradientWithPosition
        (position: GradientPosition)
        (firstColor: string)
        (secondColor: string)
        : ConicGradient
        =
        let angleAndPosition: ConicAngleAndPosition = {
            Angle = None
            Position = Some position
        }
        let gradient: ConicGradient = {
            AngleAndPosition = Some angleAndPosition
            ColorInterpolation = None
            FirstColorStop = firstColor
            OtherColorStops = [
                ConicColorHintAndColorStop.init None secondColor
            ]
        }
        gradient

    let simpleGradientWithAngle (angle: GradientAngle) (firstColor: string) (secondColor: string) : ConicGradient =
        let angleAndPosition: ConicAngleAndPosition = { Angle = Some angle; Position = None }
        let gradient: ConicGradient = {
            AngleAndPosition = Some angleAndPosition
            ColorInterpolation = None
            FirstColorStop = firstColor
            OtherColorStops = [
                ConicColorHintAndColorStop.init None secondColor
            ]
        }
        gradient

    let colorGradient (colors: string list) : ConicGradient =
        let props: ConicGradient = {
            AngleAndPosition = None
            ColorInterpolation = None
            FirstColorStop = colors.Head
            OtherColorStops =
                List.tail colors
                |> List.map (ConicColorHintAndColorStop.init None)
        }
        props

    let toString (variant: GradientVariant) (gradient: ConicGradient) : string =
        let angleAndPosition =
            gradient.AngleAndPosition
            |> Option.bind (ConicAngleAndPosition.toString >> Some)
            |> Option.bind (fun elem ->
                if gradient.ColorInterpolation.IsSome then
                    elem + " " |> Some
                else
                    None
            )
            |> Option.defaultValue ""
        let colorInterpolation =
            gradient.ColorInterpolation
            |> Option.bind (ColorInterpolationMethod.toString >> Some)
            |> Option.defaultValue ""
        let otherColors =
            gradient.OtherColorStops
            |> List.map ConicColorHintAndColorStop.toString
            |> String.concat ", "

        let propToColorDelimiter =
            match gradient.AngleAndPosition, gradient.ColorInterpolation with
            | Some _, Some _
            | Some _, None
            | None, Some _ -> ", "
            | None, None -> ""

        let gradientVariantStr = GradientVariant.toString GradientType.Conic variant

        let gradientStr =
            gradientVariantStr
            + "("
            + angleAndPosition
            + colorInterpolation
            + propToColorDelimiter
            + gradient.FirstColorStop
            + if otherColors.Length > 0 then ", " else ""
            + otherColors
            + ")"
        gradientStr
