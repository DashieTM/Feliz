namespace Feliz

open Fable.Core
open Feliz.Styles

[<RequireQualifiedAccess>]
type GradientVariant =
    | Linear
    | Radial
    | Conic
    | RepeatingLinear
    | RepeatingRadial
    | RepeatingConic

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
        "to " + cornerStr.ToLowerInvariant()

[<RequireQualifiedAccess>]
type Angle =
    | Deg of float
    | Grad of float
    | Turn of float

[<RequireQualifiedAccess>]
module Angle =
    let toString (angle: Angle) : string =
        match angle with
        | Angle.Deg value -> string<float> value + "deg"
        | Angle.Grad value -> string<float> value + "rad"
        | Angle.Turn value -> string<float> value + "turn"

type GradientCornerOrAngle =
    | Corner of GradientCorner
    | Angle of Angle

[<RequireQualifiedAccess>]
module GradientCornerOrAngle =
    let toString (cornerOrAngle: GradientCornerOrAngle) : string =
        match cornerOrAngle with
        | GradientCornerOrAngle.Angle angle -> Angle.toString angle
        | GradientCornerOrAngle.Corner corner -> GradientCorner.toString corner

[<RequireQualifiedAccess>]
type RadialGradientShape =
    | Circle
    | Ellipse

type ColorHintAndColorStop =
    { ColorHint: string option
      ColorStop: string }

[<RequireQualifiedAccess>]
module ColorHintAndColorStop =
    let toString (hintAndStop: ColorHintAndColorStop) : string =
        hintAndStop.ColorHint
        |> Option.bind (fun value -> Some(value + ", "))
        |> Option.defaultValue ""
        |> (+) hintAndStop.ColorStop

type LinearGradientProps =
    { Prefixes: GradientCornerOrAngle list option
      FirstColorStop: string
      SecondColorStop: ColorHintAndColorStop
      OtherColorStops: ColorHintAndColorStop list }

[<RequireQualifiedAccess>]
module LinearGradientProps =
    let toString (props: LinearGradientProps) : string =
        let prefixes =
            props.Prefixes
            |> Option.bind (fun gradientOrCorners ->
                List.map GradientCornerOrAngle.toString gradientOrCorners
                |> String.concat ", "
                |> Some)
            |> Option.defaultValue ""

        let secondColorStop = ColorHintAndColorStop.toString props.SecondColorStop

        let otherColorStops =
            props.OtherColorStops
            |> List.map ColorHintAndColorStop.toString
            |> String.concat ", "

        prefixes + props.FirstColorStop + secondColorStop + otherColorStops

[<RequireQualifiedAccess>]
type RadialShape =
    | Circle
    | Ellipse

[<RequireQualifiedAccess>]
module LinearGradientProps =
    let toString (radialShape: RadialShape) : string =
        string<RadialShape> radialShape |> String.ToLowerInvariant()

type RadialGradientProps =
    { SizeAndShape: GradientCornerOrAngle list option
      Positions: GradientCornerOrAngle list option
      FirstColorStop: string
      SecondColorStop: ColorHintAndColorStop
      OtherColorStops: ColorHintAndColorStop list }

[<Erase>]
type LinearGradient =
    /// Pixels are (1px = 1/96th of 1in).
    ///
    /// **Note**: Pixels (px) are relative to the viewing device. For low-dpi devices, 1px is one device pixel (dot) of the display. For printers and high resolution screens 1px implies multiple device pixels.
    /// TODO beforepr comment
    static member inline linearGradient(props: LinearGradientProps) : IGradient =
        unbox ("linear-gradient(" + (LinearGradientProps.toString props) + ")")

    static member inline linearGradientSingleStep(firstStop: string, secondStop: string) : IGradient =
        unbox ("linear-gradient(" + firstStop + secondStop + ")")

    static member inline linearGradientSingleStepAndCorner
        (corner: GradientCorner, firstStop: string, secondStop: string)
        : IGradient =
        unbox (
            "linear-gradient("
            + (GradientCorner.toString corner)
            + firstStop
            + secondStop
            + ")"
        )

    static member inline linearGradientSingleStepAndAngle
        (angle: Angle, firstStop: string, secondStop: string)
        : IGradient =
        unbox ("linear-gradient(" + (Angle.toString angle) + firstStop + secondStop + ")")

    /// Creates a linear gradient from string, please keep in mind that this is not type safe and can break at runtime
    static member inline linearGradientFromString(linearGradient: string) : IGradient =
        unbox ("linear-gradient(" + linearGradient + ")")

[<Erase>]
type RadialGradient =
    static member inline radialGradient(props: RadialGradientProps) : IGradient =
        unbox ("radial-gradient(" + (radialGradientProps.toString props) + ")")
