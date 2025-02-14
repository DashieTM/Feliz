namespace Feliz.Styles

open Fable.Core

[<Erase>]
type gridColumn =
    static member inline span(value: string) : IGridSpan = unbox("span " + value)
    static member inline span(value: string, count: int) : IGridSpan = unbox("span " + value + " " + (unbox<string> count))
    static member inline span(value: int) : IGridSpan = unbox("span " + (unbox<string> value))

[<Erase>]
type gridRow =
    static member inline span(value: string) : IGridSpan = unbox("span " + value)
    static member inline span(value: string, count: int) : IGridSpan = unbox("span " + value + " " + (unbox<string> count))
    static member inline span(value: int) : IGridSpan = unbox("span " + (unbox<string> value))

[<Erase>]
type grid =
    static member inline namedLine(value: string) : IGridTemplateItem = unbox ("[" + value + "]")
    static member inline namedLines(value: string[]) : IGridTemplateItem = unbox ("[" + (String.concat " " value) + "]")
    static member inline namedLines(value: string list) : IGridTemplateItem = unbox ("[" + (String.concat " " value) + "]")
    static member inline templateWidth(value: ICssUnit) : IGridTemplateItem = unbox value
    static member inline templateWidth(value: int) : IGridTemplateItem = unbox ((unbox<string>value) + "px")
    static member inline templateWidth(value: float) : IGridTemplateItem = unbox ((unbox<string>value) + "px")

[<Erase>]
type gridLine = 
    static member inline auto : IGridLine = unbox "auto"
    static member inline line (identifier: string) : IGridLine = unbox identifier
    static member inline line (value: int, identifier: string) : IGridLine = unbox (string<int> value + " " + identifier)
    static member inline span (value: int) : IGridLine = unbox ("span " + string<int>value)
    static member inline span (identifier: string) : IGridLine = unbox ("span " + identifier)
    static member inline span (value: int, identifier: string) : IGridLine = unbox ("span " + string<int> value + " " + identifier)

[<Erase>]
type gridLineNames = 
    static member inline names (name: string) : IGridLineNames = unbox ("[" + name + "]")
    static member inline names (names: string seq) : IGridLineNames = unbox ("[" + String.concat " " names + "]")

[<Erase>]
type gridTrackRepeat = 
    static member inline repeat (count: int, size: ICssUnit) : IGridTrackRepeat = unbox (string<int> count + ", " + unbox size)
    static member inline repeat (count: int, lineNames: IGridLineNames, size: ICssUnit) : IGridTrackRepeat = unbox (string<int> count + ", " + unbox lineNames + unbox size)
    static member inline repeat (count: int, lineNamesAndSizes: (IGridLineNames option * ICssUnit) seq) : IGridTrackRepeat =
        let lineAndSizeStr = 
            lineNamesAndSizes 
            |> Seq.map (fun (lineNameOpt, size) ->
                let lineStr = 
                    lineNameOpt
                    |> Option.bind (fun lineName -> unbox lineName |> Some) 
                    |> Option.bind (fun elem -> elem + " " |> Some)
                    |> Option.defaultValue ""
                let sizeStr = unbox size
                lineStr + sizeStr
            )
            |> String.concat " "
        unbox (string<int> count + ", " + lineAndSizeStr)
    static member inline repeat (count: int, size: ICssUnit, additionalLines: IGridLineNames) : IGridTrackRepeat = unbox (string<int> count + ", " + unbox size + " " + unbox additionalLines)
    static member inline repeat (count: int, lineNames: IGridLineNames, size: ICssUnit, additionalLines: IGridLineNames) : IGridTrackRepeat = unbox (string<int> count + ", " + unbox lineNames + unbox size + " " + unbox additionalLines)
    static member inline repeat (count: int, lineNamesAndSizes: (IGridLineNames option * ICssUnit) seq, additionalLines: IGridLineNames) : IGridTrackRepeat =
        let lineAndSizeStr = 
            lineNamesAndSizes 
            |> Seq.map (fun (lineNameOpt, size) ->
                let lineStr = 
                    lineNameOpt
                    |> Option.bind (fun lineName -> unbox lineName |> Some) 
                    |> Option.bind (fun elem -> elem + " " |> Some)
                    |> Option.defaultValue ""
                let sizeStr = unbox size
                lineStr + sizeStr
            )
            |> String.concat " "
        unbox (string<int> count + ", " + lineAndSizeStr + " " + unbox additionalLines)

[<Erase>]
type gridTemplate = 
    static member inline none : IGridTemplate = unbox "none"
    static member inline trackList : IGridTemplate = unbox "none"
    static member inline autoTrackList : IGridTemplate = unbox "none"
    static member inline subGrid : IGridTemplate = unbox "none"

    
