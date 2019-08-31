namespace Feliz

open Browser.Types
open Fable.Core.JsInterop
open Fable.Core
open Feliz.Styles

[<StringEnum; RequireQualifiedAccess>]
type AriaDropEffect =
    /// A duplicate of the source object will be dropped into the target.
    | Copy
    /// A function supported by the drop target is executed, using the drag source as an input.
    | Execute
    /// A reference or shortcut to the dragged object will be created in the target object.
    | Link
    /// The source object will be removed from its current location and dropped into the target.
    | Move
    /// No operation can be performed; effectively cancels the drag operation if an attempt is made
    /// to drop on this object. Ignored if combined with any other token value. e.g. 'none copy'
    /// is equivalent to a 'copy' value.
    | None
    /// There is a popup menu or dialog that allows the user to choose one of the drag operations
    /// (copy, move, link, execute) and any other drag functionality, such as cancel.
    | Popup

[<StringEnum; RequireQualifiedAccess>]
type AriaRelevant =
    /// Element nodes are added to the DOM within the live region.
    | Additions
    /// Equivalent to the combination of all values, "additions removals text".
    | All
    /// Text or element nodes within the live region are removed from the DOM.
    | Removals
    /// Text is added to any DOM descendant nodes of the live region.
    | Text

type prop =
    static member inline id(value: string) = Interop.mkAttr "id" value
    static member inline ref(handler: Element -> unit) = Interop.mkAttr "ref" handler
    static member inline ref(ref: Fable.React.IRefValue<HTMLElement option>) = Interop.mkAttr "ref" ref
    /// Sets the inner Html content of the element.
    static member inline dangerouslySetInnerHTML(content: string) = Interop.mkAttr "dangerouslySetInnerHTML" (createObj [ "__html" ==> content ])
    /// Alias for `dangerouslySetInnerHTML`, sets the inner Html content of the element.
    static member inline innerHtml (content: string) = Interop.mkAttr "dangerouslySetInnerHTML" (createObj [ "__html" ==> content ])
    /// `prop.ref` callback that sets the value of an input after DOM element is created.
    /// Can be used instead of `prop.defaultValue` and `prop.value` props to override input box value.
    static member inline valueOrDefault(value: string) =
        prop.ref (fun e -> if e |> isNull |> not && !!e?value <> !!value then e?value <- !!value)
    /// `prop.ref` callback that sets the value of an input after DOM element is created.
    /// Can be used instead of `prop.defaultValue` and `prop.value` props to override input value.
    static member inline valueOrDefault(value: int) =
        prop.ref (fun e -> if e |> isNull |> not && !!e?value <> !!value then e?value <- !!value)
    /// `prop.ref` callback that sets the value of an input after DOM element is created.
    /// Can be used instead of `prop.defaultValue` and `prop.value` props to override input value.
    static member inline valueOrDefault(value: bool) =
        prop.ref (fun e -> if e |> isNull |> not && !!e?value <> !!value then e?value <- !!value)
    static member inline id(value: int) = Interop.mkAttr "id" (string value)
    /// Specifies a CSS class for this element.
    static member inline className(value: string) = Interop.mkAttr "className" value
    /// Takes a list of conditional classes (`predicate:bool` * `className:string`), filters out the ones where the `predicate` is false and joins the rest of them using a space to combine the classses into a single class property.
    ///
    ///`prop.className [ true, "one";  false, "two" ]`
    ///
    /// is the same as
    ///
    ///`prop.className "one"`
    ///
    static member inline className (classes: #seq<bool * string>) =
        classes
        |> Seq.filter fst
        |> Seq.map snd
        |> String.concat " "
        |> Interop.mkAttr "className"

    /// Takes a `seq<string>` and joins them using a space to combine the classses into a single class property.
    ///
    /// `prop.classes [ "one"; "two" ]` => `prop.className "one two"`
    static member inline classes(names: seq<string>) = Interop.mkAttr "className" (String.concat " " names)
    /// Takes a `seq<string>` and joins them using a space to combine the classses into a single class property.
    ///
    /// `prop.className [ "one"; "two" ]`
    ///
    /// is the same as
    ///
    /// `prop.className "one two"`
    static member inline className(names: seq<string>) = Interop.mkAttr "className" (String.concat " " names)
    /// Defines the text content of the element. Alias for `children [ Html.text value ]`
    static member inline text (value: string) = Interop.mkAttr "children" value
    /// Defines the text content of the element. Alias for `children [ Html.text value ]`
    static member inline text (value: int) = Interop.mkAttr "children" value
    /// Defines the text content of the element. Alias for `children [ Html.text value ]`
    static member inline text (value: float) = Interop.mkAttr "children" value
    static member inline key(value: string) = Interop.mkAttr "key" value
    static member inline key(value: int) = Interop.mkAttr "key" value
    static member inline key(value: System.Guid) = Interop.mkAttr "value" (string value)
    static member inline defaultChecked(value: bool) = Interop.mkAttr "defaultChecked" value
    static member inline defaultValue(value: string) = Interop.mkAttr "defaultValue" value
    static member inline defaultValue(value: int) = Interop.mkAttr "defaultValue" value
    static member inline defaultValue(value: bool) = Interop.mkAttr "defaultValue" value
    static member inline defaultValue(value: float) = Interop.mkAttr "defaultValue" value
    static member inline value(value: string) = Interop.mkAttr "value" value
    static member inline value(value: int) = Interop.mkAttr "value" value
    static member inline value(value: float) = Interop.mkAttr "value" value
    static member inline value(value: bool) = Interop.mkAttr "value" value
    static member inline value(value: System.Guid) = Interop.mkAttr "value" (string value)
    static member inline selected(value: bool) = Interop.mkAttr "selected" value
    static member inline x(value: int) = Interop.mkAttr "x" value
    static member inline x(value: ICssUnit) = Interop.mkAttr "x" value
    static member inline y(value: int) = Interop.mkAttr "y" value
    static member inline y(value: ICssUnit) = Interop.mkAttr "y" value
    static member inline r(value: int) = Interop.mkAttr "r" value
    static member inline r(value: ICssUnit) = Interop.mkAttr "r" value
    static member inline viewPort(x: int, y: int, height: int, width: int) =
        (unbox<string> x) + " " +
        (unbox<string> y) + " " +
        (unbox<string> height) + " " +
        (unbox<string> width)
    static member inline fill(color: string) = Interop.mkAttr "fill" color
    static member inline x1(value: int) = Interop.mkAttr "x1" value
    static member inline x1(value: ICssUnit) = Interop.mkAttr "x1" value
    static member inline x1(value: float) = Interop.mkAttr "y1" value
    static member inline x2(value: int) = Interop.mkAttr "x2" value
    static member inline x2(value: ICssUnit) = Interop.mkAttr "x1" value
    static member inline x2(value: float) = Interop.mkAttr "x1" value
    static member inline y1(value: int) = Interop.mkAttr "y1" value
    static member inline y1(value: float) = Interop.mkAttr "y1" value
    static member inline y1(value: ICssUnit) = Interop.mkAttr "y1" value
    static member inline y2(value: int) = Interop.mkAttr "y2" value
    static member inline y2(value: ICssUnit) = Interop.mkAttr "y2" value
    static member inline y2(value: float) = Interop.mkAttr "y2" value
    static member inline cx(value: int) = Interop.mkAttr "cx" value
    static member inline cx(value: ICssUnit) = Interop.mkAttr "cx" value
    static member inline cx(value: float) = Interop.mkAttr "cx" value
    static member inline rx(value: int) = Interop.mkAttr "rx" value
    static member inline rx(value: ICssUnit) = Interop.mkAttr "rx" value
    static member inline rx(value: float) = Interop.mkAttr "rx" value
    static member inline ry(value: int) = Interop.mkAttr "ry" value
    static member inline ry(value: ICssUnit) = Interop.mkAttr "ry" value
    static member inline ry(value: float) = Interop.mkAttr "ry" value
    static member inline cy(value: float) = Interop.mkAttr "cy" value
    static member inline cy(value: int) = Interop.mkAttr "cy" value
    static member inline cy(value: ICssUnit) = Interop.mkAttr "cy" value
    static member inline fillOpacity(value: float) = Interop.mkAttr "fillOpacity" value
    static member inline stroke(color: string) = Interop.mkAttr "stroke" color
    static member inline strokeWidth(value: int) = Interop.mkAttr "strokeWidth" value
    static member inline strokeWidth(value: float) = Interop.mkAttr "strokeWidth" value
    static member inline strokeWidth(value: ICssUnit) = Interop.mkAttr "strokeWidth" value
    static member inline offset(value: int) = Interop.mkAttr "offset" value
    static member inline offset(value: float) = Interop.mkAttr "offset" value
    static member inline offset(value: ICssUnit) = Interop.mkAttr "offset" value
    static member inline points(value: string) = Interop.mkAttr "points" value
    static member inline stopColor(value: string) = Interop.mkAttr "stopColor" value
    static member inline stopOpacity(value: float) = Interop.mkAttr "stopOpacity" value
    static member inline accept(value: string) = Interop.mkAttr "accept" value
    static member inline acceptCharset(value: string) = Interop.mkAttr "acceptCharset" value
    static member inline accessKey(value: string) = Interop.mkAttr "accessKey" value
    static member inline action(value: string) = Interop.mkAttr "action" value
    static member inline alt(value: string) = Interop.mkAttr "alt" value
    static member inline async(value: bool) = Interop.mkAttr "async" value
    static member inline autoComplete(value: string) = Interop.mkAttr "autoComplete" value
    static member inline autoFocus(value: bool) = Interop.mkAttr "autoFocus" value
    static member inline autoPlay(value: bool) = Interop.mkAttr "autoPlay" value
    static member inline capture(value: bool) = Interop.mkAttr "capture" value
    static member inline isChecked(value: bool) = Interop.mkAttr "checked" value
    static member inline cols(value: int) = Interop.mkAttr "cols" value
    static member inline colSpan(value: int) = Interop.mkAttr "colSpan" value
    static member inline contentEditable(value: bool) = Interop.mkAttr "contenteditable" value
    static member inline disabled(value: bool) = Interop.mkAttr "disabled" value
    static member inline height(value: int) = Interop.mkAttr "height" value
    static member inline width(value: int) = Interop.mkAttr "width" value
    static member inline href (value: string) = Interop.mkAttr "href" value
    static member inline hidden (value: bool) = Interop.mkAttr "hidden" value
    static member inline htmlFor(value: string) = Interop.mkAttr "htmlFor" value
    static member inline min(value: int) = Interop.mkAttr "min" value
    static member inline max(value: int) = Interop.mkAttr "max" value
    static member inline maxLength(value: int) = Interop.mkAttr "maxlength" value
    static member inline multiple(value: bool) = Interop.mkAttr "multiple" value
    static member inline method(value: string) = Interop.mkAttr "method" value
    static member inline muted(value: bool) = Interop.mkAttr "muted" value
    static member inline name(value: string) = Interop.mkAttr "name" value
    static member inline placeholder(value: string) = Interop.mkAttr "placeholder" value
    static member inline isOpen(value: bool) = Interop.mkAttr "open" value
    static member inline sizes (value: string) = Interop.mkAttr "sizes" value
    static member inline srcset (value: string) = Interop.mkAttr "srcset" value
    static member inline required(value: bool) = Interop.mkAttr "required" value
    static member inline content(value: string) = Interop.mkAttr "content" value
    static member inline children(value: Fable.React.ReactElement) = Interop.mkAttr "children" value
    static member inline rows(value: int) = Interop.mkAttr "rows" value
    static member inline rowSpan(value: int) = Interop.mkAttr "rowSpan" value
    static member inline inputType(value: string) = Interop.mkAttr "type" value
    static member inline src(value: string) = Interop.mkAttr "src" value
    static member inline start(value: string) = Interop.mkAttr "start" value
    static member inline readOnly (value: bool) = Interop.mkAttr "readOnly" value
    static member inline custom(key: string, value: 't) = Interop.mkAttr key value
    static member inline children (elems: Fable.React.ReactElement seq) = Interop.mkAttr "children" (Interop.reactApi.Children.toArray elems)
    static member inline onCut (handler: ClipboardEvent -> unit) = Interop.mkAttr "onCut" handler
    static member inline onPaste (handler: ClipboardEvent -> unit) = Interop.mkAttr "onPaste" handler
    static member inline onCompositionEnd (handler: CompositionEvent -> unit) = Interop.mkAttr "onCompositionEnd" handler
    static member inline onCompositionStart (handler: CompositionEvent -> unit) = Interop.mkAttr "onCompositionStart" handler
    static member inline onCopy (handler: ClipboardEvent -> unit) = Interop.mkAttr "onCopy" handler
    static member inline onCompositionUpdate (handler: CompositionEvent -> unit) = Interop.mkAttr "onCompositionUpdate" handler
    static member inline onFocus (handler: FocusEvent -> unit) = Interop.mkAttr "onFocus" handler
    static member inline onBlur (handler: FocusEvent -> unit) = Interop.mkAttr "onBlur" handler
    static member inline onChange (handler: Event -> unit) = Interop.mkAttr "onChange" handler
    /// Same as `onChange` but let's you deal with the text changed from the `input` element directly
    /// instead of extracting it from the event arguments.
    static member inline onTextChange (handler: string -> unit) = Interop.mkAttr "onChange" (fun (ev: Event) -> handler (!!ev.target?value))
    /// Same as `onChange` that takes an event as input but instead let's you deal with the text changed from the `input` element directly
    /// instead of extracting it from the event arguments.
    static member inline onChange (handler: string -> unit) = Interop.mkAttr "onChange" (fun (ev: Event) -> handler (!!ev.target?value))
    /// Same as `onChange` that takes an event as input but instead let's you deal with the `checked` value changed from the `input` element directly when it is defined as a checkbox with `prop.inputType.checkbox`.
    static member inline onChange (handler: bool -> unit) = Interop.mkAttr "onChange" (fun (ev: Event) -> handler (!!ev.target?``checked``))
    /// Same as `onChange` but let's you deal with the `checked` value that has changed from the `input` element directly instead of extracting it from the event arguments.
    static member inline onCheckedChange (handler: bool -> unit) = Interop.mkAttr "onChange" (fun (ev: Event) -> handler (!!ev.target?``checked``))
    static member inline onInput (handler: Event -> unit) = Interop.mkAttr "onInput" handler
    static member inline onSubmit (handler: Event -> unit) = Interop.mkAttr "onSubmit" handler
    static member inline onReset (handler: Event -> unit) = Interop.mkAttr "onReset" handler
    static member inline onLoad (handler: Event -> unit) = Interop.mkAttr "onLoad" handler
    static member inline onError (handler: Event -> unit) = Interop.mkAttr "onError" handler
    static member inline onKeyDown (handler: KeyboardEvent -> unit) = Interop.mkAttr "onKeyDown" handler
    static member inline onKeyPress (handler: KeyboardEvent -> unit) = Interop.mkAttr "onKeyPress" handler
    static member inline onKeyUp (handler: KeyboardEvent -> unit) = Interop.mkAttr "onKeyUp" handler
    static member inline onAbort (handler: Event -> unit) = Interop.mkAttr "onAbort" handler
    static member inline onCanPlay (handler: Event -> unit) = Interop.mkAttr "onCanPlay" handler
    static member inline onCanPlayThrough (handler: Event -> unit) = Interop.mkAttr "onCanPlayThrough" handler
    static member inline onDurationChange (handler: Event -> unit) = Interop.mkAttr "onDurationChange" handler
    static member inline onEmptied (handler: Event -> unit) = Interop.mkAttr "onEmptied" handler
    static member inline onEncrypted (handler: Event -> unit) = Interop.mkAttr "onEncrypted" handler
    static member inline onEnded (handler: Event -> unit) = Interop.mkAttr "onEnded" handler
    static member inline onLoadedData (handler: Event -> unit) = Interop.mkAttr "onLoadedData" handler
    static member inline onLoadedMetadata (handler: Event -> unit) = Interop.mkAttr "onLoadedMetadata" handler
    static member inline onLoadStart (handler: Event -> unit) = Interop.mkAttr "onLoadStart" handler
    static member inline onPause (handler: Event -> unit) = Interop.mkAttr "onPause" handler
    static member inline onPlay (handler: Event -> unit) = Interop.mkAttr "onPlay" handler
    static member inline onPlaying (handler: Event -> unit) = Interop.mkAttr "onPlaying" handler
    static member inline onProgress (handler: Event -> unit) = Interop.mkAttr "onProgress" handler
    static member inline onRateChange (handler: Event -> unit) = Interop.mkAttr "onRateChange" handler
    static member inline onSeeked (handler: Event -> unit) = Interop.mkAttr "onSeeked" handler
    static member inline onSeeking (handler: Event -> unit) = Interop.mkAttr "onSeeking" handler

    static member inline onStalled (handler: Event -> unit) = Interop.mkAttr "onStalled" handler
    static member inline onSuspend (handler: Event -> unit) = Interop.mkAttr "onSuspend" handler
    static member inline onTimeUpdate (handler: Event -> unit) = Interop.mkAttr "onTimeUpdate" handler
    static member inline onVolumeChange (handler: Event -> unit) = Interop.mkAttr "onVolumeChange" handler
    static member inline onWaiting (handler: Event -> unit) = Interop.mkAttr "onWaiting" handler
    static member inline onClick (handler: MouseEvent -> unit) = Interop.mkAttr "onClick" handler
    static member inline onContextMenu (handler: MouseEvent -> unit) = Interop.mkAttr "onContextMenu" handler
    static member inline onDoubleClick (handler: MouseEvent -> unit) = Interop.mkAttr "onDoubleClick" handler
    static member inline onDrag (handler: DragEvent -> unit) = Interop.mkAttr "onDrag" handler
    static member inline onDragEnd (handler: DragEvent -> unit) = Interop.mkAttr "onDragEnd" handler
    static member inline onDragEnter (handler: DragEvent -> unit) = Interop.mkAttr "onDragEnter" handler
    static member inline onDragExit (handler: DragEvent -> unit) = Interop.mkAttr "onDragExit" handler
    static member inline onDragLeave (handler: DragEvent -> unit) = Interop.mkAttr "onDragLeave" handler
    static member inline onDragOver (handler: DragEvent -> unit) = Interop.mkAttr "onDragOver" handler
    static member inline onDragStart (handler: DragEvent -> unit) = Interop.mkAttr "onDragStart" handler
    static member inline onDrop (handler: DragEvent -> unit) = Interop.mkAttr "onDrop" handler
    static member inline onMouseDown (handler: MouseEvent -> unit) = Interop.mkAttr "onMouseDown" handler
    static member inline onMouseEnter (handler: MouseEvent -> unit) = Interop.mkAttr "onMouseEnter" handler
    static member inline onMouseLeave (handler: MouseEvent -> unit) = Interop.mkAttr "onMouseLeave" handler
    static member inline onMouseMove (handler: MouseEvent -> unit) = Interop.mkAttr "onMouseMove" handler
    static member inline onMouseOut (handler: MouseEvent -> unit) = Interop.mkAttr "onMouseOut" handler
    static member inline onMouseOver (handler: MouseEvent -> unit) = Interop.mkAttr "onMouseOver" handler
    static member inline onSelect (handler: Event -> unit) = Interop.mkAttr "onSelect" handler
    static member inline onTouchCancel (handler: TouchEvent -> unit) = Interop.mkAttr "onTouchCancel" handler
    static member inline onTouchEnd (handler: TouchEvent -> unit) = Interop.mkAttr "onTouchEnd" handler
    static member inline onTouchMove (handler: TouchEvent -> unit) = Interop.mkAttr "onTouchMove" handler
    static member inline onTouchStart (handler: TouchEvent -> unit) = Interop.mkAttr "onTouchStart" handler
    static member inline onScroll (handler: UIEvent -> unit) = Interop.mkAttr "onScroll" handler
    static member inline onWheel (handler: WheelEvent -> unit) = Interop.mkAttr "onWheel" handler
    static member inline onAnimationStart (handler: AnimationEvent -> unit) = Interop.mkAttr "onAnimationStart" handler
    static member inline onAnimationEnd (handler: AnimationEvent -> unit) = Interop.mkAttr "onAnimationEnd" handler
    static member inline onAnimationIteration (handler: AnimationEvent -> unit) = Interop.mkAttr "onAnimationIteration" handler
    static member inline onTransitionEnd (handler: TransitionEvent -> unit) = Interop.mkAttr "onTransitionEnd" handler
    /// https://www.w3.org/WAI/PF/aria-1.1/roles
    static member inline role ([<System.ParamArray>] roles: string []) = Interop.mkAttr "role" (String.concat " " roles)
    /// Indicates whether assistive technologies will present all, or only parts of,
    /// the changed region based on the change notifications defined by the
    /// `aria-relevant` attribute.
    ///
    /// https://www.w3.org/WAI/PF/aria-1.1/states_and_properties#aria-atomic
    static member inline ariaAtomic (value: bool) = Interop.mkAttr "aria-atomic" value
    /// Indicates whether an element, and its subtree, are currently being updated.
    ///
    /// https://www.w3.org/WAI/PF/aria-1.1/states_and_properties#aria-busy
    static member inline ariaBusy (value: bool) = Interop.mkAttr "aria-busy" value
    /// Identifies the element (or elements) whose contents or presence are
    /// controlled by the current element. See related `aria-owns`.
    ///
    /// https://www.w3.org/WAI/PF/aria-1.1/states_and_properties#aria-controls
    static member inline ariaControls ([<System.ParamArray>] ids: string []) = Interop.mkAttr "aria-controls" (String.concat " " ids)
    /// Specifies a URI referencing content that describes the object.
    /// See related `aria-describedby`.
    ///
    /// https://www.w3.org/WAI/PF/aria-1.1/states_and_properties#aria-describedat
    static member inline ariaDescribedAt (uri: string) = Interop.mkAttr "aria-describedat" uri
    /// Identifies the element (or elements) that describes the object. See related
    /// `aria-describedat` and `aria-labelledby`.
    ///
    /// The `aria-labelledby` attribute is similar to `aria-describedby` in that both
    /// reference other elements to calculate a text alternative, but a label should
    /// be concise, where a description is intended to provide more verbose information.
    ///
    /// https://www.w3.org/WAI/PF/aria-1.1/states_and_properties#aria-describedby
    static member inline ariaDescribedBy ([<System.ParamArray>] ids: string []) = Interop.mkAttr "aria-describedby" (String.concat " " ids)
    /// Indicates that the element is perceivable but disabled, so it is not editable
    /// or otherwise operable. See related `aria-hidden` and `aria-readonly`.
    ///
    /// https://www.w3.org/WAI/PF/aria-1.1/states_and_properties#aria-disabled
    static member inline ariaDisabled (value: bool) = Interop.mkAttr "aria-disabled" value
    /// Indicates what functions can be performed when the dragged object is released
    /// on the drop target. This allows assistive technologies to convey the possible
    /// drag options available to users, including whether a pop-up menu of choices
    /// is provided by the application. Typically, drop effect functions can only
    /// be provided once an object has been grabbed for a drag operation as the
    /// drop effect functions available are dependent on the object being dragged.
    ///
    /// https://www.w3.org/WAI/PF/aria-1.1/states_and_properties#aria-dropeffect
    static member inline ariaDropEffect ([<System.ParamArray>] values: AriaDropEffect []) = Interop.mkAttr "aria-dropeffect" (values |> unbox<string []> |> String.concat " ")
    /// Identifies the next element (or elements) in an alternate reading order of content which,
    /// at the user's discretion, allows assistive technology to override the general default of
    /// reading in document source order.
    ///
    /// https://www.w3.org/WAI/PF/aria-1.1/states_and_properties#aria-flowto
    static member inline ariaFlowTo ([<System.ParamArray>] ids: string []) = Interop.mkAttr "aria-flowto" (String.concat " " ids)
    /// Indicates an element's "grabbed" state in a drag-and-drop operation.
    ///
    /// When it is set to true it has been selected for dragging, false indicates
    /// that the element can be grabbed for a drag-and-drop operation, but is not
    /// currently grabbed, and undefined (or no value) indicates the element cannot
    /// be grabbed (default).
    ///
    /// https://www.w3.org/WAI/PF/aria-1.1/states_and_properties#aria-grabbed
    static member inline ariaGrabbed (value: bool) = Interop.mkAttr "aria-grabbed" value
    /// Indicates that the element has a popup context menu or sub-level menu.
    ///
    /// https://www.w3.org/WAI/PF/aria-1.1/states_and_properties#aria-haspopup
    static member inline ariaHasPopup (value: bool) = Interop.mkAttr "aria-haspopup" value
    /// Indicates that the element and all of its descendants are not visible or perceivable
    /// to any user as implemented by the author. See related `aria-disabled`.
    ///
    /// https://www.w3.org/WAI/PF/aria-1.1/states_and_properties#aria-hidden
    static member inline ariaHidden (value: bool) = Interop.mkAttr "aria-hidden" value
    /// Indicates the entered value does not conform to the format expected by the application.
    ///
    /// https://www.w3.org/WAI/PF/aria-1.1/states_and_properties#aria-invalid
    static member inline ariaInvalid (value: bool) = Interop.mkAttr "aria-invalid" value
    /// Defines a string value that labels the current element. See related `aria-labelledby`.
    ///
    /// https://www.w3.org/WAI/PF/aria-1.1/states_and_properties#aria-label
    static member inline ariaLabel (value: string) = Interop.mkAttr "aria-label" value
    /// Identifies the element (or elements) that labels the current element.
    /// See related `aria-label` and `aria-describedby`.
    ///
    /// https://www.w3.org/WAI/PF/aria-1.1/states_and_properties#aria-labelledby
    static member inline ariaLabelledBy ([<System.ParamArray>] ids: string []) = Interop.mkAttr "aria-labelledby" (String.concat " " ids)
    /// Identifies an element (or elements) in order to define a visual, functional, or
    /// contextual parent/child relationship between DOM elements where the DOM hierarchy
    /// cannot be used to represent the relationship. See related `aria-controls`.
    ///
    /// https://www.w3.org/WAI/PF/aria-1.1/states_and_properties#aria-owns
    static member inline ariaOwns ([<System.ParamArray>] ids: string []) = Interop.mkAttr "aria-owns" (String.concat " " ids)
    /// Indicates what user agent change notifications (additions, removals, etc.)
    /// assistive technologies will receive within a live region. See related `aria-atomic`.
    ///
    /// https://www.w3.org/WAI/PF/aria-1.1/states_and_properties#aria-relevant
    static member inline ariaRelevant ([<System.ParamArray>] values: AriaRelevant []) = Interop.mkAttr "aria-relevant" (values |> unbox<string []> |> String.concat " ")
    static member inline style (properties: #IStyleAttribute list) = Interop.mkAttr "style" (createObj !!properties)
    static member style (properties: (bool * IStyleAttribute list) list) =
        properties
        |> List.filter fst
        |> List.collect snd
        |> unbox
        |> createObj
        |> Interop.mkAttr "style"

module prop =

    /// https://www.w3.org/WAI/PF/aria-1.1/roles
    [<Erase>]
    type role =
        /// A message with important, and usually time-sensitive, information.
        /// See related `alertdialog` and `status`.
        ///
        /// https://www.w3.org/WAI/PF/aria-1.1/roles#alert
        static member inline alert = Interop.mkAttr "role" "alert"
        /// A type of dialog that contains an alert message, where initial focus
        /// goes to an element within the dialog. See related `alert` and
        /// `dialog`.
        ///
        /// https://www.w3.org/WAI/PF/aria-1.1/roles#alertdialog
        static member inline alertDialog = Interop.mkAttr "role" "alertdialog"
        /// An input that allows for user-triggered actions when clicked or
        /// pressed. See related `link`.
        ///
        /// https://www.w3.org/WAI/PF/aria-1.1/roles#button
        static member inline button = Interop.mkAttr "role" "button"
        /// A checkable input that has three possible values: `true`, `false`,
        /// or `mixed`.
        ///
        /// https://www.w3.org/WAI/PF/aria-1.1/roles#checkbox
        static member inline checkbox = Interop.mkAttr "role" "checkbox"
        /// A dialog is an application window that is designed to interrupt the
        /// current processing of an application in order to prompt the user to
        /// enter information or require a response. See related `alertdialog`.
        /// 
        /// https://www.w3.org/WAI/PF/aria-1.1/roles#dialog
        static member inline dialog = Interop.mkAttr "role" "dialog"
        /// A cell in a grid or treegrid.
        ///
        /// https://www.w3.org/WAI/PF/aria-1.1/roles#gridcell
        static member inline gridCell = Interop.mkAttr "role" "gridcell"
        /// An interactive reference to an internal or external resource that,
        /// when activated, causes the user agent to navigate to that resource.
        /// See related `button`.
        ///
        /// https://www.w3.org/WAI/PF/aria-1.1/roles#link
        static member inline link = Interop.mkAttr "role" "link"
        /// A type of live region where new information is added in meaningful
        /// order and old information may disappear. See related `marquee`.
        ///
        /// https://www.w3.org/WAI/PF/aria-1.1/roles#log
        static member inline log = Interop.mkAttr "role" "log"
        /// A type of live region where non-essential information changes
        /// frequently. See related `log`.
        ///
        /// https://www.w3.org/WAI/PF/aria-1.1/roles#marquee
        static member inline marquee = Interop.mkAttr "role" "marquee"
        /// An option in a set of choices contained by a `menu` or `menubar`.
        ///
        /// https://www.w3.org/WAI/PF/aria-1.1/roles#menuitem
        static member inline menuItem = Interop.mkAttr "role" "menuitem"
        /// A `menuitem` with a checkable state whose possible values are
        /// `true`, `false`, or `mixed`.
        ///
        /// https://www.w3.org/WAI/PF/aria-1.1/roles#menuitemcheckbox
        static member inline menuItemCheckbox = Interop.mkAttr "role" "menuitemcheckbox"
        /// A checkable menuitem in a set of elements with role `menuitemradio`,
        /// only one of which can be checked at a time.
        ///
        /// https://www.w3.org/WAI/PF/aria-1.1/roles#menuitemradio
        static member inline menuItemRadio = Interop.mkAttr "role" "menuitemradio"
        /// A selectable item in a `select` list.
        ///
        /// https://www.w3.org/WAI/PF/aria-1.1/roles#option
        static member inline option = Interop.mkAttr "role" "option"
        /// An element that displays the progress status for tasks that take a
        /// long time.
        ///
        /// https://www.w3.org/WAI/PF/aria-1.1/roles#progressbar
        static member inline progressBar = Interop.mkAttr "role" "progressbar"
        /// A checkable input in a group of elements with role radio, only one
        /// of which can be checked at a time.
        ///
        /// https://www.w3.org/WAI/PF/aria-1.1/roles#radio
        static member inline radio = Interop.mkAttr "role" "radio"
        /// A graphical object that controls the scrolling of content within a
        /// viewing area, regardless of whether the content is fully displayed
        /// within the viewing area.
        ///
        /// https://www.w3.org/WAI/PF/aria-1.1/roles#scrollbar
        static member inline scrollBar = Interop.mkAttr "role" "scrollbar"
        /// A user input where the user selects a value from within a given
        /// range.
        ///
        /// https://www.w3.org/WAI/PF/aria-1.1/roles#slider
        static member inline slider = Interop.mkAttr "role" "slider"
        /// A form of `range` that expects the user to select from among
        /// discrete choices.
        ///
        /// https://www.w3.org/WAI/PF/aria-1.1/roles#spinbutton
        static member inline spinButton = Interop.mkAttr "role" "spinbutton"
        /// A container whose content is advisory information for the user but
        /// is not important enough to justify an alert, often but not
        /// necessarily presented as a status bar. See related `alert`.
        ///
        /// https://www.w3.org/WAI/PF/aria-1.1/roles#status
        static member inline status = Interop.mkAttr "role" "status"
        /// A grouping label providing a mechanism for selecting the tab content
        /// that is to be rendered to the user.
        ///
        /// https://www.w3.org/WAI/PF/aria-1.1/roles#tab
        static member inline tab = Interop.mkAttr "role" "tab"
        /// A container for the resources associated with a `tab`, where each
        /// `tab` is contained in a `tablist`.
        ///
        /// https://www.w3.org/WAI/PF/aria-1.1/roles#tabpanel
        static member inline tabPanel = Interop.mkAttr "role" "tabpanel"
        /// Input that allows free-form text as its value.
        ///
        /// https://www.w3.org/WAI/PF/aria-1.1/roles#textbox
        static member inline textBox = Interop.mkAttr "role" "textbox"
        /// A type of live region containing a numerical counter which indicates
        /// an amount of elapsed time from a start point, or the time remaining
        /// until an end point.
        ///
        /// https://www.w3.org/WAI/PF/aria-1.1/roles#timer
        static member inline timer = Interop.mkAttr "role" "timer"
        /// A contextual popup that displays a description for an element.
        ///
        /// https://www.w3.org/WAI/PF/aria-1.1/roles#tooltip
        static member inline tooltip = Interop.mkAttr "role" "tooltip"
        /// An option item of a `tree`. This is an element within a tree that
        /// may be expanded or collapsed if it contains a sub-level group of
        /// `treeitem` elements.
        ///
        /// https://www.w3.org/WAI/PF/aria-1.1/roles#treeitem
        static member inline treeItem = Interop.mkAttr "role" "treeitem"
        /// A presentation of a `select`; usually similar to a `textbox` where
        /// users can type ahead to select an option, or type to enter arbitrary
        /// text as a new item in the list. See related `listbox`.
        ///
        /// https://www.w3.org/WAI/PF/aria-1.1/roles#combobox
        static member inline comboBox = Interop.mkAttr "role" "combobox"
        /// A grid is an interactive control which contains cells of tabular
        /// data arranged in rows and columns, like a table.
        ///
        /// https://www.w3.org/WAI/PF/aria-1.1/roles#grid
        static member inline grid = Interop.mkAttr "role" "grid"
        /// A widget that allows the user to select one or more items from a
        /// list of choices. See related `combobox` and `list`.
        ///
        /// https://www.w3.org/WAI/PF/aria-1.1/roles#listbox
        static member inline listBox = Interop.mkAttr "role" "listbox"
        /// A type of widget that offers a list of choices to the user.
        ///
        /// https://www.w3.org/WAI/PF/aria-1.1/roles#menu
        static member inline menu = Interop.mkAttr "role" "menu"
        /// A presentation of `menu` that usually remains visible and is usually
        /// presented horizontally.
        ///
        /// https://www.w3.org/WAI/PF/aria-1.1/roles#menubar
        static member inline menuBar = Interop.mkAttr "role" "menubar"
        /// A group of radio buttons.
        ///
        /// https://www.w3.org/WAI/PF/aria-1.1/roles#radiogroup
        static member inline radioGroup = Interop.mkAttr "role" "radiogroup"
        /// A list of `tab` elements, which are references to `tabpanel`
        /// elements.
        ///
        /// https://www.w3.org/WAI/PF/aria-1.1/roles#tablist
        static member inline tabList = Interop.mkAttr "role" "tablist"
        /// A type of `list` that may contain sub-level nested groups that can
        /// be collapsed and expanded.
        ///
        /// https://www.w3.org/WAI/PF/aria-1.1/roles#tree
        static member inline tree = Interop.mkAttr "role" "tree"
        /// A `grid` whose rows can be expanded and collapsed in the same manner
        /// as for a `tree`.
        ///
        /// https://www.w3.org/WAI/PF/aria-1.1/roles#treegrid
        static member inline treeGrid = Interop.mkAttr "role" "treegrid"
        /// A section of a page that consists of a composition that forms an
        /// independent part of a document, page, or site.
        ///
        /// https://www.w3.org/WAI/PF/aria-1.1/roles#article
        static member inline article = Interop.mkAttr "role" "article"
        /// A cell containing header information for a column.
        ///
        /// https://www.w3.org/WAI/PF/aria-1.1/roles#columnheader
        static member inline columnHeader = Interop.mkAttr "role" "columnheader"
        /// A definition of a term or concept.
        ///
        /// https://www.w3.org/WAI/PF/aria-1.1/roles#definition
        static member inline definition = Interop.mkAttr "role" "definition"
        /// A list of references to members of a group, such as a static table
        /// of contents.
        ///
        /// https://www.w3.org/WAI/PF/aria-1.1/roles#directory
        static member inline directory = Interop.mkAttr "role" "directory"
        /// A region containing related information that is declared as document
        /// content, as opposed to a web application.
        ///
        /// https://www.w3.org/WAI/PF/aria-1.1/roles#document
        static member inline document = Interop.mkAttr "role" "document"
        /// A set of user interface objects which are not intended to be
        /// included in a page summary or table of contents by assistive
        /// technologies.
        ///
        /// https://www.w3.org/WAI/PF/aria-1.1/roles#group
        static member inline group = Interop.mkAttr "role" "group"
        /// A heading for a section of the page.
        ///
        /// https://www.w3.org/WAI/PF/aria-1.1/roles#heading
        static member inline heading = Interop.mkAttr "role" "heading"
        /// A container for a collection of elements that form an image.
        ///
        /// https://www.w3.org/WAI/PF/aria-1.1/roles#img
        static member inline img = Interop.mkAttr "role" "img"
        /// A group of non-interactive list items. See related `listbox`.
        ///
        /// https://www.w3.org/WAI/PF/aria-1.1/roles#list
        static member inline list = Interop.mkAttr "role" "list"
        /// A single item in a list or directory.
        ///
        /// https://www.w3.org/WAI/PF/aria-1.1/roles#listitem
        static member inline listItem = Interop.mkAttr "role" "listitem"
        /// Content that represents a mathematical expression. 
        ///
        /// https://www.w3.org/WAI/PF/aria-1.1/roles#math
        static member inline math = Interop.mkAttr "role" "math"
        /// A section whose content is parenthetic or ancillary to the main
        /// content of the resource.
        ///
        /// https://www.w3.org/WAI/PF/aria-1.1/roles#note
        static member inline note = Interop.mkAttr "role" "note"
        /// An element whose implicit native role semantics will not be mapped
        /// to the accessibility API.
        ///
        /// https://www.w3.org/WAI/PF/aria-1.1/roles#presentation
        static member inline presentation = Interop.mkAttr "role" "presentation"
        /// A large perceivable section of a web page or document, that is
        /// important enough to be included in a page summary or table of
        /// contents, for example, an area of the page containing live sporting
        /// event statistics.
        ///
        /// https://www.w3.org/WAI/PF/aria-1.1/roles#region
        static member inline region = Interop.mkAttr "role" "region"
        /// A row of cells in a grid.
        ///
        /// https://www.w3.org/WAI/PF/aria-1.1/roles#row
        static member inline row = Interop.mkAttr "role" "row"
        /// A group containing one or more row elements in a grid.
        ///
        /// https://www.w3.org/WAI/PF/aria-1.1/roles#rowgroup
        static member inline rowGroup = Interop.mkAttr "role" "rowgroup"
        /// A cell containing header information for a row in a grid.
        ///
        /// https://www.w3.org/WAI/PF/aria-1.1/roles#rowheader
        static member inline rowHeader = Interop.mkAttr "role" "rowheader"
        /// A divider that separates and distinguishes sections of content or
        /// groups of menuitems.
        ///
        /// https://www.w3.org/WAI/PF/aria-1.1/roles#separator
        static member inline separator = Interop.mkAttr "role" "separator"
        /// A collection of commonly used function buttons or controls
        /// represented in compact visual form.
        ///
        /// https://www.w3.org/WAI/PF/aria-1.1/roles#toolbar
        static member inline toolbar = Interop.mkAttr "role" "toolbar"
        /// A region declared as a web application, as opposed to a web
        /// `document`.
        ///
        /// https://www.w3.org/WAI/PF/aria-1.1/roles#application
        static member inline application = Interop.mkAttr "role" "application"
        /// A region that contains mostly site-oriented content, rather than
        /// page-specific content.
        ///
        /// https://www.w3.org/WAI/PF/aria-1.1/roles#banner
        static member inline banner = Interop.mkAttr "role" "banner"
        /// A supporting section of the document, designed to be complementary
        /// to the main content at a similar level in the DOM hierarchy, but
        /// remains meaningful when separated from the main content.
        ///
        /// https://www.w3.org/WAI/PF/aria-1.1/roles#complementary
        static member inline complementary = Interop.mkAttr "role" "complementary"
        /// A large perceivable region that contains information about the
        /// parent document.
        ///
        /// https://www.w3.org/WAI/PF/aria-1.1/roles#contentinfo
        static member inline contentInfo = Interop.mkAttr "role" "contentinfo"
        /// A `landmark` region that contains a collection of items and objects
        /// that, as a whole, combine to create a form. See related search.
        ///
        /// https://www.w3.org/WAI/PF/aria-1.1/roles#form
        static member inline form = Interop.mkAttr "role" "form"
        /// The main content of a document.
        ///
        /// https://www.w3.org/WAI/PF/aria-1.1/roles#main
        static member inline main = Interop.mkAttr "role" "main"
        /// A collection of navigational elements (usually links) for navigating
        /// the document or related documents.
        ///
        /// https://www.w3.org/WAI/PF/aria-1.1/roles#navigation
        static member inline navigation = Interop.mkAttr "role" "navigation"
        /// A `landmark` region that contains a collection of items and objects
        /// that, as a whole, combine to create a search facility. See related
        /// `form`.
        ///
        /// https://www.w3.org/WAI/PF/aria-1.1/roles#search
        static member inline search = Interop.mkAttr "role" "search"

    /// Indicates an element's "grabbed" state in a drag-and-drop operation.
    ///
    /// When it is set to true it has been selected for dragging, false indicates
    /// that the element can be grabbed for a drag-and-drop operation, but is not
    /// currently grabbed, and undefined (or no value) indicates the element cannot
    /// be grabbed (default).
    ///
    /// https://www.w3.org/WAI/PF/aria-1.1/states_and_properties#aria-grabbed
    [<Erase>]
    type ariaGrabbed =
        static member inline undefined = Interop.mkAttr "aria-grabbed" "undefined"

    /// Indicates the entered value does not conform to the format expected by the application.
    ///
    /// https://www.w3.org/WAI/PF/aria-1.1/states_and_properties#aria-invalid
    [<Erase>]
    type ariaInvalid =
        /// A grammatical error was detected.
        static member inline grammar = Interop.mkAttr "aria-invalid" "grammar"
        /// A spelling error was detected.
        static member inline spelling = Interop.mkAttr "aria-invalid" "spelling"

    /// Indicates that an element will be updated, and describes the types of updates
    /// the user agents, assistive technologies, and user can expect from the live region.
    ///
    /// https://www.w3.org/WAI/PF/aria-1.1/states_and_properties#aria-live
    [<Erase>]
    type ariaLive =
        /// Indicates that updates to the region have the highest priority and
        /// should be presented the user immediately.
        static member inline assertive = Interop.mkAttr "aria-live" "assertive"
        /// Indicates that updates to the region should not be presented to the user
        /// unless the used is currently focused on that region.
        static member inline off = Interop.mkAttr "aria-live" "off"
        /// Indicates that updates to the region should be presented at the next graceful
        /// opportunity, such as at the end of speaking the current sentence or when the user
        /// pauses typing.
        static member inline polite = Interop.mkAttr "aria-live" "polite"

    type inputType =
        /// Defines a password field
        static member inline password = Interop.mkAttr "type" "password"
        /// Default. Defines a single-line text field
        static member inline text = Interop.mkAttr "type" "text"
        /// Defines a clickable button (mostly used with a JavaScript code to activate a script)
        static member inline button = Interop.mkAttr "type" "button"
        /// Defines a checkbox
        static member inline checkbox = Interop.mkAttr "type" "checkbox"
        /// Defines a color picker
        static member inline color = Interop.mkAttr "type" "color"
        /// Defines a date control with year, month and day (no time)
        static member inline date = Interop.mkAttr "type" "date"
        /// Defines a date and time control (year, month, day, time (no timezone)
        static member inline dateTimeLocal = Interop.mkAttr "type" "datetime-local"
        /// Defines a field for an e-mail address
        static member inline email = Interop.mkAttr "type" "email"
        /// Defines a file-select field and a "Browse" button (for file uploads)
        static member inline file = Interop.mkAttr "type" "file"
        /// Defines a hidden input field
        static member inline hidden = Interop.mkAttr "type" "hidden"
        /// Defines an image as the submit button
        static member inline image = Interop.mkAttr "type" "image"
        /// Defines a month and year control (no timezone)
        static member inline month = Interop.mkAttr "type" "month"
        /// Defines a field for entering a number
        static member inline number = Interop.mkAttr "type" "number"
        /// Defines a radio button
        static member inline radio = Interop.mkAttr "type" "radio"
        /// Defines a range control (like a slider control)
        static member inline range = Interop.mkAttr "type" "range"
        /// Defines a reset button
        static member inline reset = Interop.mkAttr "type" "reset"
        /// Defines a text field for entering a search string
        static member inline search = Interop.mkAttr "type" "search"
        /// Defines a submit button
        static member inline submit = Interop.mkAttr "type" "submit"
        /// Defines a field for entering a telephone number
        static member inline tel = Interop.mkAttr "type" "tel"
        /// Defines a control for entering a time (no timezone)
        static member inline time = Interop.mkAttr "type" "time"
        /// Defines a field for entering a URL
        static member inline url = Interop.mkAttr "type" "url"
        /// Defines a week and year control (no timezone)
        static member inline week = Interop.mkAttr "type" "week"