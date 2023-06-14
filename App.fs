module App

open System
open Fable.Core
open Fable.Core.JsInterop
open Browser.Dom
open Fable.React
open Fable.React.Props
open Feliz

#if DEBUG
importSideEffects "./WebSocket.js"
#endif

type React =
    static member useState<'T>(_initialState: 'T) : 'T * ('T -> unit) = import "useState" "react"
    static member useEffect(_action: unit -> unit, _dependencies: obj array) : unit = import "useEffect" "react"

let useCopyToClipboard () : string * (string -> unit) =
    import "useCopyToClipboard" "usehooks-ts"

// https://colorsea.js.org/pages/QuickStart.html#import
[<AbstractClass>]
type ColourSea =
    [<Emit("$0.darken($1)")>]
    abstract member darken: amount: int -> ColourSea

    [<Emit("$0.lighten($1)")>]
    abstract member lighten: amount: int -> ColourSea

    [<Emit("$0.hex($1)")>]
    abstract member hex: unit -> string

let colourSea (_: string) : ColourSea = importDefault "colorsea"

let isBlackTextOk (hex: string) =
    if String.IsNullOrWhiteSpace hex || hex.Length < 7 then
        false
    else
        let color = if hex.[0] = '#' then hex.Substring(1, 6).ToUpper() else hex
        let hexToFloat v = Convert.ToInt32(v, 16) |> float
        let r = hexToFloat (color.Substring(0, 2))
        let g = hexToFloat (color.Substring(2, 2))
        let b = hexToFloat (color.Substring(4, 2))
        (r * 0.299) + (g * 0.587) + (b * 0.114) > 186.0

[<ReactComponent>]
let App () =
    let colour, setColour = React.useState "#42A5E7"
    let name, setName = React.useState "primary"
    let level, setLevel = React.useState 15

    let updateLevel (v: string) =
        match Int32.TryParse v with
        | false, _ -> setLevel 15
        | true, v -> setLevel v

    let copyButtonText, setCopyButtonText = React.useState "Click to copy"
    let _, copy = useCopyToClipboard ()

    React.useEffect (
        fun () ->
            if copyButtonText = "Copied!" then
                JS.setTimeout (fun () -> setCopyButtonText "Click to copy") 400 |> ignore
        , [| copyButtonText |]
    )

    let result name (colour: string) level =
        let colour = colour.Trim()
        let hoverColour = colourSea(colour).lighten(level).hex ()
        let borderColour = colourSea(colour).darken(level).hex ()
        let textColour = if isBlackTextOk colour then "black" else "white"

        let resultCode =
            $"""    --%s{name}: %s{colour};
    --%s{name}-hover: %s{hoverColour};
    --%s{name}-border: %s{borderColour};"""

        let resultText =
            $""":root {{
%s{resultCode}
}}"""

        div [ Id "results" ] [
            code [] [ pre [] [ str resultText ] ]
            button [
                OnClick(fun ev ->
                    ev.preventDefault ()
                    copy resultCode
                    setCopyButtonText "Copied!")
                OnMouseEnter(fun ev -> ev?target?style?backgroundColor <- hoverColour)
                OnMouseLeave(fun ev -> ev?target?style?backgroundColor <- colour)
                Style [
                    Color textColour
                    BackgroundColor colour
                    Border $"1px solid {borderColour}"
                    Cursor "pointer"
                ]
            ] [ str copyButtonText ]
        ]

    fragment [] [
        form [] [
            input [ Type "color"; Value colour; OnChange(fun e -> setColour e.Value) ]
            input [ Type "text"; Value colour; OnChange(fun e -> setColour e.Value) ]
            label [] [ str "variable name" ]
            input [ Type "text"; DefaultValue name; OnChange(fun e -> setName e.Value) ]
            label [] [ str "Level" ]
            input [ Type "number"; DefaultValue level; OnChange(fun e -> updateLevel e.Value) ]
        ]
        result name colour level
    ]

let app = document.querySelector "main"
ReactDom.render (App(), app)
