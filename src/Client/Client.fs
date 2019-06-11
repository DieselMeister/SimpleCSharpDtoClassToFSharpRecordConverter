module Client

open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open Fetch.Types
open Thoth.Fetch
open Fulma
open Thoth.Json



type Model = {
    InputClass:string
    OutputRecord:string
    ErrorMessage:string
}

type Msg =
| Translate
| Translated of string
| ChangeInputClass of string
| Error of exn


let convert inputClass =
    Fetch.post<string,string> ("/api/convert",inputClass)


let init () : Model * Cmd<Msg> =
    
    let initialModel = {
        InputClass = ""
        OutputRecord = ""
        ErrorMessage = ""
    }
    
    initialModel, Cmd.none

let update (msg : Msg) (model : Model) : Model * Cmd<Msg> =
    match msg with
    | Translate ->
        model, (Cmd.OfPromise.either convert model.InputClass Translated Error)
    | Translated result ->
        { model with OutputRecord = result }, Cmd.none
    | Error e ->
        { model with ErrorMessage = e.Message}, Cmd.none
    | ChangeInputClass str ->
        { model with InputClass = str}, Cmd.none



let view (model : Model) (dispatch : Msg -> unit) =
    div []
        [
            Navbar.navbar [
                    Navbar.Color IsPrimary
                ] [
                    Navbar.Item.div [ ]
                        [
                            Heading.h2 [ ] [
                                str "C# Class to F# Record Converter"
                            ]
                        ]
                ]

            Container.container [] [
                Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ] [
                    Heading.h3 [] [ str "convert C# class to F# record" ]
                ]
                Columns.columns [] [
                    Column.column [] [
                        Textarea.textarea [
                            Textarea.ValueOrDefault model.InputClass
                            Textarea.OnChange (fun e -> dispatch (ChangeInputClass e.Value))
                        ] [

                        ]
                    ]
                    Column.column [] [
                        Textarea.textarea [
                            Textarea.ValueOrDefault model.OutputRecord
                            Textarea.IsReadOnly true
                        ] [

                        ]
                    ]
                ]
                Columns.columns [] [
                    Column.column [] [
                        button [ OnClick (fun _ -> dispatch Translate) ]  [ str "translate class"]
                    ]
                ]
            ]

            Footer.footer [ ] [
                Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ] [
                    str "Footer"
                ]
            ]
        ]

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram init update view
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactBatched "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
