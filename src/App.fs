module App

open Elmish
open Elmish.React
open Feliz
open Fable.SimpleHttp

type RemoteData<'a> =
  | HasNotLoadedYet
  | Loading 
  | FinishedLoading of 'a

type State =
    { 
      Count: int
      ResponseText : RemoteData<Result<string, string>> 
    }

type Msg =
    | Increment
    | Decrement
    | GetData
    | DataReceived of string 
    | ErrorWhileReceivingData of string 

let getData () = 
  async {
    do! Async.Sleep 1000
    let! (statusCode, responseText) = Http.get "/data.txt"
    return
      if statusCode = 200 then 
        DataReceived responseText
      else
        ErrorWhileReceivingData (sprintf "Something weird happened: %i" statusCode)
  }

let withGetDataCommand state =
  { state with ResponseText = Loading }, Cmd.OfAsync.perform getData () id

let withoutCommands state =
  state, Cmd.none

let init() =
    { 
      Count = 0 
      ResponseText = HasNotLoadedYet
    }
    |> withGetDataCommand


let update (msg: Msg) (state: State): State * Cmd<Msg>  =
    match msg with
    | GetData ->
        state
        |> withGetDataCommand

    | Increment ->
        { state with Count = state.Count + 1 }
        |> withoutCommands

    | Decrement ->
        { state with Count = state.Count - 1 }
        |> withoutCommands

    | DataReceived data ->
        { state with ResponseText = FinishedLoading (Ok data) }
        |> withoutCommands

    | ErrorWhileReceivingData error  -> 
        { state with ResponseText = FinishedLoading (Error error) }
        |> withoutCommands

let private renderResponseText (responseText : RemoteData<Result<string, string>>) =
  match responseText with  
  | HasNotLoadedYet ->
      Html.none

  | Loading ->
      Html.h1 "Imagine fancy spinner"

  | FinishedLoading (Ok data) ->
      Html.h1 data

  | FinishedLoading (Error error) ->
      Html.h1 [
        prop.style [ style.color.red ]
        prop.text error
      ]


let render (state: State) (dispatch: Msg -> unit) =
  Html.div [
    Html.button [
      prop.onClick (fun _ -> dispatch GetData)
      prop.text "Get Data"
    ]
    Html.button [
      prop.onClick (fun _ -> dispatch GetData)
      prop.text "Increment"
    ]

    Html.button [
      prop.onClick (fun _ -> dispatch Decrement)
      prop.text "Decrement"
    ]

    renderResponseText state.ResponseText

    Html.h1 state.Count
  ]

Program.mkProgram init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run