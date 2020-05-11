// Copyright 2018-2019 Fabulous contributors. See LICENSE.md for license.
namespace BertQA

open System.Diagnostics
open Fabulous
open Fabulous.XamarinForms
open Fabulous.XamarinForms.LiveUpdate
open Xamarin.Forms
open System

module App = 

    type Msg = 
        | Scoring of context:string*query:string
        | Scored of string option
        | ListItemSel of int option
        | NavigationPopped
       
    let initModel = Model.initModel()
    
    let init () = initModel, Cmd.none

    let scoreCmd (ctx,q) =
        async {
            do! Async.SwitchToThreadPool()
            let rslt = DroidInterop.scoreText(ctx,q)
            return (Scored rslt)                                                        
        }
        |> Cmd.ofAsyncMsg

    let update msg model =
        match msg with
        | Scoring (ctx,q)  -> {model with ScoreRequested = true}, scoreCmd (ctx,q)
        | Scored scs -> {model with Score=scs; ScoreRequested=false}, Cmd.none
        | ListItemSel idx -> idx |> Option.map (fun i -> {model with Page=QAPage; CurrItem=i},Cmd.none) |> Option.defaultValue (model,Cmd.none)
        | NavigationPopped -> {model with Page=HomePage; Score=None; ScoreRequested=false}, Cmd.none

    let listView model dispatch =
        View.ListView(
            items =
                    (model.Contexts
                    |> Array.map (fun c -> View.TextCell c.Title)
                    |> Array.toList),


            itemSelected = (fun idx -> dispatch (ListItemSel idx)) 
        )

    let highlight (text:string) hlt =
        hlt
        |> Option.map(fun (h:string) -> 
            let idx = text.IndexOf(h)
            if idx >= 0 then
                [
                    yield View.Span(text.Substring(0,idx))
                    yield View.Span(h, backgroundColor=Color.Orange)
                    yield View.Span(text.Substring(idx+h.Length))
                ]
            else
                [View.Span(text)])
        |> Option.defaultValue [View.Span(text)]

    //main view
    let view (model: Model) dispatch =

        let rootPage dispatch = 
            View.ContentPage(
              useSafeArea=true,
              title = "Question and Answer: Select Item",
              content = View.StackLayout(padding = Thickness 20.0, verticalOptions = LayoutOptions.Center, 
                children = [ 
                    listView model dispatch
                ]))

        let queryPage dispatch =
            View.ContentPage(
              useSafeArea = true,
              title = "Query Text",
              content = View.StackLayout(padding = Thickness 20.0, verticalOptions = LayoutOptions.Start,
                children = [                     
                    View.Label(text = model.Contexts.[model.CurrItem].Title, verticalOptions=LayoutOptions.Start,fontAttributes=FontAttributes.Bold)
                    View.Label(
                        formattedText = View.FormattedString(spans=highlight model.Contexts.[model.CurrItem].Context model.Score),
                        verticalOptions=LayoutOptions.Start)
                    View.StackLayout(
                        children = 
                            [
                                View.Label(text = "Questions", padding=Thickness 10., fontAttributes=FontAttributes.Bold, backgroundColor=Color.Coral,verticalOptions=LayoutOptions.End)
                                View.CarouselView(
                                    items = 
                                        (model.Contexts.[model.CurrItem].Queries 
                                            |> Array.map (fun x-> 
                                                View.Frame(
                                                    content =  View.Label( 
                                                                    text = x,
                                                                    gestureRecognizers = 
                                                                        [ 
                                                                            View.TapGestureRecognizer(command=(fun () -> dispatch (Scoring (model.Contexts.[model.CurrItem].Context,x))))
                                                                        ]), 
                                                    backgroundColor=Color.LightSeaGreen,  padding=Thickness 5.,margin=Thickness 50., hasShadow=true, 
                                                    borderColor=Color.LightGray, cornerRadius=10.0))
                                            |> Array.toList),
                                    horizontalScrollBarVisibility = ScrollBarVisibility.Always, 
                                    verticalOptions=LayoutOptions.End,
                                    peekAreaInsets = Thickness 50.
                                    

                                    )
                                View.ActivityIndicator(isRunning=model.ScoreRequested, height=20.)
                            ],
                        
                        verticalOptions=LayoutOptions.End
                    ).HeightRequest(150.0)
                ])
                )
              .HasNavigationBar(true)
              .HasBackButton(true)


        View.NavigationPage (
            popped = (fun _ -> dispatch NavigationPopped), 
            
            pages =
                [
                    yield rootPage dispatch
                    if model.Page=QAPage then
                        yield queryPage dispatch

                ]
            )


    // Note, this declaration is needed if you enable LiveUpdate
    let program = XamarinFormsProgram.mkProgram init update view

type App () as app = 
    inherit Application ()
    
    let runner = 
        App.program
#if DEBUG
        |> Program.withConsoleTrace
#endif
        |> XamarinFormsProgram.run app

#if DEBUG
    // Uncomment this line to enable live update in debug mode. 
    // See https://fsprojects.github.io/Fabulous/Fabulous.XamarinForms/tools.html#live-update for further  instructions.
    //
    //do runner.EnableLiveUpdate()
#endif    

    // Uncomment this code to save the application state to app.Properties using Newtonsoft.Json
    // See https://fsprojects.github.io/Fabulous/Fabulous.XamarinForms/models.html#saving-application-state for further  instructions.
#if APPSAVE
    let modelId = "model"
    override __.OnSleep() = 

        let json = Newtonsoft.Json.JsonConvert.SerializeObject(runner.CurrentModel)
        Console.WriteLine("OnSleep: saving model into app.Properties, json = {0}", json)

        app.Properties.[modelId] <- json

    override __.OnResume() = 
        Console.WriteLine "OnResume: checking for model in app.Properties"
        try 
            match app.Properties.TryGetValue modelId with
            | true, (:? string as json) -> 

                Console.WriteLine("OnResume: restoring model from app.Properties, json = {0}", json)
                let model = Newtonsoft.Json.JsonConvert.DeserializeObject<App.Model>(json)

                Console.WriteLine("OnResume: restoring model from app.Properties, model = {0}", (sprintf "%0A" model))
                runner.SetCurrentModel (model, Cmd.none)

            | _ -> ()
        with ex -> 
            App.program.onError("Error while restoring model found in app.Properties", ex)

    override this.OnStart() = 
        Console.WriteLine "OnStart: using same logic as OnResume()"
        this.OnResume()
#endif


