// Copyright 2018-2019 Fabulous contributors. See LICENSE.md for license.
namespace BertQA

open System
open System.Diagnostics
open Fabulous
open Fabulous.XamarinForms
open Fabulous.XamarinForms.LiveUpdate
open Xamarin.Forms
open System.Reflection
open System.IO

type ContextQ =
    {
        Context : string
        Queries : string []
        Title   : string
    }

type AppPages = HomePage | QAPage

type Model = 
  { 
    Contexts        : ContextQ []
    Score           : (string*int) option
    ScoreRequested  : bool
    ShowDetail      : bool
    Page            : AppPages
    CurrItem        : int
    }

module Model =
    
    let ctxJson() =
        let assm = Assembly.GetExecutingAssembly()
        let resName = assm.GetManifestResourceNames() |> Seq.find (fun n -> n.EndsWith("contextq.json"))
        use ms = assm.GetManifestResourceStream(resName)
        use str = new StreamReader(ms)
        let jstr = str.ReadToEnd()
        FsJson.parse jstr
    
    let initModel() =
        let j = ctxJson()
        let cq = 
            j.Array
            |> Array.map (fun cj -> 
                {
                    Context = cj?content.Val
                    Title   = cj?title.Val.Replace("_"," ")
                    Queries = cj?questions.Array |> Array.map (fun x->x.Val)
                }
            )
        {
            Contexts = cq
            Score     = None
            ScoreRequested = false
            ShowDetail  = false
            Page = HomePage
            CurrItem = 0
        }



