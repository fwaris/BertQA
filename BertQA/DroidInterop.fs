namespace BertQA
open Xamarin.Forms

type ITextScorer =
    abstract member Score:context:string*query:string->(string*int)option

module DroidInterop =

    let scoreText(ctx,q) =
        let t = DependencyService.Get<ITextScorer>()
        t.Score(ctx,q)

        

