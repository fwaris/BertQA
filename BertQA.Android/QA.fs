namespace BertQA.Android
open Android.App
open Xamarin.Forms
open Android
open Java.IO
open Java.Nio.Channels
open Xamarin.TensorFlow.Lite
open System
open System.IO
open System.Collections.Generic
open System.Text.RegularExpressions

type Features =
    { 
       InputIds     : int list
       InputMask    : int list
       SegmentIds   : int list
       TokenMap     : IDictionary<int,int>
       Tokens       : (string*int option*int) list
       DebugTokens  : string[]
       OrigTokens   : string list 
    }

type Score =
    {
        Input     : Features
        StartIdxs : (int*float32)[]
        EndIdxs   : (int*float32)[]
    }

type ScoredAnswer =
    {
       Answer   : string
       Logit    : float32
    }

module Scorer =
    let MODEL_PATH = "mobilebert_float_20191023.tflite"
    let DIC_PATH = "vocab.txt"
    let MAX_ANS_LEN = 32
    let MAX_QUERY_LEN = 64
    let MAX_SEQ_LEN = 384
    let DO_LOWER_CASE = true
    let PREDICT_ANS_NUM = 5
    let NUM_LITE_THREADS = 4
    let assets = Application.Context.Assets

    let yourself x = x

    let loadModel() =
        let desc = assets.OpenFd(MODEL_PATH)
        use str = new FileInputStream(desc.FileDescriptor)
        let buff = str.Channel.Map(FileChannel.MapMode.ReadOnly,desc.StartOffset,desc.DeclaredLength)
        let opts = 
            (new Interpreter.Options())
             .SetNumThreads(NUM_LITE_THREADS)
        new Interpreter(buff,opts)

    let mutable model = lazy(loadModel())

    let unloadModel() = 
        if model.IsValueCreated then 
            model.Value.Dispose()           //model takes a large amount of memory, so unload if not in the app
            model <- lazy(loadModel())

    let loadDictionary() =
        seq {
            use str = new StreamReader(assets.Open DIC_PATH)
            let mutable line = null
            while (line <- str.ReadLine(); line <> null) do    
            yield line}
        |> Seq.mapi (fun i x -> x,i)
        |> dict

    let loadTestDictionary() =
        seq {
            use str = new StreamReader(@"C:\s\Repos\BertQA\BertQA\BertQA.Android\Assets\vocab.txt")
            let mutable line = null
            while (line <- str.ReadLine(); line <> null) do    
            yield line}
        |> Seq.mapi (fun i x -> x,i)
        |> dict
        
    
    //let vocab = lazy(loadTestDictionary())
    let vocab = lazy(loadDictionary())
    let asStr ls = new String(ls |> List.rev |> List.toArray)

    module Tokenizer =
        let isControl c = Char.IsControl c
        let isWS c      = Char.IsWhiteSpace c  //seems to cover unicode  Zs, \u2028, \u2029
        let isValid c   = c >= Char.MinValue && c <= Char.MaxValue //not sure we need this
        let isPunc c    = Char.IsPunctuation c

        let rec s_main accW acc = function
            | []                          -> (accW::acc) |> List.filter (List.isEmpty>>not) |> List.map asStr |> List.rev
            | c::rest when isWS c         -> s_ws   (accW::acc) rest
            | c::rest when isControl c    -> s_main accW acc rest                    //skip
            | c::rest when not(isValid c) -> s_main accW acc rest                    //skip
            | c::rest when isPunc c       -> s_main [] ([c]::accW::acc) rest         //punctuation is its own token
            | c::rest                     -> s_main (c::accW) acc rest               //accumulate token
        and s_ws acc = function
            | []                          -> s_main [] acc []
            | c::rest when isWS c         -> s_ws   acc rest                         //skip
            | ls                          -> s_main [] acc ls

        let tokenize (str:string) =
            let str = str.ToLower() |> Seq.toList
            s_main [] [] str


    module WordTokenizer =
        [<Literal>]
        let UNK = "[UNK]"

        let MAX_INPUTCHARS_PER_WORD = 200

        let (|Match|_|) x = 
            let s = asStr x
            if vocab.Value.TryGetValue(asStr x) |> fst then 
                Some s
            else 
                None

        let rec findGreedy rem  = function
            | []             -> None
            | '#'::'#'::[]   -> None
            | Match s        -> Some(s,rem)
            | x::rest        -> findGreedy (x::rem) rest

        let rec findPieces addHash allPieces chars =
            if List.length chars = 0 then 
                allPieces |> List.rev
            else
                let revChars = (if addHash then '#'::'#'::chars else chars) |> List.rev
                match findGreedy [] revChars with 
                | Some(wordPiece,rest)  -> findPieces true (wordPiece::allPieces)  rest
                | None                  -> allPieces |> List.rev

        let wordPieces (token:string) = 
            if token.Length > MAX_INPUTCHARS_PER_WORD then
                [UNK]
            else
                match findPieces false [] (Seq.toList token) with 
                | [] -> [UNK] 
                | xs -> xs

    module Featurizer =
        let CLS = "[CLS]"
        let SEP = "[SEP]"

        let padZero len ls = seq {yield! ls; while true do yield 0} |> Seq.truncate len |> Seq.toList

        let seg0 = 0
        let seg1 = 1

        let toFeatures query context =

            let queryTokens = 
                query 
                |> Tokenizer.tokenize 
                |> List.collect WordTokenizer.wordPieces
                |> List.truncate MAX_QUERY_LEN
                |> List.map (fun x -> x,None,seg0)

            let ctxOrigTokens = Regex.Split(context, @"\s+")  |> Array.toList 
            
            let ctxTokens =
                ctxOrigTokens
                |> List.mapi (fun i t -> t,i)
                |> List.collect (fun (t,i) -> 
                    t
                    |> Tokenizer.tokenize
                    |> List.map WordTokenizer.wordPieces
                    |> List.collect (fun wts -> wts |> List.map (fun wt -> wt, Some i, seg1)))

            let allTokens =
                seq{
                    CLS,  None, seg0
                    yield! queryTokens
                    SEP, None, seg0
                    yield! ctxTokens
                }
                |> Seq.truncate  (MAX_SEQ_LEN - 1)
                |> fun xs -> Seq.append xs [SEP, None, seg1]
                |> Seq.toList

            let inputIds   = allTokens |> List.map (fun (t,_,_) -> vocab.Value.[t]) |> padZero MAX_SEQ_LEN 
            let inputMask  = allTokens |> List.map (fun __ -> 1)                    |> padZero MAX_SEQ_LEN
            let segmentIds = allTokens |> List.map (fun (_,_,s) -> s)               |> padZero MAX_SEQ_LEN

            let tknOrigMap = 
                allTokens 
                |> List.mapi(fun j (_,oi,_) -> j,oi) 
                |> List.choose (fun (j,oi)->oi |> Option.map(fun i -> j,i)) 
                |> dict

            { 
               InputIds     = inputIds
               InputMask    = inputMask
               SegmentIds   = segmentIds
               TokenMap     = tknOrigMap
               Tokens       = allTokens
               OrigTokens   = ctxOrigTokens 
               DebugTokens  = allTokens |> List.map (fun (x,_,_) -> x) |> List.toArray
            }
    (*
    let vtst = vocab.Value.["k"]
    let test1 = WordTokenizer.findGreedy [] ("judgemental" |> Seq.toList |> List.rev)
    let test2 = WordTokenizer.findPieces false [] ("judgemental" |> Seq.toList)
    let test3 = WordTokenizer.wordPieces "don't be so judgemental"
    let strTest = 
        """
        The S&P 500 is no kl longer in a bear market either. It's 14% from its record high. And one market strategist 
        has a bold prediction. Brett Ewing, chief market strategist at First Franklin Financial Services, says the blue 
        chip index will continue rallying and hit a new all-time high of 3,500 before the presidential election. 
        The S&P 500 peaked at just under 3,400 on February 19.
        """
    let strTest2 =
        """
        Ewing is bullish because he thinks that the number of people filing for jobless claims every week has peaked 
        and that investors will continue to flock to the big tech stocks like Apple (AAPL), Microsoft (MSFT), 
        Amazon (AMZN), Google owner Alphabet (GOOGL) and Facebook (FB) that dominate the S&P 500.
        """
    let tokens = Tokenizer.tokenize strTest
    let encoded = tokens |> List.map WordTokenizer.wordPieces |> List.iter (printfn "%A")
    *)

    let testContext1 = 
        """
           Super Bowl 50 was an American football game to determine the champion of the National 
           Football League (NFL) for the 2015 season. The American Football Conference (AFC) champion 
           Denver Broncos defeated the National Football Conference (NFC) champion Carolina Panthers 24\u201310 to earn 
           their third Super Bowl title. The game was played on February 7, 2016, at Levi's Stadium in the 
           San Francisco Bay Area at Santa Clara, California. As this was the 50th Super Bowl, the league 
           emphasized the \"golden anniversary\" with various gold-themed initiatives, as well as temporarily 
           suspending the tradition of naming each Super Bowl game with Roman numerals (under which the game 
           would have been known as \"Super Bowl L\"), so that the logo could prominently feature the 
           Arabic numerals 50."""

    let testQ11 = "Which NFL team represented the AFC at Super Bowl 50?"
    let testQ12 = "Which NFL team represented the NFC at Super Bowl 50?"
    let testQ13 = "Where did Super Bowl 50 take place?"

    let testContext2 = 
        """
        The Normans (Norman: Nourmands; French: Normands; Latin: Normanni) were the people 
        who in the 10th and 11th centuries gave their name to Normandy, a region in France. 
        They were descended from Norse ("Norman" comes from "Norseman") raiders and pirates 
        from Denmark, Iceland and Norway who, under their leader Rollo, agreed to swear fealty 
        to King Charles III of West Francia. Through generations of assimilation and mixing with 
        the native Frankish and Roman-Gaulish populations, their descendants would gradually merge 
        with the Carolingian-based cultures of West Francia. The distinct cultural and ethnic identity 
        of the Normans emerged initially in the first half of the 10th century, and it continued to evolve 
        over the succeeding centuries.
        """
    let testQ21 = "In what country is Normandy located?"
    let testQ22 = "When were the Normans in Normandy?"
    let testQ23 = "From which countries did the Norse originate?"
    let testQ24 = "Who was the Norse leader?"
    let testQ25 = "What century did the Normans first gain their separate identity?"
 
    //let input = Featurizer.toFeatures testQuery1 testContext

    let topN xs = xs |> Seq.mapi(fun i x -> i,x) |> Seq.sortByDescending snd |> Seq.truncate  PREDICT_ANS_NUM

    let score text query =
        let input = Featurizer.toFeatures query text

        let endLgtsIdx  = new Java.Lang.Integer(0)
        let strtLgtsIdx = new Java.Lang.Integer(1)

        let inputIds    = [| input.InputIds   |> List.toArray      |]
        let inputMask   = [| input.InputMask  |> List.toArray      |]
        let segmentIds  = [| input.SegmentIds |> List.toArray      |]
        let startLogits = [| Array.create MAX_SEQ_LEN 0.0f         |]
        let endLogits   = [| Array.create MAX_SEQ_LEN 0.0f         |]

        let inputIdsJ    = Java.Lang.Object.FromArray<int[]>(inputIds)
        let inputMaskJ   = Java.Lang.Object.FromArray<int[]>(inputMask)
        let segmentIdsJ  = Java.Lang.Object.FromArray<int[]>(segmentIds)
        let startLogitsJ = Java.Lang.Object.FromArray<float32[]>(startLogits)
        let endLogitsJ   = Java.Lang.Object.FromArray<float32[]>(endLogits)

        let inputs = [|inputIdsJ; inputMaskJ; segmentIdsJ |]
        let output = [endLgtsIdx,endLogitsJ; strtLgtsIdx, startLogitsJ] |> dict
        model.Value.RunForMultipleInputsOutputs(inputs,output)
        let stJ = output.[strtLgtsIdx]
        let edJ = output.[endLgtsIdx]
        let st = stJ.ToArray<float32[]>()
        let ed = edJ.ToArray<float32[]>()
        let startIdxs = topN st.[0] |> Seq.toArray
        let endIdxs   = topN ed.[0] |> Seq.toArray
        {
            Input     = input
            StartIdxs = startIdxs         //best starting positions for answer
            EndIdxs   = endIdxs           //best ending positions for answer
        }


    let toAnswers score =
         [for (s,sL) in score.StartIdxs do 
            for (e,eL) in score.EndIdxs do
                if s > 0 
                   && s <= e 
                   && score.Input.TokenMap.ContainsKey s
                   && score.Input.TokenMap.ContainsKey e
                   && e - s < MAX_ANS_LEN
                   then
                   let startT = score.Input.TokenMap.[s]
                   let endT   = score.Input.TokenMap.[e]
                   let ans = System.String.Join(" ",score.Input.OrigTokens.[startT .. endT])
                   let scrAns = {Answer=ans; Logit=sL+eL}
                   yield scrAns]

        

type TextScorer() =
    interface BertQA.ITextScorer  with 
        member x.Score(context,query) = 
            let scr = Scorer.score context query
            Scorer.toAnswers scr |> List.sortByDescending (fun x->x.Logit) |> List.tryHead  |> Option.map(fun a->a.Answer)
            
            //let scr1 = Scorer.score Scorer.testContext2 Scorer.testQ21
            //let ans1 = Scorer.toAnswers scr1 |> List.sortByDescending (fun x->x.Logit) |> List.head
            //let scr2 = Scorer.score Scorer.testContext2 Scorer.testQ22
            //let ans2 = Scorer.toAnswers scr2 |> List.sortByDescending (fun x->x.Logit) |> List.head
            //let scr3 = Scorer.score Scorer.testContext2 Scorer.testQ23
            //let ans3 = Scorer.toAnswers scr3 |> List.sortByDescending (fun x->x.Logit) |> List.head
            //let scr4 = Scorer.score Scorer.testContext2 Scorer.testQ24
            //let ans4 = Scorer.toAnswers scr4 |> List.sortByDescending (fun x->x.Logit) |> List.head

    end

[<assembly:Dependency(typeof<TextScorer>)>]

do 
    ()
