namespace FPlus
open System
open System.Text
open System.Collections.Generic
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open Microsoft.FSharp.Quotations
open System.Threading.Tasks

type Default6 = class end
type Default5 = class inherit Default6 end
type Default4 = class inherit Default5 end
type Default3 = class inherit Default4 end
type Default2 = class inherit Default3 end
type Default1 = class inherit Default2 end
#if FABLE_COMPILER
exception AggregateException of Exception seq
#endif
type Id0 (v: string) =
   let value = v
   member __.getValue = value

[<Extension; Sealed>]
type Plus =     
    inherit Default1
    static member inline ``+`` (x: 'Plus             , y: 'Plus             ,             _mthd: Default2) = (^Plus :  (static member (<|>) : _*_ -> _) x, y) : ^Plus

    static member inline ``+`` (x: 'Plus             , y: 'Plus             , _mthd: Default1) = x + y : ^Plus
    static member inline ``+`` (_: ^t when ^t: null and ^t: struct, _: ^t   , _mthd: Default1) = id

    static member        ``+`` (x: list<_>           , y                    , _mthd: Plus    ) = x @ y
    static member        ``+`` (x: array<_>          , y                    , _mthd: Plus    ) = Array.append x y
    static member        ``+`` (()                   , ()                   , _mthd: Plus    ) = ()
    static member        ``+`` (x: bool              , y: bool              , _mthd: Plus    ) = x <> y
    static member        ``+`` (x: Set<_>            , y                    , _mthd: Plus    ) = Set.union x y
    
    #if !FABLE_COMPILER
    static member        ``+`` (x: StringBuilder     , y: StringBuilder     , _mthd: Plus    ) = StringBuilder().Append(x).Append(y)
    static member        ``+`` (_: Id0               , _: Id0               , _mthd: Plus    ) = Id0 ""    
    //static member        ``+`` (x: AggregateException, y: AggregateException, _mthd: Plus    ) = new AggregateException (seq {yield! x.InnerExceptions; yield! y.InnerExceptions})
    static member        ``+`` (x: exn               , y: exn               , _mthd: Plus    ) =
        let f (e: exn) = match e with :? AggregateException as a -> a.InnerExceptions :> seq<_> | _ -> Seq.singleton e
        new AggregateException (seq {yield! f x; yield! f y}) :> exn
    #else
    static member        ``+`` (x: StringBuilder     , y: StringBuilder     , _mthd: Plus    ) = StringBuilder().Append(string x).Append(string y)
    static member        ``+`` (_: Id0               , _: Id0               , _mthd: Plus    ) = Id0 ""
    static member        ``+`` (x: exn               , y: exn               , _mthd: Plus    ) =
        let f (e: exn) = match e with :? AggregateException as a -> a.Data0 :> seq<_> | _ -> Seq.singleton e
        AggregateException (seq {yield! f x; yield! f y}) :> exn
    #endif
    
    static member inline Invoke (x: 'Plus) (y: 'Plus) : 'Plus =
        let inline call (mthd : ^M, input1 : ^I, input2 : ^I) = ((^M or ^I) : (static member ``+`` : _*_*_ -> _) input1, input2, mthd)
        call (Unchecked.defaultof<Plus>, x, y)

type Plus with
    static member inline ``+`` (x: option<_>, y, _mthd: Plus) =
                    match x, y with
                    | (Some a , Some b) -> Some (Plus.Invoke a b)
                    | (Some a , None  ) -> Some a
                    | (None   , Some b) -> Some b
                    | _                 -> None


type Plus with 

    static member inline ``+`` ((x1,x2               ), (y1,y2               ), _mthd: Plus) = (Plus.Invoke x1 y1, Plus.Invoke x2 y2                                                                                               ) :'a*'b
    static member inline ``+`` ((x1,x2,x3            ), (y1,y2,y3            ), _mthd: Plus) = (Plus.Invoke x1 y1, Plus.Invoke x2 y2, Plus.Invoke x3 y3                                                                            ) :'a*'b*'c
    static member inline ``+`` ((x1,x2,x3,x4         ), (y1,y2,y3,y4         ), _mthd: Plus) = (Plus.Invoke x1 y1, Plus.Invoke x2 y2, Plus.Invoke x3 y3, Plus.Invoke x4 y4                                                         ) :'a*'b*'c*'d
    static member inline ``+`` ((x1,x2,x3,x4,x5      ), (y1,y2,y3,y4,y5      ), _mthd: Plus) = (Plus.Invoke x1 y1, Plus.Invoke x2 y2, Plus.Invoke x3 y3, Plus.Invoke x4 y4, Plus.Invoke x5 y5                                      ) :'a*'b*'c*'d*'e
    static member inline ``+`` ((x1,x2,x3,x4,x5,x6   ), (y1,y2,y3,y4,y5,y6   ), _mthd: Plus) = (Plus.Invoke x1 y1, Plus.Invoke x2 y2, Plus.Invoke x3 y3, Plus.Invoke x4 y4, Plus.Invoke x5 y5, Plus.Invoke x6 y6                   ) :'a*'b*'c*'d*'e*'f
    static member inline ``+`` ((x1,x2,x3,x4,x5,x6,x7), (y1,y2,y3,y4,y5,y6,y7), _mthd: Plus) = (Plus.Invoke x1 y1, Plus.Invoke x2 y2, Plus.Invoke x3 y3, Plus.Invoke x4 y4, Plus.Invoke x5 y5, Plus.Invoke x6 y6, Plus.Invoke x7 y7) :'a*'b*'c*'d*'e*'f*'g

type Plus with

    static member inline ``+`` (x: Dictionary<'Key,'Value>, y: Dictionary<'Key,'Value>, _mthd: Plus) =
                    let d = Dictionary<'Key,'Value> ()
                    let plus = OptimizedClosures.FSharpFunc<_,_,_>.Adapt Plus.Invoke
                    for KeyValue(k, v ) in x do d.[k] <- v
                    for KeyValue(k, v') in y do d.[k] <- match d.TryGetValue k with true, v -> plus.Invoke (v, v') | _ -> v'
                    d

    static member inline ``+`` (f: 'T->'Monoid, g: 'T->'Monoid, _mthd: Plus) = (fun x -> Plus.Invoke (f x) (g x)) : 'T->'Monoid
   

    static member inline ``+`` (x: 'a Lazy                   , y: 'a Lazy                   , _mthd: Plus    ) = lazy Plus.Invoke x.Value y.Value
    


