namespace FPlus
open System
open System.Text
open System.Collections.Generic
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open Microsoft.FSharp.Quotations
open System.Threading.Tasks


[<Extension; Sealed>]
type Plus =
    static member inline Invoke (x: 'Plus) (y: 'Plus) : 'Plus =
        let inline call (mthd : ^M, input1 : ^I, input2 : ^I) = ((^M or ^I) : (static member ``+`` : _*_*_ -> _) input1, input2, mthd)
        call (Unchecked.defaultof<Plus>, x, y)
type Plus with 
    static member inline ``+`` ((x1,x2               ), (y1,y2               ), _mthd: Plus) = ( (+) x1 y1, (+) x2 y2                                                                                               ) :'a*'b
    static member inline ``+`` ((x1,x2,x3            ), (y1,y2,y3            ), _mthd: Plus) = ( (+) x1 y1, (+) x2 y2, (+) x3 y3                                                                            ) :'a*'b*'c
    static member inline ``+`` ((x1,x2,x3,x4         ), (y1,y2,y3,y4         ), _mthd: Plus) = ( (+) x1 y1, (+) x2 y2, (+) x3 y3, (+) x4 y4                                                         ) :'a*'b*'c*'d