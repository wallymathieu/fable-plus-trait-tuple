module ExtensionsTests

open FPlus
open Testing
open System.Collections.Generic


let ExtensionsTest = 


    testList "Extension Tests" [

      testCase "semigroups 1"
        (fun () ->
            let lzy1 = Plus.Invoke (lazy [1]) (lazy [2;3])
            let asy1 = Plus.Invoke (async.Return [1]) (async.Return [2;3])
            
            let mapA =
                Map.empty
                |> Map.add 1 (lazy "Hey")
                |> Map.add 2 (lazy "Hello")

            let mapB =
                Map.empty
                |> Map.add 3 (lazy " You")
                |> Map.add 2 (lazy " World")

            let mapAB = Plus.Invoke mapA mapB
            
            //let tup22 = ( 2) ++ ( 3) // compiles
            let tup22 = Plus.Invoke (1, 2) (2, 3) //does not compile
            let tup23 = Plus.Invoke (Some 1,Some 2) (Some 2,Some 3)
            let tup2 = Plus.Invoke ([1;2], [|1;2|]) ([3;4], [|3;4|])
            let tup2Some =Plus.Invoke ([1;2], Some 1) ([3;4], Some 2)
            let tup3 = Plus.Invoke ([1;2], [|1;2|], 1) ([3;4], [|3;4|], 2) // <-- causes Fable Cannot resolve trait call +
            let tup3Some = Plus.Invoke ([1;2], [|1;2|], Some 1) ([3;4], [|3;4|], Some 2)

            equal ([1; 2; 3; 4], [|1; 2; 3; 4|]) tup2

            equal ([1; 2; 3; 4], Some 3) tup2Some

            equal [1;2;3] lzy1.Value
            // equal [1;2;3] (Async.RunSynchronously asy1)  <-- weird runSynchronously error
            // equal [(1, "Hey"); (2, "Hello World"); (3, " You")] (Map.toList mapAB |> List.map (fun (x, y) -> (x, y.Value))) <-- gets " World" instead of "Hellp World" ???
            equal ([1; 2; 3; 4], [|1; 2; 3; 4|], 3) tup3
            equal ([1; 2; 3; 4], [|1; 2; 3; 4|], Some 3) tup3Some
        )

]
