[<AutoOpen; ReflectedDefinition>]
module UINextAddress.Pervasives

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Html
open IntelliFactory.WebSharper.Piglets

type JS = JavaScriptAttribute
type Remote = RemoteAttribute

module Piglets =
    let piglet = Piglet.Do

module Option =
    let ofNull = function
        | null -> None
        | x -> Some x

let (|+) (e: Element) c = e.AddClass c; e
let (|-) (e: Element) s =
    e.GetAttribute "style"
    |> Option.ofNull
    |> Option.map (fun x -> x + ";" + s)
    |> defaultArg <| s
    |> fun result -> e -- Attr.Style result