namespace UINextAddress

open IntelliFactory.Html
open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Sitelets

type Action =
    | [<CompiledName "">] Home 

type EntryPoint() =
    inherit Web.Control()

    [<JS>]
    override x.Body = AddressForm.Client.main() :> _

[<Sealed>]
type Website() =
    interface IWebsite<Action> with
        member this.Actions = []
        member this.Sitelet =
            Sitelet.Infer (fun Home ->
                Content.PageContent (fun ctx ->
                    {
                        Page.Default with
                            Title = Some "Piglets vs UI.Next"
                            Body = [Div [new EntryPoint()]]
                            Head = [Link [Rel "stylesheet"; HRef "site.css"]]
                    }
                )
            )

[<assembly: Website(typeof<Website>)>]
do ()