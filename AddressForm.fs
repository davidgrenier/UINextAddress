module UINextAddress.AddressForm

type Country = { CountryCode: string; Name: string }
type Subdivision = { Code: string; Name: string }

let countries, subdivisions =
    let data = FSharp.Data.XmlProvider<"Data.xml">.Load "Data.xml"
    let countries =
        data.Countries
        |> Array.choose (fun c ->
            c.Notation.String
            |> Option.map (fun code -> { CountryCode = code; Name = c.Label })
        )
        |> Array.sortBy (fun x -> x.Name)

    let subdivisions =
        data.Features
        |> Seq.choose (fun sub ->
            match sub.Notation.Split '-' with
            | [|countryCode; code|] -> Some (countryCode, { Code = code; Name = sub.Label })
            | _ -> None
        )
        |> Seq.groupBy fst
        |> Seq.choose (
            let countries = Seq.map (fun c -> c.CountryCode, c) countries |> Map.ofSeq
            fun (countryCode, subdivisions) ->
                countries.TryFind countryCode
                |> Option.map (fun country ->
                    let subdivisions =
                        subdivisions
                        |> Seq.map snd
                        |> Seq.sortBy (fun x -> x.Name)
                        |> Seq.toArray
                    country, subdivisions
                )
        )
        |> Map.ofSeq

    countries, subdivisions

type Address =
    {
        FirstName: string
        LastName: string
        Address1: string
        Address2: string
        Country: Country
        Subdivision: Subdivision
        City: string
        PostalCode: string
        PhoneNumber: string
    }
    
[<Remote>]
let getCountries() = countries

[<Remote>]
let getSubdivisions country = subdivisions.[country]

[<JS>]
let sample =
    {
        FirstName = "David"
        LastName = ""
        Address1 = "12 Unknown"
        Address2 = ""
        Country = { CountryCode = "CA"; Name = "Canada" }
        Subdivision = { Code = "QC"; Name = "Broken" }
        City = "Montreal"
        PostalCode = ""
        PhoneNumber = ""
    }

[<JS>]
module Validate =
    open IntelliFactory.WebSharper.UI.Next

    let (|Success|Failure|) x : Choice<_, string> = id x

    let succeed x = Choice1Of2 x
    let fail (x: string) = Choice2Of2 x

    module V =
        let is f err =
            View.FromVar
            >> View.Map (function x when f x -> succeed x | _ -> fail err)

        let notBlank = is ((<>) "")

[<JS>]
module Next =
    open IntelliFactory.WebSharper
    open IntelliFactory.WebSharper.UI.Next
    open IntelliFactory.WebSharper.UI.Next.Html

    open Notation
    open Validate
    module L = Layout

    let lbl x = Doc.Element "label" x
    let style = Attr.Style
    let var = Var.Create

    let spans width =
        [
            style "width" <| L.pixels (width - 1)
            style "margin-left" (L.pixels 1)
            style "float" "left"
        ]
    
    let input field = Doc.Input (spans 12) field
    let label text = lbl (spans 7) [Doc.TextNode text]
    
    let errorSpans width = style "color" "red" :: spans width

    let showError =
        View.Map (function
            | Success _ -> lbl (spans 15) []
            | Failure err -> lbl (errorSpans 15) [Doc.TextNode err]
        )
        >> Doc.EmbedView

    let main id =
        let firstName = var sample.FirstName
        let lastName = var sample.LastName
        let address1 = var sample.Address1
        let address2 = var sample.Address2
        let country = var sample.Country
        let subdivision = var sample.Subdivision
        let city = var sample.City
        let postalCode = var sample.PostalCode

        let notEmptyField name field =
            [
                label name
                input field
                field |> V.notBlank (name + " is mandatory") |> showError
            ]
            |> Doc.Concat

        let combined =
            View.Const (fun fn ln a1 a2 country sub city pc pn ->
                {
                    FirstName = fn
                    LastName = ln
                    Address1 = a1
                    Address2 = a2
                    Country = country
                    Subdivision = sub
                    City = city
                    PostalCode = pc
                    PhoneNumber = pn
                }
            )
            <*> firstName.View
            <*> lastName.View
            <*> address1.View
            <*> address2.View
            <*> country.View
            <*> subdivision.View
            <*> city.View
            <*> postalCode.View
        
        [
            Doc.Element "hr" [style "visibility" "hidden"] []

            notEmptyField "First Name" firstName
            notEmptyField "Last Name" lastName
            notEmptyField "Address 1" address1
            
            label "Address 2"
            input address2
            
            label "City"
            input city
            
            label "Postal Code"
            input postalCode
        ]
        |> Doc.Concat
        |> Doc.RunById id

[<JS>]
module Client =
    open IntelliFactory.WebSharper
    open IntelliFactory.WebSharper.Html
    open Piglets

    module L = Layout
    module V = Piglet.Validation

    let countries =
        getCountries()
        |> Array.map (fun x -> x, x.Name)

    let regionPiglet country subdivision =
        piglet {
            let! country = Piglet.Yield country
            let subdivisions = getSubdivisions country
            return!
                Piglet.Yield subdivision
                |> Piglet.MapViewArgs (fun sub -> sub, subdivisions)
                |> V.Is (fun x -> Array.exists ((=) x) subdivisions) "Pick a valid country subdivision"
                |> Piglet.TransmitReader
                |> Piglet.Map (fun sub -> country, sub)
        }
        
    let (<*!>) p value = p <*> Piglet.Yield value
    let (/>) field value = Piglet.Yield value |> V.Is V.NotEmpty (field + " is mandatory") |> Piglet.TransmitReader

    let addressPiglet a =
        Piglet.Return (fun fn ln a1 a2 (country, sub) city pc pn ->
            {
                FirstName = fn
                LastName = ln
                Address1 = a1
                Address2 = a2
                Country = country
                Subdivision = sub
                City = city
                PostalCode = pc
                PhoneNumber = pn
            }
        )
        <*> "First Name" /> a.FirstName 
        <*> "Last Name" /> a.LastName
        <*> "Address 1" /> a.Address1
        <*!> a.Address2
        <*> regionPiglet a.Country a.Subdivision
        <*!> a.City
        <*!> a.PostalCode
        <*!> a.PhoneNumber
        
    let error reader =
        Label [Attr.Style "color:red"]
        |> Controls.ShowErrors reader (fun err ->
            [Text (String.concat "," err)]
        )
        |> L.spans 15
        
    let cleared x = L.clears 15 x
    let label name = Label [Text name] |> L.spans 7
    let input reader = Controls.Input reader |> L.spans 12
    let clearedInput = input >> cleared
    let select reader = Controls.Select reader >> L.spans 12
    
    let main () =
        addressPiglet sample
        |> Piglet.Render (fun fn fnErr ln lnErr a1 a1Err a2 regionChoice city pc number ->
            Div [
                [
                    L.ruler()

                    label "First Name"
                    input fn
                    error fnErr

                    label "Last Name"
                    input ln
                    error lnErr

                    label "Address 1"
                    input a1
                    error a1Err

                    label "Address 2"
                    clearedInput a2
                ]
                @ regionChoice.Chooser (fun country ->
                    [
                        label "Country"
                        select country countries |> cleared
                    ]
                )
                @ [
                    label "Subdivision"
                    Div []
                    |> Controls.RenderChoice regionChoice (fun (sub, subdivisions) result ->
                        Div [
                            subdivisions
                            |> Array.map (fun x -> x, x.Name)
                            |> select sub

                            error result
                        ]
                    )

                    label "City"
                    clearedInput city

                    label "Postal Code"
                    clearedInput pc
                ]
                |> L.section 35
                |- "border-right:aliceblue solid"

                L.section 34 [Attr.Id "next"]
                |>! OnAfterRender (fun e -> Next.main e.Id)
            ]
        )