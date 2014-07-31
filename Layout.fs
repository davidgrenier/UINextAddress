[<JS>]
module UINextAddress.Layout

open IntelliFactory.WebSharper.Html
        
let ruler() = Hr [Attr.Style "visibility:hidden"]

let pixels x = string (15 * x) + "px"
let spans width e = e |- "width:" + pixels (width - 1) |- "margin-left:" + pixels 1 |- "float:left"
let clears width e = e |- "margin-right:" + pixels width
let section width elements = Div elements |- "width:" + pixels width |- "float:left"