module DigitalCupons.View

open Suave.Form
open Suave.Html
open System.Net
open DigitalCupons.XmlProvider
open DigitalCupons.Form

let resources = AutocompleteItems.Load("autocompleteItems.xml")

let em s = tag "em" [] [Text s]
let cssLink href = link [ "href", href; " rel", "stylesheet"; " type", "text/css" ]
let h2 s = tag "h2" [] [Text s]
let ul nodes = tag "ul" [] nodes
let ulAttr attr nodes = tag "ul" attr nodes
let li = tag "li" []
let table x = tag "table" [] x
let th x = tag "th" [] x
let tr x = tag "tr" [] x
let td x = tag "td" [] x
let strong s = tag "strong" [] (text s)

let form x = tag "form" ["method", "POST"] x
let formInput = Suave.Form.input
let submitInput value = input ["type", "submit"; "value", value]


type UnityOfMeasures = Pezzo | Kilo | Etto | Pacco

let UnityOfMeasuresCombo = [("Pezzo","Pezzo");("Kilo","Kilo");("Etto","Etto");("Confezione","Confezione")]

type Field<'a> = {
    Label : string
    Html : Form<'a> -> Suave.Html.Node
}

type Fieldset<'a> = {
    Legend : string
    Fields : Field<'a> list
}

type FormLayout<'a> = {
    Fieldsets : Fieldset<'a> list
    SubmitText : string
    Form : Form<'a>
}

let renderForm (layout : FormLayout<_>) =    

    form [
        for set in layout.Fieldsets -> 
            tag "fieldset" [] [
                yield tag "legend" [] [Text set.Legend]

                for field in set.Fields do
                    yield div ["class", "editor-label"] [
                        Text field.Label
                    ]
                    yield div ["class", "autocomplete"] [
                        field.Html layout.Form
                    ]
            ]

        yield submitInput layout.SubmitText
    ]

let changeMyPassword =
    [
        h2 "cambio password"
        renderForm
            { Form = Form.changePassword
              Fieldsets = 
                  [ { Legend = "Informazioni "
                      Fields = 
                          [ { Label = "Vecchia password"
                              Html = formInput (fun f -> <@ f.OldPassword @>) [] }
                            { Label = "Nuova Password"
                              Html = formInput (fun f -> <@ f.NewPassword @>) [] } 
                            { Label = "Conferma nuova passwod"
                              Html = formInput (fun f -> <@ f.ConfirmNewPassword @>) [] } 

                          ] 
                      } 
                  ]
              SubmitText = "aggiorna" }
    ]


let askForEmailInMakingOrderAsOngoing (order: LiteDb.Order) =
    [
        h2 "conferma invio mail di notifica inizio lavoro"
    ]


let editUser (user: LiteDb.User) =
    [
        h2 "modifica utente"
        br []
        (Text("username: "+user.UserName))
        br []
        renderForm
            { 
              Form = Form.modifyUser
              Fieldsets = 
                  [ { 
                      Legend = "Informazioni "
                      Fields = 
                          [ 
                            { 
                              Label = "Email"
                              Html = formInput (fun f -> <@ f.Email @>) ["value",match user.UserEmail with Some X -> X| _ -> ""] 
                            }
                            { 
                              Label = "Phone number"
                              Html = formInput (fun f -> <@ f.PhoneNumber @>) ["value",match user.UserPhone with Some X -> X | _ -> ""] 
                            }
                            { 
                              Label = "Address"
                              Html = formInput (fun f -> <@ f.Address @>) ["value", match user.Address with Some X -> X | _ -> ""] 
                            }
                          ] 
                      } 
                   ]
              SubmitText = "Inserisci" 
            }

    ]

let createNewAccount message =
    [
        h2 "inserimento nuovo utente"
        Text(message)
        renderForm
            { 
              Form = Form.registerUser
              Fieldsets = 
                  [ { 
                      Legend = "Informazioni "
                      Fields = 
                          [ 
                            { 
                              Label = "User Name"
                              Html = formInput (fun f -> <@ f.Username @>) [] 
                            }
                            { 
                              Label = "Email"
                              Html = formInput (fun f -> <@ f.Email @>) [] 
                            }
                            { 
                              Label = "Phone number"
                              Html = formInput (fun f -> <@ f.PhoneNumber @>) [] 
                            }
                            { 
                              Label = "Address"
                              Html = formInput (fun f -> <@ f.Address @>) [] 
                            }
                            { 
                              Label = "Password"
                              Html = formInput (fun f -> <@ f.Password @>) [] 
                            } 
                            { 
                              Label = "Confirm Password"
                              Html = formInput (fun f -> <@ f.ConfirmPassword @>) [] 
                            } 
                          ] 
                      } 
                   ]
              SubmitText = "Inserisci" 
            }
    ]


let detailedMenu (order: LiteDb.Order)(availableCourses: LiteDb.Courses list) (orderitems: LiteDb.OrderItem list) (mapImagesForCourses: Map<int, string option>) currentPage availableCourseCount keySearch = 
    let totalPages = availableCourseCount/Utils.ITEMS_PER_PAGE
    let total = orderitems |>  List.map (fun x -> x.Course.Price*(decimal)x.ItemQuantity) |> List.sum
    [
        h2 "contenuti carrello"
        br []
        table 
            [
                for orderItem in orderitems ->
                    let (printStringFormat:Path.StringDecimal) = match orderItem.Course.UnityOfMeasure with | LiteDb.Pezzo -> "  %s: %.0f" | _ -> "  %s:  %.2f"
                    tr [
                        td [
                            (Text(orderItem.Course.Name))
                        ]
                        td [
                            (Text(match (orderItem.Comment) with Some X -> "\""+X+"\"" | _ -> ""))
                        ]

                        td [
                                (Text(sprintf  printStringFormat (orderItem.Course.UnityOfMeasure.ToString())   (orderItem.ItemQuantity)))
                        ]
                        td [
                            (Text(sprintf "€: %.2f " (orderItem.Course.Price*((decimal)(orderItem.ItemQuantity)))))
                        ]
                        td [
                            (a (sprintf Path.Orders.decreaseOrderItemQuantity orderItem.Id currentPage)) ["class","buttonB"] [Text("  -  ")]
                        ]
                        td [
                            (a (sprintf Path.Orders.increaseOrderItemQuantity orderItem.Id currentPage)) ["class","buttonB"] [Text("  +  ")]
                        ]
                        td [
                            (a (sprintf Path.Orders.editOrderItemFromMenuItem orderItem.Id)) ["class","buttonX"] [Text(" modifica ")]
                        ]
                        td [
                            (a (sprintf Path.Orders.removeOrderItemFromCompleteMenu orderItem.Id currentPage)) ["class","buttonX"] [Text(" rimuovi ")]
                        ]
                    ]
            ]
        br []
        table [
            tr [
                td [
                    Text(sprintf "totale €:  %.2f" total )
                ]
            ]
        ]

        br []
        br []

        Text("ricerca")
        br []
        renderForm 
            {
                Form = Form.nameSearch
                Fieldsets =
                    [   {
                            Legend = "ricerca"
                            Fields =
                            [
                                {
                                    Label = "nome"
                                    Html = formInput (fun f -> <@ f.Name @>) []
                                }
                            ]
                        }
                    ]
                SubmitText = "cerca"
            }
        

        h2 "scegli tra i seguenti:"




        table
            [
                for menuItem in availableCourses ->
                tr [
                    td [
                        (Text(menuItem.Name))
                    ]
                    td [(match mapImagesForCourses.[menuItem.Id] with
                            Some X -> (tag "img" [("src","data:image/bmp;base64, "+X);("width","100")][])
                            | None -> (em "no image")
                        )
                    ]

                    // td [(match menuItem.EncodedImage  with
                    //         Some X -> (tag "img" [("src","data:image/bmp;base64, "+X);("width","100")][])
                    //         | None -> (em "no image")
                    //     )
                    // ]

                    td [
                        (Text(match menuItem.Description with Some X -> " ("+ X + " ) " | _ -> ""))
                    ]
                    td [
                        (Text(menuItem.UnityOfMeasure.ToString()))
                    ]
                    td [
                        (Text(sprintf "€: %.2f" menuItem.Price))
                    ]

                    td [
                        (a (sprintf Path.Orders.addItemFromDetailedMenu order.Id menuItem.Id currentPage)) ["class","buttonX"] [Text("aggiungi")]
                    ]
                   ]
            ]
        (match currentPage with 
            | X when X > 0 -> (a (sprintf Path.Orders.completeMenuViewPaged order.Id (currentPage - 1)) ["class","buttonX"] [Text("<")])
            | _ -> em ""
        )
        (Text(sprintf "pagina %d di %d" currentPage totalPages))
        (match currentPage with 
            | X when X<(totalPages)  -> (a (sprintf Path.Orders.completeMenuViewPaged order.Id (currentPage + 1)) ["class","buttonX"] [Text(">")])
            | _ -> em ""
        
        )
        
        br []

        (match (List.length orderitems) with
            | X when X > 0  -> a (sprintf Path.Orders.goToConfirmOrder order.Id) ["class","buttonX"] [Text "Pagina di conferma"]
            | _ -> em ""
        )

        br []

    ]

let error =
    [
        h2 "si e' verificato un errore"
    ]

let manageWelcomeMessage (welcomeMessages: LiteDb.WelcomeMessage list) (defaultWelcomeMessage: LiteDb.WelcomeMessage option) = 
    
    [
        h2 "gestisci messaggio di benevenuto"

        br[]
       
        (match defaultWelcomeMessage with | Some X -> (h2 ("messaggio attivo: "+X.Message)) | None -> (h2 "nessun messaggio attivo"))

        br[]
        Text("messaggi archiviati:")
        br[]
        div []
            [
                for message in welcomeMessages ->
                li [
                    Text(message.Message) 
                    (a (sprintf Path.Admin.removeWelcomeMessage message.Id) [] [Text(" rimuovi ")])
                    (match message.IsCurrentMessage with
                        | true -> (a (sprintf Path.Admin.makeMessageAsNotDefault message.Id) [] [Text(" disattiva ")])
                        | false -> (a (sprintf Path.Admin.makeMessageAsDefault message.Id) [] [Text(" attiva ")])
                    )
                ]
            ]
        br[]

        renderForm 
            {
                Form = Form.welcomeMessage
                Fieldsets =
                    [  {
                        Legend = "Nuovo messaggio"
                        Fields =
                        [
                            {
                                Label = "Testo Messaggio"
                                Html = formInput (fun f -> <@ f.Message @>) []
                            }
                        ]
                        
                       }

                    ]
                SubmitText = "aggiungi"
            }
    ]

let addCommentToOrderAndChangeItsState orderId =
    [
        h2 "cambia stato ad ordinei ordine"
    ]

let addCourse message =
    [
        h2 "add course"
        h2 message
        renderForm
            {
                Form = Form.course
                Fieldsets =
                    [ {
                        Legend = "aggiungi piatto"
                        Fields = 
                        [
                            {
                                Label = "Nome"
                                Html = formInput (fun f -> <@ f.Name@>) []
                            }
                            {
                                Label = "Descrizione"
                                Html = formInput (fun f -> <@ f.Description@>) []
                            }
                            {
                                Label = "Prezzo"
                                Html = formInput (fun f -> <@ f.Price @>) ["value", formatDec(0.1M)] 
                            }
                            {
                                Label = "Unita' di misura"
                                Html = selectInput (fun f -> <@ f.UnityOfMeasure @>) UnityOfMeasuresCombo (Some "Pezzo")
                            }

                            {
                                Label = "Disponibile"
                                Html = selectInput (fun f -> <@ f.Available @>) [("Yes","Yes");("No","No")] (Some "Yes")
                            }
                        ]
                      }
                    ]
                SubmitText = "Aggiungi" 
            }
    ]

let editCourse (course:LiteDb.Courses) message = 
    [
        h2 "edit course"
        h2 message
        renderForm
            {
                Form = Form.course
                Fieldsets =
                    [ {
                        Legend = "modifica piatto"
                        Fields = 
                        [
                            {
                                Label = "Course Name"
                                Html = formInput (fun f -> <@ f.Name@>) ["value", course.Name]
                            }
                            {
                                Label = "Descrizione"
                                Html = formInput (fun f -> <@ f.Description@>) ["value", match course.Description with | Some X -> X | _ -> "" ]
                            }
                            {
                                Label = "Prezzo"
                                Html = formInput (fun f -> <@ f.Price @>) ["value", formatDec(course.Price)] 
                            }

                            {
                                Label = "Unita' di misura"
                                Html = selectInput (fun f -> <@ f.UnityOfMeasure @>) UnityOfMeasuresCombo (Some (course.UnityOfMeasure.ToString()))
                            }
                            {
                                Label = "Availability"
                                Html = selectInput (fun f -> <@ f.Available @>) [("Yes","Yes");("No","No")] (if (course.Available) then Some "Yes" else Some "No")
                            }
                        ]
                      }
                    ]
                SubmitText = "Salva modifiche" 
            }
    ]

let aggregateView (nameQuantityPair: (string*decimal) list) =
    [
        for (q,v) in nameQuantityPair ->
            p [] [(Text(q+":" + (sprintf "%.2f" v)))]
    ]

let usersList (users:LiteDb.User list) =
    [
        for user in users ->
            p [] [
                (a (sprintf Path.Account.editUser user.Id) [] [Text user.UserName])

                (match user.UserType with
                    | LiteDb.Admin -> em ""
                    | _ -> a (sprintf Path.Account.removeUser user.Id) ["onclick","return confirm_click(" +  "\""+user.UserName+"\"" + ");"] [Text("  (rimuovi)")]
                )

                br []
                script [] [Raw("function confirm_click(username) { return confirm(\"Sei sicuro di voler eliminare l'utente  \"+username+\"?\"     ); }")]
            ]
    ]


let aggregateViewWithCustomerScomposition nameQuantityPair =
    [
        for (q,v,z) in nameQuantityPair ->
            div []  [
                (Text(q+":" + (sprintf "%M"  v)   ))
                li [ 
                    for (i,j,k)in z ->
                    a (sprintf Path.Orders.viewOrder k) [] [Text ("( " + i + ": " + (sprintf "%M" j) +  "), ")]
                ]
                br []
            ] 
    ]

let adminExcursions (periodicalExcursions: LiteDb.PeriodicalExcursion list)=
    [
        h2 "gestione corse"
        br []
        (a Path.Admin.createPeriodicalExcursion) [] [Text("aggiugi nuova corsa periodica")]
        br []
        br []
        br []
        div [] [
            for i in periodicalExcursions ->
                li [
                    (Text(sprintf "Nome: %s, " i.Name))
                    (Text("a partire da: "+(i.DateInit.Day.ToString()+"/"+i.DateInit.Month.ToString())+",")) 
                    (Text("ultimo giorno: "+(i.DateEnding.Day.ToString()+"/"+i.DateInit.Month.ToString())+","))
                    (Text("orario: "+i.Time.TimeOfDay.ToString()+", "))
                    (Text(sprintf "capienza posti: %d " i.Seats))
                    li [
                        for j in (i.DaysOfWeek) ->
                            (Text(j.ToString()+","))
                    ]
                    (a (sprintf Path.Admin.removePeriodicalExcursion i.Id) ["class","buttonX"] [Text(" rimuovi")])
                    br []
                    br []
                    ]
            ]
    ]


let cuponToSlots =
    [
        h2 "associazion cupon a slots"
    ]

let adminCupons = 
    [
        h2 "gestione cupon digitali"
    ]


let lookForCuponPage (unclaimedCupons: LiteDb.Cupon list)=
    [
        h2 "scegli il tuo cupon di sconto"

        table [ for cupon in unclaimedCupons ->
                    let slots = cupon.Slots |> List.sortBy (fun x -> x.DateTime) |>  List.map (fun x -> x.Excursion.Name+", "+x.DayOfWeek+" "+x.DateTime.ToString()) |>    List.fold (fun acc x -> acc+x) ""
                    let discountKind = match cupon.Discount with 
                        | LiteDb.Percentage X -> (sprintf "sconto percentuale %.2f %% " X)
                        | LiteDb.Amount X -> (sprintf "sconto di  %.2f  sul totale " X)
                    tr [
                        li [Text("cupon persone: "+(string)(cupon.NumberOfPeople)+ " con  "+ discountKind + " per slots:"+slots)]
                        (a (sprintf Path.Cupon.claimCupon cupon.Id ) ["class","buttonX"] [Text "richiedi questo cupon"])
                    ]
                ]
    ]


let claimCupon (cupon:LiteDb.Cupon) =
    [
        h2 "richiesta di cupon"

        renderForm 
            { Form = Form.subscribeForCupon
              Fieldsets =
                [ { Legend = "Email"
                    Fields =
                        [ {
                            Label = "Email adress"
                            Html = formInput (fun f -> <@ f.Email @>) []

                          }
                        ]
                  }

                ]
              SubmitText = "send"
            }
    ]

let home  userId role (welcomeMessage:LiteDb.WelcomeMessage option) nodeService   = [
    h2 "gestione orari e offert "
    br[]

    // (match welcomeMessage with | Some X -> h2 X.Message | _ -> h2 "")
    // br []

    // p [] [a (Path.Orders.newOrder) [] [Text "nuovo ordine" ]]
    // p [] [a Path.Orders.myOrders [] [Text "miei ordini" ]]
    // p [] [a (sprintf Path.Account.changePassword userId) [] [Text "cambio password" ]]

    (if (role = "Admin") then
        p [] [

            a (nodeService |> string)[] [Text "riconoscimento codice qr"]

            br[]
            br[]

            a Path.Admin.adminExcursions [] [Text "gestione corse/orari  " ]

            // a Path.Account.usersList [] [Text "lista utenti  " ]
            br []
            br []
            // a Path.Account.createNewAccount [] [Text "aggiungi nuovo utente  " ]
            br []
            br []
            a Path.Admin.adminCupons [] [Text "inserisci nuovi cupon  "]
            br []
            br []
            a Path.Admin.viewCupons [] [Text " visualizza cupon esistenti "]
            br []
            br []

            a Path.Cupon.lookForCuponPage [] [Text " visualizza pagina di ricerca dei cupon per clienti "]
            br []
            br []



            a Path.Account.manageQrCodeUsers [] [Text "aggiungi nuovo utente con url/qrcode  " ]
            br []
            br []
            // a Path.Orders.allOrders [] [Text "archivio di tutti gli ordini " ]
            br []
            br []
            // a Path.Orders.allOpenOrders [] [Text "tutti gli ordini non archiviati " ]
            br []
            br []
            // a Path.Orders.aggregatedOpenAndOngoingOrders [] [Text "vista aggregata ordini aperti e in progress " ]
            br []
            br []
            // a Path.Orders.aggregatedOnlyOpenOrders [] [Text "vista aggregata solo ordini aperti " ]
            br []
            br []
            // a (sprintf Path.Admin.aminCourses 0) [] [Text "gestione piatti " ]
            br []
            br []
            // a Path.Orders.allConfirmedOrders [] [Text "tutti gli ordini attivi" ]
            br []
            br []


            a Path.Admin.manageWelcomeMessage  [] [Text "gestione messaggio di benvenuto " ]

        ]

        else div [][]
    )
]



let truncate k (s : string) =
    if s.Length > k then
        s.Substring(0, k - 3) + "..."
    else s


let unauthorized = [
    h2 "pagina protetta"
]

let logon msg = [
    h2 "Log On"
    p [] [
        Text "Please enter your user name and password."
    ]

    div ["id", "logon-message"] [
        Text msg
    ]

    renderForm
        { Form = Form.logon
          Fieldsets = 
              [ { Legend = "Account Information"
                  Fields = 
                      [ { Label = "User Name"
                          Html = formInput (fun f -> <@ f.Username @>) [] }
                        { Label = "Password"
                          Html = formInput (fun f -> <@ f.Password @>) [] } ] } ]
          SubmitText = "Log On" }
]

let notFound = [
    h2 "Page not found"
    p [] [
        Text "Could not find the requested resource"
    ]
    p [] [
        Text "Back to "
        a Path.home [] [Text "Home"]
    ]
]

let partNav cartItems = 
    ulAttr ["id", "navlist"] [ 
    ]

let partUser (user : string option) = 
    div ["id", "part-user"] [
        match user with
        | Some user -> 
            yield Text (sprintf "Logged on as %s, " user)
            yield a Path.Account.logoff [] [Text "Log off"]
        | None ->
            yield a Path.Account.logon [] [Text "Log on"]
    ]

let detectCupon msg validity  optionId  (isValidForNextSlot:bool) (lookedupCupon:LiteDb.Cupon option) = 
    let discountMessage = match lookedupCupon with
        | Some X -> 
            match X.Discount with 
                | LiteDb.Percentage P -> (sprintf "sconto percentuale del %.2f %% " P)
                | LiteDb.Amount A ->  (sprintf "previsto sconto totale  di %.2f" A)
        | _ -> ""

    let cuponIsValidForNextSlot =
        match isValidForNextSlot with
        | true -> "cupon valido per la prossima tratta "+discountMessage
        | false -> "cupon non valido per la prossima tratta"

    [
        h2 ("detect cupon: "+ msg+"\n"+cuponIsValidForNextSlot)
        (match optionId with
        | Some X -> (a (sprintf Path.Cupon.voidCupon X) [] [Text "invalia il cupon"])
        | None -> em "")
        
    ]


let viewCupons (allCupons:LiteDb.Cupon list) = 
    [
        h2 "cupon esistenti:"

        table [ for cupon in allCupons ->
            let slots = cupon.Slots |> List.sortBy (fun x -> x.DateTime) |>  List.map (fun x -> x.Excursion.Name+", "+x.DayOfWeek+" "+x.DateTime.ToString()) |>    List.fold (fun acc x -> acc+x) ""
            let discountKind = match cupon.Discount with 
                | LiteDb.Percentage X -> (sprintf "sconto percentuale %.2f %% " X)
                | LiteDb.Amount X -> (sprintf "sconto di  %.2f  sul totale " X)
            tr [
                li [Text("cupon persone: "+(string)(cupon.NumberOfPeople)+ " con  "+ 
                    discountKind + " per slots:"+slots+ 
                    (match cupon.OwnerEmail with | Some X -> "email: richiedente "+X | _ -> "")+
                    (" - usato: "+cupon.Used.ToString())   )]
                (a (sprintf Path.Cupon.displayQrOfCupon cupon.Id ) ["class","buttonX"] [Text "vedi"])
                (a (sprintf Path.Cupon.removeCupon cupon.Id ) ["class","buttonX"] [Text "elimina"])
            ]
        ]
    ]


let emptyCart = [
    h2 "Your cart is empty"
    Text "Find some great music in our "
    a Path.home [] [Text "store"]
    Text "!"
]


let editOrderItem (orderItem: LiteDb.OrderItem) (allCourses: LiteDb.Courses list) =
    let allCoursesIdAndName = allCourses |> List.map (fun x -> ((decimal)x.Id,x.Name))
    [
        h2 "Modifica order item"
        renderForm
            {
                Form = Form.orderItemRef
                Fieldsets =
                    [ { Legend = "Order item"
                        Fields =
                            [ 
                                { 
                                    Label = "Nome merce"
                                    Html = selectInput (fun f -> <@ f.CourseId @>) allCoursesIdAndName (Some ((decimal)orderItem.CourseId))
                                }
                                { 
                                    Label = "Quantita"
                                    Html = formInput (fun f -> <@ f.Quantity @>) ["value",(orderItem.ItemQuantity|> string)] 
                                }
                                { 
                                    Label = "Commento"
                                    Html = formInput (fun f -> <@ f.Comment @>) ["value",(match orderItem.Comment with Some X -> X | _ -> "")]
                                }
                        ]

                      }
                    ]
                SubmitText = "Aggiorna" 
             }
    ]

let myOrders (orders: LiteDb.Order list ) =
    [
        h2 "lista miei ordini"
        div ["id","album-details"] [
            for order in orders ->
            li [
                Text(order.OrderDate.ToLongDateString()+" * ")
                Text(order.OrderDate.ToLongTimeString()+" * ")
                Text(LiteDb.textOfOrderState order.OrderState + " * ")

                (match order.AdminComment with
                    | Some X -> Text("annotazioni: "+X)
                    | None -> Text(""))

                (match order.OrderState with   
                    | LiteDb.Editing -> 
                        // (a (sprintf Path.Orders.editOrderRef order.Id) [] [Text " modifica"])
                        (a (sprintf Path.Orders.completeMenuViewPaged order.Id 0) [] [Text " modifica"])
                    | _ ->  Text("")
                )

                (match order.OrderState with   
                    | LiteDb.Editing -> 
                        a ((sprintf Path.Orders.removeOrder order.Id) |> 
                            Path.withParam(Path.Store.backUrl,WebUtility.UrlEncode(Path.Orders.myOrders))) [] [Text " rimuovi"]
                    | _ ->  Text("")
                )

                (match order.OrderState with
                    | X when X <> LiteDb.Editing ->
                        a ((sprintf Path.Orders.viewOrder order.Id) |> 
                            Path.withParam(Path.Store.backUrl,WebUtility.UrlEncode(Path.Orders.myOrders))) [] [Text " vedi "]
                    | _ ->  Text("")
                    
                )

            ]
        ]
    ]

let allOrders (orders: LiteDb.Order list ) =
    [
        h2 "lista  ordini"
        div ["id","album-details"] [
            for order in orders ->
            li [
                Text(order.OrderDate.ToLongDateString()+" * ")
                Text(order.OrderDate.ToLongTimeString()+" * ")
                Text(order.OrderState.ToString()+" * ")
                a (sprintf Path.Orders.editOrderRef order.Id) [] [Text " modifica"]
                a ((sprintf Path.Orders.removeOrder order.Id) |> 
                    Path.withParam(Path.Store.backUrl,WebUtility.UrlEncode(Path.Orders.allOrders))) [] [Text " rimuovi "]
                a ((sprintf Path.Orders.makeOrderAsOnGoing order.Id) |> 
                    Path.withParam(Path.Store.backUrl,WebUtility.UrlEncode(Path.Orders.allOrders))) [] [Text " evadi "]
                a ((sprintf Path.Orders.makeOrderAsDone order.Id) |> 
                    Path.withParam(Path.Store.backUrl,WebUtility.UrlEncode(Path.Orders.allOrders))) [] [Text " finito "]
                a ((sprintf Path.Orders.makeOrderAsAchived order.Id) |> 
                    Path.withParam(Path.Store.backUrl,WebUtility.UrlEncode(Path.Orders.allOrders))) [] [Text " archivia "]
            ]
        ]
    ]

let adminCourses (courses:LiteDb.Courses list) (imagesForCourses: Map<int,string option>) =

    [
        h2 "amministrazione piatti"
        a (Path.Admin.addNew ) [] [Text "aggiungi nuovo "]
        br []
        br []

        table [
            for i in courses ->
            tr [
                td [Text(i.Name)]
                td [a (sprintf Path.Admin.editCourse  i.Id) [] [Text " modifica "]]
                td [a (sprintf Path.Admin.removeCourse  i.Id) [] [Text " rimuovi "]]
                td [Text(if (i.Available) then "Disponibile" else "non disponibile")]

                td [(match imagesForCourses.[i.Id] with
                        Some X -> (tag "img" [("src","data:image/bmp;base64, "+X);("width","100")][])
                        | None -> (em "no image")
                    )
                ]

                td [a (sprintf Path.Admin.imageCourseUploader  i.Id) [] [Text " aggiungi immagine "]]
                td [a (sprintf Path.Admin.imageCourseRemover  i.Id) [] [Text " rimuovi immagine "]]
            ]
        ]
        (a (sprintf Path.Admin.aminCourses 0) [] [Text ">"])

    ]




let confirmedOrders (orders: LiteDb.Order list) =
    let mailNotify = orders |> List.map (fun x -> (x,x.User.UserEmail.IsSome)) |> Map.ofList

    [
        h2 "lista  ordini"
        table [
            for order in orders ->
            tr [
                td [
                    Text(order.OrderDate.ToLongDateString()+" * ")
                    Text(order.OrderDate.ToLongTimeString()+" * ")
                    Text(LiteDb.textOfOrderState (order.OrderState)+" * ")
                    (a (sprintf Path.Account.editUser order.User.Id) [] [Text(order.User.UserName) ])
                ]

                td [
                    ( match order.OrderState with 
                        LiteDb.Confirmed ->
                            ( match mailNotify.[order] with
                                | true -> a (sprintf Path.Orders.manageConfirmedOrderWithEmail order.Id) [] [Text " gestisci "]
                                | false -> a (sprintf Path.Orders.manageConfirmedOrder order.Id) [] [Text " gestisci "]
                            )
                            | _ -> Text("")
                    )
                ]

                td [
                    a (sprintf Path.Orders.viewOrder order.Id) [] [Text " vedi "]
                ]

                td [
                    ( match order.OrderState with   
                        LiteDb.Accepted ->
                                a (sprintf Path.Orders.makeOrderAsOngoingRef order.Id) [] [Text " comincia "]
                                | _ -> Text("")
                    )

                    ( match order.OrderState with   
                        LiteDb.Ongoing ->
                            a (sprintf Path.Orders.makeOrderAsDoneRef order.Id) [] [Text " finito "]
                            | _ -> Text("")
                    )

                    ( match order.OrderState with   
                        (LiteDb.Rejected|LiteDb.Done) -> 
                            a (sprintf Path.Orders.makeOrderAsAchived order.Id) [] [Text " archivia "]
                            | _ -> Text("")
                    )
                ]

            ]
        ]
    ]



let openOrders (orders: LiteDb.Order list ) =
    [
        h2 "lista  ordini"
        div ["id","album-details"] [
            for order in orders ->
            li [
                Text(order.OrderDate.ToLongDateString()+" * ")
                Text(order.OrderDate.ToLongTimeString()+" * ")
                Text(order.OrderState.ToString()+" * ")
                Text(order.User.UserName + " * ")
                a (sprintf Path.Orders.viewOrder order.Id) [] [Text " vedi "]
                a ((sprintf Path.Orders.removeOrder order.Id) |> 
                    Path.withParam(Path.Store.backUrl,WebUtility.UrlEncode(Path.Orders.allOpenOrders))) [] [Text " rimuovi"]
                a ((sprintf Path.Orders.makeOrderAsOnGoing order.Id) |> 
                    Path.withParam(Path.Store.backUrl,WebUtility.UrlEncode(Path.Orders.allOpenOrders))) [] [Text " evadi"]
                a ((sprintf Path.Orders.makeOrderAsDone order.Id) |> 
                    Path.withParam(Path.Store.backUrl,WebUtility.UrlEncode(Path.Orders.allOpenOrders))) [] [Text " finito "]
                a ((sprintf Path.Orders.makeOrderAsAchived order.Id) |> 
                    Path.withParam(Path.Store.backUrl,WebUtility.UrlEncode(Path.Orders.allOpenOrders))) [] [Text " archivia "]
            ]
        ]
    ]

let aknowledgeConfmation =
    [
        h2 "l'ordine e' stato inoltrato"
    ]

let viewOrder (orderItems: LiteDb.OrderItem list) (order: LiteDb.Order) =
    [
        h2 (sprintf "dettaglio ordine: cliente: %s, data/ora %s-%s " order.User.UserName (order.OrderDate.ToLongDateString()) (order.OrderDate.ToLongTimeString()))
        h2 ("stato dell'ordine: "+(LiteDb.textOfOrderState (order.OrderState)))
        (match order.AdminComment with 
            | Some X -> ( Text("Commento amministratore: "+X))
            | None -> Text("")
        )

        div ["id", "album-details"] [
            for orderItem in orderItems ->
                li [
                    Text (sprintf "%s -  %s - quantita': %.2f\n" ("name: "+orderItem.Course.Name) (match orderItem.Comment with | Some X -> X | _ -> "") (orderItem.ItemQuantity))
                ]
        ]
    ]



let goToConfirmOrder (order: LiteDb.Order) (orderItems: LiteDb.OrderItem list) =
    let total = orderItems |>  List.map (fun x -> x.Course.Price*(decimal)x.ItemQuantity) |> List.sum
    
    [
        h2 "stai per confermare il seguente ordine"

        table [
            for orderItem in orderItems ->
                    tr [
                        td [
                            (Text(orderItem.Course.Name))
                        ]
                        td [
                            (Text(match orderItem.Comment with Some X -> "commento: "+X+"" | _ -> ""))
                        ]
                        td [
                            (Text(sprintf "quantità: %.2f" orderItem.ItemQuantity))
                        ]
                        td [
                            (Text(sprintf "€: %.2f " (orderItem.Course.Price*((decimal)(orderItem.ItemQuantity)))))
                        ]
                    ]
            ]
        br[]
        table [
            tr [
                td [
                    Text(sprintf "totale €:  %.2f" total )
                ]
            ]
        ]
        br[]
        a (sprintf Path.Orders.confirmOrder order.Id ) ["class","buttonX"] [Text " clicca per confermare"]
        a (sprintf Path.Orders.payWithPayPal order.Id ) ["class","buttonX"] [Text " clicca per pagare con carta di credito"]

    ]

let manageConfirmedOrder (order: LiteDb.Order) (orderItems: LiteDb.OrderItem list) =
    [
        h2 "approva/modifica/respingi ordine"
        div ["id", "album-details"] [
            for orderItem in orderItems ->
                li [
                    Text (sprintf "%s -  %s - %.2f\n" (orderItem.Course.Name) (match orderItem.Comment with Some X -> (X) | None -> "") (orderItem.ItemQuantity))
                    a (sprintf Path.Orders.removeOrderItemRef orderItem.Id) [] [Text "rimuovi "]
                    a (sprintf Path.Orders.editOrderItemRef orderItem.Id) [] [Text "modifica"]
                ]
        ]
        renderForm
            {
                Form = Form.orderApproval 
                Fieldsets =
                    [ { Legend = "Order item"
                        Fields =
                            [ 
                                { 
                                    Label = "Approvazione"
                                    Html = selectInput (fun f -> <@ f.Approval @>)  Utils.APPROVED_REJECTED_SELECT_INPUT    (Some Utils.APPROVED)
                                }
                                { 
                                    Label = "Commento"
                                    Html = formInput (fun f -> <@ f.AdminComment @>) [] 
                                }
                        ]

                      }
                    ]
                SubmitText = "Conferma" 
             }

    ]

let manageConfirmedOrderWithEmail (order: LiteDb.Order) (orderItems: LiteDb.OrderItem list) =
    [
        h2 "approva/modifica/respingi ordine"
        div ["id", "album-details"] [
            for orderItem in orderItems ->
                li [
                    Text (sprintf "%s -  %s - %.2f\n" (orderItem.Course.Name) (match orderItem.Comment with Some X -> (X) | None -> "") (orderItem.ItemQuantity))
                    a (sprintf Path.Orders.removeOrderItemRef orderItem.Id) [] [Text "rimuovi "]
                    a (sprintf Path.Orders.editOrderItemRef orderItem.Id) [] [Text "modifica"]
                ]
        ]
        renderForm
            {
                Form = Form.orderApprovalWithEmail
                Fieldsets =
                    [ { Legend = "Order item"
                        Fields =
                            [ 
                                { 
                                    Label = "Approvazione"
                                    Html = selectInput (fun f -> <@ f.Approval @>) Utils.APPROVED_REJECTED_SELECT_INPUT (Some Utils.APPROVED)
                                }
                                { 
                                    Label = "Commento"
                                    Html = formInput (fun f -> <@ f.AdminComment @>) [] 
                                }
                                { 
                                    Label = "Manda email di conferma"
                                    Html = selectInput (fun f -> <@ f.SendConfirmationEmail @>)  Utils.YES_NO_SELECT_INPUT (Some Utils.YES) 
                                }
                            ]

                      }
                    ]
                SubmitText = "Conferma" 
             }

    ]

let editOrderRef (order: LiteDb.Order) (orderItems: LiteDb.OrderItem list) (courses: LiteDb.Courses list) =
    let autoComp = "[" + (resources.ItemNames |> Array.toList |>   List.fold (fun acc x -> acc + "\"" + x +  "\"" + "," ) "" ) + "];"
    let coursesDropList = courses |> List.map (fun (x:LiteDb.Courses) -> ((decimal)x.Id,x.Name))
    [
        h2 "carrello"
        div ["id", "album-details"] [
            for orderItem in orderItems ->
                let textComment = match orderItem.Comment with 
                    | None -> ""
                    | Some X -> "commento: "+X+" - "

                li [
                    Text (sprintf "nome: %s -  %s quantita' %.2f\n" (orderItem.Course.Name) textComment (orderItem.ItemQuantity))
                    a (sprintf Path.Orders.removeOrderItem orderItem.Id) [] [Text "rimuovi "]
                    a (sprintf Path.Orders.editOrderItem orderItem.Id) [] [Text "modifica"]
                ]
        ]

        renderForm
            {
                Form = Form.orderItemRef 
                Fieldsets =
                    [ { Legend = "Scegli il prodotto:"
                        Fields =
                            [ 
                                { 
                                    Label = "Prodotto"
                                    Html = selectInput (fun f -> <@ f.CourseId @>)  coursesDropList  None
                                }
                                { 
                                    Label = "Quantita"
                                    Html = formInput (fun f -> <@ f.Quantity @>) ["value", "1"] 
                                }
                                { 
                                    Label = "Commento"
                                    Html = formInput (fun f -> <@ f.Comment @>) [] 
                                }
                        ]

                      }
                    ]
                SubmitText = "Aggiungi" 
             }

        br []
        a (sprintf Path.Orders.completeMenuViewPaged order.Id 0) [] [Text "Vedi menu dettagliato"]
        br []

        a (sprintf Path.Orders.goToConfirmOrder order.Id) [] [Text "Vai a pagina di conferma"]
        br []

        script [ "type", "text/javascript"; "src", "/autocomplete.js" ] []
    ]



let index  partUser  container =
    "<!DOCTYPE html><meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">"+
    (html [] [
        head [] [
            title [] "digital cupons"
            cssLink "/Site.css?uyYXuz"
        ]

        body [] [
            div ["id", "header"] [
                tag "h1" [] [
                    a Path.home [] [Text "Gestione orari e offerte"]
                ]
                partUser
            ]

            div ["id", "main"] container

            div ["id", "footer"] [
                Text "made by Tony_X - tonyx1@gmail.com (Luckysoft)"
            ]
        ]
    ]
    |> htmlToString)

let manageQrCodeUsers code =
    [
        h2 "qr code manager"

        Text("codice accesso utente: "+code)
    ]

let autoRegisterUser =
    [
        h2 "autoregistrazione"
    ]


let imageView =
    [
        h2 "image View"
    ]

let courseImageUpload id =
    [
        h2 "courseImageUpload"
    ]