module DigitalCupons.App

open System

open Suave
open Suave.Authentication
open Suave.Cookie
open Suave.Filters
open Suave.Form
open Suave.Model.Binding
open Suave.Operators
open Suave.RequestErrors
open Suave.State.CookieStateStore
open Suave.Successful
open Suave.Web
open System.Security
open System.Linq
open System.Net.Mail
open FSharp.Configuration
open QRCoder

open LiteDB
open LiteDB.FSharp
open LiteDB.FSharp.Linq
open DigitalCupons.LiteDb
open System.Net
open Newtonsoft.Json
open PayPalCheckoutSdk.Core
open PayPalCheckoutSdk.Orders
open System.Globalization
open System.Drawing

open BraintreeHttp

// open Newtonsoft.Json

type Settings = AppSettings<"App.config">
type dummy = {a: string}

let log = log4net.LogManager.GetLogger(System.Reflection.MethodBase.GetCurrentMethod().DeclaringType);

let englishLocal = DigitalCupons.LocalSchema.Resource.Load ("resources-en.xml")
let italianLocal = DigitalCupons.LocalSchema.Resource.Load ("resources-it.xml")

let defaultLocal = DigitalCupons.LocalSchema.Resource.Load ("resources-"+Settings.Localization+".xml")

let mapOfLocals = [("it",italianLocal);("en",englishLocal)] |> Map.ofList   


type UserLoggedOnSession = {
    UserId: int
    Username : string
    Role : string
}


type Session = 
    | NoSession
    | CartIdOnly of string
    | UserLoggedOn of UserLoggedOnSession


let session f = 
    statefulForSession
    >=> context (fun x -> 
        match x |> HttpContext.state with
        | None -> f NoSession
        | Some state ->
            match state.get "cartid", state.get "username", state.get "role", state.get "id" with
            | Some cartId, None, None,None ->
                f (CartIdOnly cartId)
            | _, Some username, Some role, Some id ->
                f (UserLoggedOn {Username = username; Role = role; UserId = id  })
            | _ -> 
                f NoSession)


// setLanguage strLanguage =




// let _ = 
//     let users = LiteDb.getAllUsers()
//     users |> Seq.iter (fun x -> printf "%s\n" x.UserName)

//     printf "cambio passwd"
//     let admin = LiteDb.getUserByName("giorgio")
//     let _ = match admin with
//         | Some X -> LiteDb.forceSetPassword X (passHash "admin")
//         | _ -> ()
//     ()

// let _  =
//     LiteDb.creatCourse "pizza" true

// let _ =
//     LiteDb.removeAllOrders()    

// let _ = LiteDb.createUser "archive" (passHash "XXunavailableXX")

// let _ =
//     LiteDb.createPeriodicalExcursion (DateTime(1999,9,9,1,1,1)) [System.DayOfWeek.Tuesday; System.DayOfWeek.Friday] 99

// let _ =
//     LiteDb.removeAllCupons()


let html container =
    let result user =
        OK (View.index 
                (View.partUser user) 
                container)
        >=> Writers.setMimeType "text/html; charset=utf-8"

    session (function
    | UserLoggedOn { Username = username } -> 
        result  (Some username)
    | CartIdOnly cartId ->
        result  None
    | NoSession ->
        result  None)


let home (userLoggedOnSession:UserLoggedOnSession) = warbler (fun _ ->
    let welcomeMessage = LiteDb.getCurrentMessages() |> List.tryHead
    View.home userLoggedOnSession.UserId userLoggedOnSession.Role welcomeMessage Settings.NodeService  |> html)




let bindToForm form handler =
    bindReq (bindForm form) handler BAD_REQUEST

let passHash (pass: string) =
    use sha = Security.Cryptography.SHA256.Create()
    Text.Encoding.UTF8.GetBytes(pass)
    |> sha.ComputeHash
    |> Array.map (fun b -> b.ToString("x2"))
    |> String.concat ""

let sessionStore setF = context (fun x ->
    match HttpContext.state x with
    | Some state -> setF state
    | None -> never)


let returnPathOrHome = 
    request (fun x -> 
        let path = 
            match (x.queryParam "returnPath") with
            | Choice1Of2 path -> path
            | _ -> Path.Account.logon
        Redirection.FOUND path)


let authenticateUser (user: User) =
    authenticated Cookie.CookieLife.Session false
    >=> session (function | _ -> succeed)
    >=> sessionStore (fun store ->
        store.set "username" user.UserName
        >=> store.set "role"  (user.UserType |> string)
            >=> store.set  "id" (user.Id ))
    >=> returnPathOrHome

let changeLocal (langCode:string)  =
    log.Debug(sprintf "change local to %s" langCode)
    session (function | _ -> succeed)
    >=> sessionStore (fun store ->
        log.Debug("sessionstore")
        store.set "lang" langCode
    ) >=>
    Redirection.FOUND (sprintf Path.Cupon.lookForCuponPage langCode)


let authenticateUserAndGoToUrl (user: User) url =
    authenticated Cookie.CookieLife.Session false
    >=> session (function | _ -> succeed)
    >=> sessionStore (fun store ->
        store.set "username" user.UserName
        >=> store.set "role"  (user.UserType |> string)
            >=> store.set  "id" (user.Id ))
    >=> Redirection.FOUND url 




let logon =
    choose [
        GET >=> (View.logon "" |> html)
        POST >=> bindToForm Form.logon (fun form ->
            let (Password password) = form.Password
            match validateUser form.Username password  with
            | Some user ->
                    authenticateUser user
            | _ ->
                View.logon "Nome utente o password non valida." |> html
        )
    ]

let reset =
    unsetPair SessionAuthCookie
    >=> unsetPair StateCookie
    >=> Redirection.FOUND Path.home

let redirectWithReturnPath redirection =
    request (fun x ->
        let path = x.url.AbsolutePath
        Redirection.FOUND (redirection |> Path.withParam ("returnPath", path)))

let loggedOn f_success =
    authenticate
        Cookie.CookieLife.Session
        false
        (fun () -> Choice2Of2(redirectWithReturnPath Path.Account.logon))
        (fun _ -> Choice2Of2 reset)
        f_success

let admin f_success =
    loggedOn (session (function
        | UserLoggedOn { Role = "Admin" } -> f_success
        | UserLoggedOn _ -> FORBIDDEN "Only for admin"
        | _ -> UNAUTHORIZED "Not logged in"
    ))

let anyUserLoggedOn f_success =
    loggedOn (session (function
        | UserLoggedOn X -> f_success X
        | _ -> UNAUTHORIZED "Not logged in"
    ))


let get64EncodedImageFromStorage name =
    let image = LiteDb.getImage name
    let stream = new System.IO.MemoryStream()
    do image.CopyTo(stream)
    let arrayOfImage = stream.ToArray()
    let encoded = System.Convert.ToBase64String (arrayOfImage)
    encoded



// let editOrder orderId =  
//     let order = LiteDb.getOrderById orderId
//     let orderOwnerId = order.UserId
//     let availableCourses = LiteDb.getAllAvailableCourses()
//     session (function 
//         | UserLoggedOn {UserId = X; Role=Y} when X = orderOwnerId || Y = "Admin" ->
//         choose [ 
//             GET >=> warbler (fun _ ->
//                 let orderItems = LiteDb.getOrderItemsOfOrder order.Id |> Seq.toList
//                 View.editOrderRef order orderItems availableCourses |> html)
//             POST >=> bindToForm Form.orderItemRef (fun form ->
//                 let _ = LiteDb.createOrderItemById order.Id ((int)form.CourseId) ((int)form.Quantity) (form.Comment)
//                 Redirection.FOUND (sprintf Path.Orders.editOrderRef order.Id))
//         ]
//         | _ -> Redirection.FOUND Path.Orders.unauthorized
//     )

let decreaseOrderItemQuantity orderItemId pageNumber =
    // let pageNumber = 0
    let orderItem = LiteDb.getOrderItem orderItemId
    let order = LiteDb. getOrderById orderItem.OrderId
    session (function
        | UserLoggedOn {UserId = X; Role = Y} when X = order.User.Id || Y = "Admin"  ->
            let orderItemDecreased = {orderItem with ItemQuantity = orderItem.ItemQuantity - 1M}
            let _ = match orderItemDecreased.ItemQuantity with 
                    |  X  when (X >0M)  -> (LiteDb.updatOrderItemByOrderitemRecord orderItemDecreased) |> ignore
                    | _ -> (LiteDb.removeOrderItem orderItemId) |> ignore
            Redirection.FOUND (sprintf Path.Orders.completeMenuViewPaged order.Id pageNumber)
        | _ -> Redirection.FOUND Path.Orders.unauthorized
    
    )

let increaseOrderItemQuantity orderItemId pageNumber =
    // let pageNumber = 0
    let orderItem = LiteDb.getOrderItem orderItemId
    let order = LiteDb.getOrderById orderItem.OrderId
    session (function
        | UserLoggedOn {UserId = X; Role = Y} when X = order.User.Id || Y = "Admin"  ->
            let orderItemDecreased = {orderItem with ItemQuantity = orderItem.ItemQuantity + 1M}
            let _ = match orderItemDecreased.ItemQuantity with 
                    |  X  when (X >0M)  -> (LiteDb.updatOrderItemByOrderitemRecord orderItemDecreased) |> ignore
                    | _ -> (LiteDb.removeOrderItem orderItemId) |> ignore
            Redirection.FOUND (sprintf Path.Orders.completeMenuViewPaged order.Id pageNumber)
        | _ -> Redirection.FOUND Path.Orders.unauthorized
    
    )




let completeMenuViewPaged orderId pageNumber  =
    let order = LiteDb.getOrderById orderId
    let orderOwnerId = order.User.Id
    let availableCourses = LiteDb.getAllAvailableCoursesByPage pageNumber


    // let availableCoursesCount = LiteDb.getCountOfAllAvailableCourses()
    // let imagesForCourses = availableCourses |> List.map (fun  x -> (x.Id,
    //     match x.ImageName with
    //         | Some name -> Some (get64EncodedImageFromStorage name) 
    //         | None -> None
    //     )
    // )

    // let mapImagesForCourses = imagesForCourses |> Map.ofList

    let orderItems = LiteDb.getOrderItemsOfOrder orderId |> Seq.toList
    session (function
        | UserLoggedOn {UserId = X; Role = Y} when X = orderOwnerId || Y = "Admin" -> 
        choose [
            GET >=> warbler (fun x ->

                // let order = LiteDb.getOrderById orderId
                // let orderOwnerId = order.User.Id
                // let availableCourses = LiteDb.getAllAvailableCoursesByPage pageNumber
                // let availableCoursesCount = LiteDb.getCountOfAllAvailableCourses()

                let imagesForCourses = availableCourses |> List.map (fun  x -> (x.Id,
                    match x.ImageName with
                        | Some name -> Some (get64EncodedImageFromStorage name) 
                        | None -> None
                    )
                )

                let mapImagesForCourses = imagesForCourses |> Map.ofList

                let (availableCourses,availableCoursesCount,keySearch,imgMap) = match (x.request.queryParam "search") with
                    | Choice1Of2 X -> 
                        let availCourses = LiteDb.geAllAvailableCorseByPageWithNameSearch 0 X
                        let imgCourses = (availCourses |> List.map (fun  x -> (x.Id,
                            match x.ImageName with
                                | Some name -> Some (get64EncodedImageFromStorage name) 
                                | None -> None))) |> Map.ofList
                        (availCourses,LiteDb.getCountOfAllAvailableCourseesWithNameSearch X, Some X,imgCourses)
                    | _ -> (LiteDb.getAllAvailableCoursesByPage pageNumber,LiteDb.getCountOfAllAvailableCourses(),None,mapImagesForCourses)
                let pageN = match keySearch with | Some X -> 0 | _ -> pageNumber

                View.detailedMenu order availableCourses orderItems imgMap pageN availableCoursesCount keySearch |> html)
            POST >=> bindToForm Form.nameSearch (fun form -> 
                match form.Name with 
                    | Some X ->  Redirection.FOUND ((sprintf Path.Orders.completeMenuViewPaged orderId pageNumber |> Path.withParam ("search",X)))
                    | _ ->   Redirection.FOUND (sprintf Path.Orders.completeMenuViewPaged orderId pageNumber)
            )

        ]
        | _ -> Redirection.FOUND Path.Orders.unauthorized
    )

    
let goToConfirmOrder orderId =
    let order = LiteDb.getOrderById orderId
    let orderOwnerId = order.User.Id
    let orderItems = LiteDb.getOrderItemsOfOrder orderId |> Seq.toList
    session ( function 
        | UserLoggedOn {UserId = X; Role = Y} when X = orderOwnerId || Y = "Admin" ->
            View.goToConfirmOrder order orderItems |> html
        | _ -> Redirection.FOUND Path.Orders.unauthorized
    )

type OrderPay = {amount:decimal}

let payWithPayPal orderId =
    let order = LiteDb.getOrderById orderId
    let orderOwnerId = order.User.Id
    let orderItems = LiteDb.getOrderItemsOfOrder orderId |> Seq.toList
    let total = orderItems |> List.map (fun x -> x.Course.Price*x.ItemQuantity) |> List.sum
    let o = {amount=total} 
    session ( function 
        | UserLoggedOn {UserId = X; Role = Y} when X = orderOwnerId || Y = "Admin" ->
            DotLiquid.page("paypal.html") o

            // View.goToConfirmOrder order orderItems |> html
        | _ -> Redirection.FOUND Path.Orders.unauthorized
    )
    


let confirmOrder orderId =
    let order = LiteDb.getOrderById orderId
    let orderOwnerId = order.User.Id
    session ( function
        | UserLoggedOn {UserId = X; Role = Y} when X = orderOwnerId || Y = "Admin" ->
            LiteDb.makeOrderAsConfirmed orderId
            View.aknowledgeConfmation |> html
        | _ -> Redirection.FOUND Path.Orders.unauthorized
    )

let makeOrderAsOngoingRef orderId =
    LiteDb.makeOrderAsOngoing orderId
    Redirection.FOUND Path.Orders.allConfirmedOrders






let addItemFromDetailedMenu orderId courseId  pageNumber=
    // let pageNumber = 0
    let order = LiteDb.getOrderById orderId
    // let course = LiteDb.getCourse courseId
    let orderItems = LiteDb.getOrderItemsOfOrder orderId |> Seq.toList

    session (function 
        | UserLoggedOn {UserId = X; Role = Y} when X = order.User.Id || Y = "admin" ->
            let matchingOrderItemExists = orderItems |> 
                List.filter (fun (x:LiteDb.OrderItem) -> (x.CourseId = courseId && x.Comment.IsNone) ) |> List.tryHead

            let _ = match matchingOrderItemExists with
                | Some X -> let updatedOrderItem = {X with ItemQuantity =  X.ItemQuantity + 1M}
                            LiteDb.updatOrderItemByOrderitemRecord updatedOrderItem |> ignore
                | _ -> (LiteDb.createOrderItemById orderId courseId 1M None)  |> ignore

            Redirection.FOUND (sprintf Path.Orders.completeMenuViewPaged orderId pageNumber)
        | _ -> Redirection.FOUND Path.Orders.unauthorized
    )


let makeOrderAsDoneRef orderId =
    log.Debug(printf " makeOrderAsDoneRef %d " orderId)
    LiteDb.makeOrderAsDone orderId

    let order = LiteDb.getOrderById orderId
    let user = order.User
    let orderItems = LiteDb.getOrderItemsOfOrder orderId |> Seq.toList

    let administrator = LiteDb.getAdministrator()

    let sendEmail  =
        async {
            match (user.UserEmail,administrator.UserEmail) with
            | (Some toAddress,Some fromAddress) ->
                try 
                    let smtpClient = new SmtpClient(Settings.SmtpAddress,Settings.SmtpPort)
                    smtpClient.Credentials <- System.Net.NetworkCredential(Settings.SmtpCredentialsName,Settings.SmtpCredentialsPassword)
                    smtpClient.DeliveryMethod <- SmtpDeliveryMethod.Network
                    smtpClient.EnableSsl <- true
                    let mail = new MailMessage()
                    mail.From <- MailAddress(fromAddress,Settings.EmailDisplayName)
                    mail.To.Add(toAddress)
                    mail.Subject <- Settings.ProductIsDone
                    let body =  (orderItems |> List.map (fun (x:LiteDb.OrderItem) -> 
                        "prodotto: "+x.Course.Name+ ", quantita':"+((string)x.ItemQuantity)+"\n" ) |> List.fold  (+) "" )+"\n"+
                        Settings.Greetings
                    mail.Body <- body
                    smtpClient.Send(mail)
                with | err -> log.Error("errore email",err)
            | _ -> ()
        }
    let _ = Async.Start(sendEmail)

    Redirection.FOUND Path.Orders.allConfirmedOrders

let editOrderRef orderId =  
    let order = LiteDb.getOrderById orderId
    let orderOwnerId = order.User.Id
    let availableCourses = LiteDb.getAllAvailableCourses()
    session (function 
        | UserLoggedOn {UserId = X; Role=Y} when X = orderOwnerId || Y = "Admin" ->
        choose [ 
            GET >=> warbler (fun _ ->
                let orderItems = LiteDb.getOrderItemsOfOrder order.Id |> Seq.toList
                View.editOrderRef order orderItems availableCourses |> html)
            POST >=> bindToForm Form.orderItemRef (fun form ->
                let _ = LiteDb.createOrderItemById order.Id ((int)form.CourseId) (form.Quantity) (form.Comment)
                Redirection.FOUND (sprintf Path.Orders.editOrderRef order.Id))
        ]
        | _ -> Redirection.FOUND Path.Orders.unauthorized
    )


let viewOrder orderId = 
    let orderItems = LiteDb.getOrderItemsOfOrder orderId |> Seq.toList
    let order = LiteDb.getOrderById orderId
    View.viewOrder orderItems order |> html

let rejectOrder orderId =
    let _ = LiteDb.makeOrderAsRejected orderId
    Redirection.FOUND Path.Orders.allConfirmedOrders
    



let editOrderItem orderItemId (userLoggedOn: UserLoggedOnSession)=
    let (userId,orderId) = LiteDb.getUserAndOrderIdOfOrderItem orderItemId
    let orderItem = LiteDb.getOrderItem orderItemId
    let allCourses = LiteDb.getAllAvailableCourses()
    if (userId=userLoggedOn.UserId || userLoggedOn.Role = "Admin") then
        choose [
            GET >=> warbler (fun _ ->
                View.editOrderItem orderItem allCourses |> html
            )
            POST >=> bindToForm Form.orderItemRef (fun form ->
                let _ = LiteDb.updateOrderItem orderItem ((int)form.CourseId) (form.Quantity) form.Comment
                Redirection.FOUND (sprintf Path.Orders.editOrderRef orderId) 
            )
        ]
    else 
        Redirection.FOUND Path.Orders.unauthorized


let manageConfirmedOrder orderId =
    let order = LiteDb.getOrderById orderId

    choose [
        GET >=> warbler (fun _ ->
            let order = LiteDb.getOrderById orderId
            let orderItems = LiteDb.getOrderItemsOfOrder orderId |> Seq.toList
            View.manageConfirmedOrder order orderItems |> html
        )
        POST >=> bindToForm Form.orderApproval (fun form ->
            let stateForOrder = if (form.Approval = "Approvato") then LiteDb.Accepted else LiteDb.Rejected
            let _ = LiteDb.updateOrderStateAndAdminComment orderId stateForOrder form.AdminComment
            Redirection.FOUND Path.Orders.allConfirmedOrders

        )
    ]


let manageConfirmedOrderWithEmail orderId =
    let order = LiteDb.getOrderById orderId
    let orderItems = LiteDb.getOrderItemsOfOrder orderId |> Seq.toList
    let user = order.User
    let administrator = LiteDb.getAdministrator()

    let ordersStatesLink = (Settings.HostAddress.ToString())+(((sprintf Path.Orders.viewOrder orderId)).Substring(1))

   
    choose [
        GET >=> warbler (fun _ ->
            let orderItems = LiteDb.getOrderItemsOfOrder orderId |> Seq.toList
            View.manageConfirmedOrderWithEmail order orderItems |> html
        )
        POST >=> bindToForm Form.orderApprovalWithEmail (fun form ->
            let stateForOrder = if (form.Approval = "Approvato") then LiteDb.Accepted else LiteDb.Rejected
            let _ = LiteDb.updateOrderStateAndAdminComment orderId stateForOrder form.AdminComment
            let messageForOrder = match stateForOrder with
                | LiteDb.Accepted -> "l'ordine è stato accettato"
                | _ -> "siamo spiacenti ma l'ordine non è stato accettato"
            let comment = match form.AdminComment with | Some X -> X | _ -> ""
            
            let sendEmail  =
                async {
                    match (user.UserEmail,administrator.UserEmail) with
                    | (Some emailAddress,Some fromAddress) ->
                        try 
                            let smtpClient = new SmtpClient(Settings.SmtpAddress,Settings.SmtpPort)
                            smtpClient.Credentials <- System.Net.NetworkCredential(Settings.SmtpCredentialsName,Settings.SmtpCredentialsPassword)
                            smtpClient.DeliveryMethod <- SmtpDeliveryMethod.Network
                            smtpClient.EnableSsl <- true
                            let mail = new MailMessage()
                            mail.From <- MailAddress(fromAddress,Settings.EmailDisplayName)
                            mail.To.Add(emailAddress)
                            mail.CC.Add(fromAddress)
                            mail.Subject <- messageForOrder
                            // let body =  (orderItems |> List.map (fun (x:LiteDb.OrderItem) -> (sprintf "prodotto: %s, quantita' %s" x.Course.Name ((string)x.ItemQuantity)+"\n" )) |> List.fold  (+) "" ) + "\n\n"+comment+"\n"+Settings.Greetings
                            let body =  (orderItems |> List.map (fun (x:LiteDb.OrderItem) -> (sprintf "prodotto: %s, quantita' %s" x.Course.Name ((string)x.ItemQuantity)+"\n" )) |> List.fold  (+) "" ) + "\n\n"+comment+"\n\n Link ordine: " + ordersStatesLink + "\n"+Settings.Greetings
                            mail.Body <- body
                            smtpClient.Send(mail)
                        with | err -> log.Error("errore email",err)

                    | _ -> ()
                }

            let _ = match form.SendConfirmationEmail with
                | Utils.YES -> Async.Start(sendEmail)
                | _ -> ()

            Redirection.FOUND Path.Orders.allConfirmedOrders

        )
    ]



let adminCourses pageNumber = warbler (fun _ ->
    let courses = LiteDb.getAllCourses() |> Seq.toList

    // let courses = LiteDb.getAllAvailableCoursesByPageAndNumberOfItemPerPage pageNumber (Settings.NumberOfItemForAminCoursesPage)


    let imagesForCourses = courses |> List.map (fun x -> (x.Id,
        match x.ImageName with 
            | Some name -> Some (get64EncodedImageFromStorage name)
            | None -> None
        ))
    let mapImagesForcourses = imagesForCourses |> Map.ofList    
    View.adminCourses courses mapImagesForcourses |> html
)





let removeCourse id = warbler (fun _ ->
    let _ = LiteDb.removeCourse id
    Redirection.FOUND (sprintf Path.Admin.aminCourses 0)
)

let editCourse id message = 
    let course = LiteDb.getCourse id
    choose [
        GET >=> (
            warbler (fun _ ->
            View.editCourse course message |> html
            )
        )
        POST >=> bindToForm Form.course (fun form ->
            let available = (form.Available = "Yes")
            match LiteDb.alreadyACourseWithThatNameAndDifferentId id form.Name with
            | false  -> LiteDb.modifyCourse id form.Name form.Description available form.UnityOfMeasure
                        Redirection.FOUND (sprintf Path.Admin.aminCourses 0)
            | true -> 
                       View.editCourse course ("il nome "+form.Name+ " esiste gia' in un altro piatto") |> html
        )
    ]


let makeMessageAsDefaut id =
    let _ = LiteDb.makeMessageAsDefault id
    Redirection.FOUND Path.Admin.manageWelcomeMessage

let makeMessageAsNotDefault id = 
    let _ = LiteDb.makeMessageAsNotDefault id
    Redirection.FOUND Path.Admin.manageWelcomeMessage


let deleteWelcomeMessage id =
    let _ = LiteDb.deleteWelcomeMessage id
    Redirection.FOUND Path.Admin.manageWelcomeMessage


let manageWelcomeMessage = 
    choose [
        GET >=> (
            warbler (fun _ ->
                let welcomeMessages = LiteDb.getAllWelcomeMessages()
                let defaultWelcomeMessage = welcomeMessages |> List.filter(fun x -> x.IsCurrentMessage) |> List.tryHead
                View.manageWelcomeMessage welcomeMessages defaultWelcomeMessage  |> html
            )
        )
        POST >=> bindToForm Form.welcomeMessage ( fun form ->
                     let _ = LiteDb.addWelcomMessage form.Message
                     Redirection.FOUND Path.Admin.manageWelcomeMessage
                )
        
    ] 
    


// warbler (fun _ ->
//     let welcomeMessages = LiteDb.getAllWelcomeMessages()
//     let defaultWelcomeMessage = welcomeMessages |> List.filter(fun x -> x.IsCurrentMessage) |> List.tryHead
//     View.manageWelcomeMessage welcomeMessages defaultWelcomeMessage  |> html
// )


let addCourse message =
    choose [
        GET >=> (
            warbler (fun _ ->
            View.addCourse  message |> html
            )
        )
        POST >=> bindToForm Form.course (fun form ->
            let available = (form.Available = "Yes")
            match LiteDb.alreadyExistsACourseWithName form.Name  with
            | None  ->  createCourse form.Name form.Description available form.Price form.UnityOfMeasure  |> ignore
                        Redirection.FOUND (sprintf Path.Admin.aminCourses 0)
            | Some x -> 
                        View.addCourse  ("il nome "+form.Name+ " esiste gia' in un altro piatto") |> html
        )
    ]




let newOrder =
    session (function 
        | NoSession -> Redirection.FOUND Path.Account.logon
        | UserLoggedOn { UserId = userId; Username = userName} -> 
            let user = LiteDb.getUser userId
            let newOrder = createOrder user userName
            // Redirection.FOUND (sprintf Path.Orders.editOrderRef (newOrder |> int))
            Redirection.FOUND (sprintf Path.Orders.completeMenuViewPaged (newOrder |> int) 0)
        | _ -> Redirection.FOUND Path.home
    )

let newOrderRef =
    session (function 
        | NoSession -> Redirection.FOUND Path.Account.logon
        | UserLoggedOn { UserId = userId; Username = userName} -> 
            let user = LiteDb.getUser userId
            let newOrder = createOrder user userName
            Redirection.FOUND (sprintf Path.Orders.editOrderRef (newOrder |> int))
        | _ -> Redirection.FOUND Path.home
    )


let cloneOrder id =
    session (function 
        | NoSession -> Redirection.FOUND Path.Account.logon
        | UserLoggedOn { UserId = userId; Username = userName} -> 
            let clonedOrder = LiteDb.cloneOrder id 
            Redirection.FOUND (sprintf Path.Orders.editOrderRef (clonedOrder |> int))
        | _ -> Redirection.FOUND Path.home
    )


let allOrders = warbler (fun _ ->
    let allOrders = LiteDb.getAllOrders()
    View.allOrders allOrders |> html
)

let allOpenOrders = warbler (fun _ ->
    let allOpenOrders = LiteDb.getAllOpenOrders()
    View.openOrders allOpenOrders |> html
)

let allConfirmedOrders = warbler (fun _ ->
    let allOpenOrders = LiteDb.getAllOpenOrders()
    View.confirmedOrders allOpenOrders |> html
)


let myOrders (userLoggedOn: UserLoggedOnSession) =
        let user = LiteDb.getUser userLoggedOn.UserId
        let myOrders = LiteDb.getAllUnarchivedOrdersOfAUser user 
        View.myOrders myOrders |> html

let removeOrder orderId (userLoggedOn: UserLoggedOnSession) = request (fun r ->
        let backUrl = 
            match r.queryParam Path.Store.backUrl with
            |  Choice1Of2 back -> WebUtility.UrlDecode(back)
            | Choice2Of2 _ -> Path.home

        let order = LiteDb.getOrderById orderId
        if (userLoggedOn.UserId = order.User.Id || userLoggedOn.Role = "Admin") then
                LiteDb.removeOrder orderId
        Redirection.FOUND backUrl 
)

let addCommentToOrderAndChangeItsState orderId = 
    View.addCommentToOrderAndChangeItsState orderId |> html



let makeOrderAsOngoing orderId = request (fun r ->
    let backUrl = 
            match r.queryParam Path.Store.backUrl with
            |  Choice1Of2 back -> WebUtility.UrlDecode(back)
            | Choice2Of2 _ -> Path.home
    let _ = LiteDb.makeOrderAsOngoing orderId
    Redirection.FOUND backUrl
)

let makeOrderAsDone orderId = request (fun r ->
    let backUrl = 
            match r.queryParam Path.Store.backUrl with
            |  Choice1Of2 back -> WebUtility.UrlDecode(back)
            | Choice2Of2 _ -> Path.home
    let _ = LiteDb.makeOrderAsDone orderId
    Redirection.FOUND backUrl
)

// let makeOrderAsArchived orderId = request (fun r ->
//     let backUrl = 
//             match r.queryParam Path.Store.backUrl with
//             |  Choice1Of2 back -> WebUtility.UrlDecode(back)
//             | Choice2Of2 _ -> Path.home
//     let _ = LiteDb.makeOrderAsArchived orderId
//     Redirection.FOUND backUrl
// )

let makeOrderAsArchived orderId = request (fun r ->
    let _ = LiteDb.makeOrderAsArchived orderId
    Redirection.FOUND Path.Orders.allConfirmedOrders
)


let aggregatedOpenAndOngoingOrders = warbler(fun _ ->
        let openAndOngoingOrders = LiteDb.getOpenAndOngoingOrders()
        let involvedOrderItems = (openAndOngoingOrders |> List.fold (fun acc x -> (LiteDb.getOrderItemsOfOrder x.Id |> Seq.toList)::acc) [] |> Seq.toList ) |> List.fold(@) []
        let itemNames = involvedOrderItems |> List.fold (fun acc x -> (x.Course.Name::acc)) [] |> Set.ofList |> Set.toList
        let itemQuantityPairs = itemNames |> 
            List.fold (fun acc x -> (x,(involvedOrderItems 
            |> List.filter (fun y -> y.Course.Name = x) 
            |> List.sumBy   (fun z ->  z.ItemQuantity )))::acc) []
        View.aggregateView itemQuantityPairs |> html
    )

let aggregatedOnlyOpenOrders = warbler(fun _ ->
        let openAndOngoingOrders = LiteDb.getOnlyOpenOrders()
        let involvedOrderItems = (openAndOngoingOrders 
            |> List.fold 
                (fun acc x -> (LiteDb.getOrderItemsOfOrder x.Id |> Seq.toList)::acc) [] |> Seq.toList ) |> List.fold(@) []
        let itemNames = involvedOrderItems |> List.fold (fun acc x -> (x.Course.Name::acc)) [] |> Set.ofList |> Set.toList
        let itemQuantityPairs = itemNames |> 
            List.fold (fun acc x -> (x,(involvedOrderItems 
            |> List.filter (fun y -> y.Course.Name = x) 
            |> List.sumBy   (fun z ->  z.ItemQuantity )))::acc) []
        View.aggregateView itemQuantityPairs |> html
    )


let aggregatedOnlyOpenOrdersWithScomposition = warbler(fun _ ->
        let openAndOngoingOrders = LiteDb.getOnlyOpenOrders()
        let involvedOrderItems = (openAndOngoingOrders 
            |> List.fold 
                (fun acc x -> (LiteDb.getOrderItemsOfOrder x.Id |> Seq.toList)::acc) [] |> Seq.toList ) |> List.fold(@) []

        let orderIdCustomerNameMap = openAndOngoingOrders |> List.fold (fun acc x -> (x.Id,x.User.UserName)::acc)[] |> Map.ofList

        let involvedItemNames = involvedOrderItems |> List.fold (fun acc x -> (x.Course.Name::acc)) [] |> Set.ofList |> Set.toList

        let involvedCustomerNames = involvedOrderItems |> List.fold (fun acc x -> (x.Course.Name::acc)) [] |> Set.ofList |> Set.toList

        let involvedOrderItemsPerItemNameAndCustomer = 
            involvedItemNames |> List.fold (fun acc name -> (name,(involvedOrderItems |> List.filter (fun (x:OrderItem) ->  x.Course.Name = name) |> List.map (fun (x:OrderItem) -> (orderIdCustomerNameMap.[x.OrderId],x.ItemQuantity,x.OrderId)  ) ) )::acc)[] |> Map.ofList

        let itemQuantityPairs = involvedItemNames |> 
            List.fold (fun acc x -> (x,(involvedOrderItems 
                |> List.filter (fun y -> y.Course.Name = x) 
                |> List.sumBy   (fun z ->  (z.ItemQuantity) )), involvedOrderItemsPerItemNameAndCustomer.[x]  )::acc) []
        View.aggregateViewWithCustomerScomposition itemQuantityPairs |> html

    )


let aggregatedOpenAndOngoingOrdersWithScomposition = warbler(fun _ ->
        let openAndOngoingOrders = LiteDb.getOpenAndOngoingOrders()
        let involvedOrderItems = (openAndOngoingOrders 
            |> List.fold 
                (fun acc x -> (LiteDb.getOrderItemsOfOrder x.Id |> Seq.toList)::acc) [] |> Seq.toList ) |> List.fold(@) []

        let orderIdCustomerNameMap = openAndOngoingOrders |> List.fold (fun acc x -> (x.Id,x.User.UserName)::acc)[] |> Map.ofList

        let involvedItemNames = involvedOrderItems |> List.fold (fun acc x -> (x.Course.Name::acc)) [] |> Set.ofList |> Set.toList

        let involvedCustomerNames = involvedOrderItems |> List.fold (fun acc x -> (x.Course.Name::acc)) [] |> Set.ofList |> Set.toList

        let involvedOrderItemsPerItemNameAndCustomer = 
            involvedItemNames |> List.fold (fun acc name -> (name,(involvedOrderItems |> List.filter (fun (x:OrderItem) ->  x.Course.Name = name) |> List.map (fun (x:OrderItem) -> (orderIdCustomerNameMap.[x.OrderId],x.ItemQuantity,x.OrderId)  ) ) )::acc)[] |> Map.ofList

        let itemQuantityPairs = involvedItemNames |> 
            List.fold (fun acc x -> (x,(involvedOrderItems 
                |> List.filter (fun y -> y.Course.Name = x) 
                |> List.sumBy   (fun z ->  z.ItemQuantity )), involvedOrderItemsPerItemNameAndCustomer.[x]  )::acc) []
        View.aggregateViewWithCustomerScomposition itemQuantityPairs |> html

    )


let unauthorized = 
    View.unauthorized |> html

let removeOrderItem orderItemId =
    let (userId,orderId) = getUserAndOrderIdOfOrderItem orderItemId
    session ( function
        | NoSession -> Redirection.FOUND Path.Account.logon
        | UserLoggedOn {UserId = theUserId; Role = userRole} when (userId = theUserId || userRole="Admin") ->
            let _ = LiteDb.removeOrderItem orderItemId 
            Redirection.FOUND (sprintf Path.Orders.editOrderRef orderId)
        | _ -> Redirection.FOUND Path.Orders.unauthorized
    )


let removeOrderItemRef orderItemId = request (fun r ->
    let (userId,orderId) = getUserAndOrderIdOfOrderItem orderItemId

    session ( function
        | NoSession -> Redirection.FOUND Path.Account.logon
        | UserLoggedOn {UserId = theUserId; Role = userRole} when (userId = theUserId || userRole="Admin") ->
            let _ = LiteDb.removeOrderItem orderItemId 
            Redirection.FOUND (sprintf Path.Orders.manageConfirmedOrder orderId)
        | _ -> Redirection.FOUND Path.Orders.unauthorized
    )
)


let editOrderItemRef orderItemId (userLoggedOn: UserLoggedOnSession)=
    let (userId,orderId) = LiteDb.getUserAndOrderIdOfOrderItem orderItemId
    let orderItem = LiteDb.getOrderItem orderItemId
    let allCourses = LiteDb.getAllAvailableCourses()
    if (userId=userLoggedOn.UserId || userLoggedOn.Role = "Admin") then
        choose [
            GET >=> warbler (fun _ ->
                View.editOrderItem orderItem allCourses |> html
            )
            POST >=> bindToForm Form.orderItemRef (fun form ->
                let _ = LiteDb.updateOrderItem orderItem ((int)form.CourseId) (form.Quantity) form.Comment
                Redirection.FOUND (sprintf Path.Orders.manageConfirmedOrder orderId) 
            )
        ]
    else 
        Redirection.FOUND Path.Orders.unauthorized

let editOrderItemFromMenuItem orderItemId (userLoggedOn: UserLoggedOnSession)=
    let pageNumber = 0
    let (userId,orderId) = LiteDb.getUserAndOrderIdOfOrderItem orderItemId
    let orderItem = LiteDb.getOrderItem orderItemId
    let allCourses = LiteDb.getAllAvailableCourses()
    if (userId=userLoggedOn.UserId || userLoggedOn.Role = "Admin") then
        choose [
            GET >=> warbler (fun _ ->
                View.editOrderItem orderItem allCourses |> html
            )
            POST >=> bindToForm Form.orderItemRef (fun form ->
                let _ = LiteDb.updateOrderItem orderItem ((int)form.CourseId) (form.Quantity) form.Comment
                Redirection.FOUND (sprintf Path.Orders.completeMenuViewPaged orderId pageNumber) 
            )
        ]
    else 
        Redirection.FOUND Path.Orders.unauthorized

let removeOrderItemFromCompleteMenu orderItemId pageNumber =
    let orderItem = LiteDb.getOrderItem orderItemId
    let order = LiteDb.getOrderById orderItem.OrderId
    let orderOwnerId = order.User.Id
    session (function
        | UserLoggedOn {UserId = X; Role = Y} when X = orderOwnerId || Y = "Admin" ->
            let _ = LiteDb.removeOrderItem orderItemId
            Redirection.FOUND (sprintf Path.Orders.completeMenuViewPaged order.Id pageNumber)
        | _ -> Redirection.FOUND Path.Orders.unauthorized
        
    
    )




let usersList = warbler ( fun _ ->
    // let users = LiteDb.getAllOrdinaryUsers() 
    let users = LiteDb.getAllUsers()  |> Seq.toList
    View.usersList users |> html

)

let deleteUser userId =
    let user = getUser userId
    match user.UserType with
    | Admin -> log.Error("can't remove admin account")
    | _ -> LiteDb.removeUser userId |> ignore
    Redirection.FOUND Path.Account.usersList

let changeMyPassword userId =
    choose [
        GET >=> (View.changeMyPassword |> html)
        POST >=> bindToForm Form.changePassword (fun form ->
            let (Password oldPassword) = form.OldPassword
            let (Password newPassword) = form.NewPassword
            let (Password confirmNewPassword) = form.ConfirmNewPassword
            let _ = LiteDb.changePassword userId (passHash oldPassword ) (passHash newPassword) 
            Redirection.FOUND Path.home

        )
    ]

let createNewAccount =
    choose [
        GET >=> (View.createNewAccount "" |> html)
        POST >=> 
            bindToForm Form.registerUser (fun form ->
            let userExists = LiteDb.getUserByName form.Username
            match userExists with
            | None ->
                let (Password password) = form.Password
                let _ = LiteDb.createUser form.Username (passHash password) form.PhoneNumber form.Address form.Email
                Redirection.FOUND Path.home
            | _ -> View.createNewAccount ("l'utente "+form.Username+" esiste gia'") |> html
            )
    ]

let editUser id =
    let user = LiteDb.getUser id
    choose [
        GET >=> (View.editUser user |> html)
        POST >=>
            bindToForm Form.modifyUser (fun form ->
                let userUpdated = {user with UserEmail = form.Email; UserPhone = form.PhoneNumber; Address = form.Address}
                let _ = LiteDb.updateUser userUpdated
                Redirection.FOUND Path.home
            
            )
    ]

type UrlPlainAndImgEncoded = {QrBase64EncodedUrl: string; PlainUrl: string}

let manageQrCodeUsers =  warbler (fun _ -> 
    let guid = Guid.NewGuid()
    let _ = LiteDb.createAutoSubscribedUser (guid.ToString())

    let accessCode = (Settings.HostAddress.ToString()+(Path.Account.autoRegisterUser.Substring(1))) |> Path.withParam ("codice",guid.ToString())

    let qrGenerator = new QRCoder.QRCodeGenerator(); 
    let qrCodeData  = qrGenerator.CreateQrCode(accessCode,QRCodeGenerator.ECCLevel.Q)
    let qrCode = new QRCode(qrCodeData);
    let qrCodeImage = qrCode.GetGraphic(20)
    let stream = new System.IO.MemoryStream()
    qrCodeImage.Save(stream,qrCodeImage.RawFormat)
    let arrayOfQrCode = stream.ToArray()
    let encoded = System.Convert.ToBase64String (arrayOfQrCode)

    let model = {QrBase64EncodedUrl=encoded; PlainUrl=accessCode}

    DotLiquid.page("qrCode.html") model
    // View.manageQrCodeUsers accessCode  |> html
)

let emailNotificationOfNewUserByUserName userName =
    let user = LiteDb.getUserByName userName
    let administrator = LiteDb.getAdministrator()
    let sendEmail =
        async {
            match (user,administrator.UserEmail) with 
                | (Some theUser,Some fromAddress) -> 
                  try
                    let smtpClient = new SmtpClient(Settings.SmtpAddress,Settings.SmtpPort)
                    smtpClient.Credentials <- System.Net.NetworkCredential(Settings.SmtpCredentialsName,Settings.SmtpCredentialsPassword)
                    smtpClient.DeliveryMethod <- SmtpDeliveryMethod.Network
                    smtpClient.EnableSsl <- true
                    let mail = new MailMessage()
                    mail.From <- MailAddress(fromAddress,Settings.EmailDisplayName)
                    mail.To.Add(fromAddress)
                    mail.CC.Add(fromAddress)
                    mail.Subject <- "nuovo utente registrato: "+userName
                    let body = "si e' autoregistrato il nuovo utente: "+userName
                    mail.Body <- body
                    smtpClient.Send(mail)
                  with | err -> log.Error("errore nella spedizione dell'email di notifica autoregistrazione utente "+userName)
                | _ -> ()
        }

    do Async.Start(sendEmail)


let autoRegisterUser =
    choose [
        GET >=>    
            request (fun x -> 
                match (x.queryParam "codice") with
                | Choice1Of2 code -> 
                    let autoSubscribedUser = LiteDb.findAutoSubscribedUserByGuidCode code
                    match autoSubscribedUser with
                    | Some X -> 
                        View.createNewAccount "inserisci i tuoi dati" |> html
                    | None -> Redirection.FOUND Path.Orders.unauthorized 
                | _ -> Redirection.FOUND Path.Orders.unauthorized
            )
        POST >=> request (fun x ->
            match (x.queryParam "codice") with
                | Choice1Of2 code -> 
                    let autoSubscribedUser = LiteDb.findAutoSubscribedUserByGuidCode code
                    match autoSubscribedUser with
                    | Some X ->
                        bindToForm Form.registerUser (fun form ->
                            let userExists = LiteDb.getUserByName form.Username
                            match userExists with
                            | None ->
                                let (Password password) = form.Password
                                let newUser = LiteDb.createUserByGuidCode form.Username (passHash password) form.PhoneNumber form.Address form.Email code
                                let _ = emailNotificationOfNewUserByUserName form.Username
                                match newUser with
                                    | Some theNewUser -> 
                                        authenticateUserAndGoToUrl theNewUser Path.home
                                    | None -> Redirection.FOUND Path.home
                            | _ -> View.createNewAccount ("l'utente "+form.Username+ " esiste gia'" ) |> html
                        )
                    | _ -> Redirection.FOUND Path.Orders.unauthorized
                | _ -> Redirection.FOUND Path.home
            )
    ]


let imageView =
    let image = LiteDb.get_test_image_as_stream()
    let stream = new System.IO.MemoryStream()
    do image.CopyTo(stream)
    let arrayOfImage = stream.ToArray()
    let encoded = System.Convert.ToBase64String (arrayOfImage)
    let model = {QrBase64EncodedUrl = encoded; PlainUrl="blabla"}
    DotLiquid.page("qrCode.html") model

let imageCourseRemover (id:int) =
    let course = LiteDb.getCourse id
    let courseReplacement = {course with ImageName = None; EncodedImage = None}
    LiteDb.updateCourse courseReplacement |> ignore
    let _ = match course.ImageName with
        | Some X -> LiteDb.removeImage X |> ignore
        | _ -> ()
    Redirection.FOUND (sprintf Path.Admin.aminCourses 0)

let courseImageUpload (id:int) =
    choose [
        GET >=> (
            View.courseImageUpload id |> html
        )
        POST >=> request (fun x -> 

            let uploadedFile = x.files |> List.filter (fun y -> y.fieldName = "imagefile") |> List.tryHead
            let imageName = "image_"+((string)id)
            let _ = 
                match uploadedFile with 
                | Some X -> 
                    uploadCourseImage imageName X.tempFilePath id |> ignore
                    let course = LiteDb.getCourse id
                    // let newCourse = { course with ImageName = Some imageName; EncodedImage = Some (get64EncodedImageFromStorage imageName)  }
                    let newCourse = { course with ImageName = Some imageName}
                    LiteDb.updateCourse newCourse |> ignore
                    ()
                | _ -> ()

            // do printf "%s" (uploadedFile.ToString())
            // do x.files |> List.iter (fun x -> x.
            // | Choice1Of2 X -> printf "%s\n" X
            // | _ -> printf "none"

            // printf "nononon\n"
            // do x.headers |> List.iter (fun x -> printf "%s\n" (x.ToString()))
            // let you = x.queryParam("file1")
            // printf "--\n"
            // do printf "%s\n" (you.ToString())
            // do match you with
            //     | Choice1Of2 x -> printf "%s\n" (x.ToString())
            //     | _ -> printf "ciao\n"

            Redirection.FOUND (sprintf Path.Admin.aminCourses 0)
            // View.courseImageUpload id |> html
        )
    ]

// let paypalTransactionComplete = warbler (fun (x:HttpContext) ->
//     log.Debug("transaction called")
//     do x.request.form |> List.iter (fun x -> printf "%s\n" (x.ToString()))
//     Redirection.FOUND Path.home
// )






let detectCupon cuponGuidString =  warbler (fun _ ->
    let lookedUpCupon = LiteDb.findCuponByGuid cuponGuidString
    let now = System.DateTime.Now
    let nextAvailableSlot = LiteDb.nextSlotStartingFrom now

    log.Debug(nextAvailableSlot.ToString())

    let cuponContainsNextSlot =
        match (lookedUpCupon,nextAvailableSlot) with
        | (Some X,Some Y) -> List.contains Y (X.Slots)
        | _ -> false

    // log.Debug(sprintf "prossimo slot: %s" (nextAvailableSlot.ToString()) )

    let (msg,validity,someId) = match lookedUpCupon with
            | Some x -> 
                match x.Used with
                    | true -> ("il cupon e' stato gia' usato",false,Some x.Id)
                    | false ->
                        (let strSlots = x.Slots |> List.map (fun (x:LiteDb.Slot) -> ((x.DateTime.ToString())+", ")) |>  List.fold (fun acc x -> acc + x ) ""
                        "cupon riconosciuto, valido per pesone:  " + ((string)x.NumberOfPeople)+
                        "nei seguenti slot: "+strSlots, true,Some x.Id)
            | None -> ("cupon non valido",false,None)
    View.detectCupon msg validity someId cuponContainsNextSlot lookedUpCupon  |> html
)

// 4 casi:  - cupon valido per la prossima tratta
// - cupon valido ma non per la prossima tratta
// - cupon gia' usato
// - cupon non valido

// per ora solo due casi: valido per la prossima tratta o tutto il resto

let makeCuponAsUsed id = warbler (fun _ ->
    log.Debug("make cupon used")
    LiteDb.makeCuponAsUsed id |> ignore
    Redirection.FOUND  ("https://" + Settings.ServerAddress + ":443") //    "https://192.168.44.153:443"
)


type RecognizeCuponModel = {recognize: bool;serverAddress: string; 
    validSlotsTime: string; isValidCupon: bool;ownerEmail: 
    string;cuponId: int;suaveService: string;
    nodeService: string }

let error = 
    View.error |> html

let noMoreCuponByThisUser =
    View.noMoreCuponByThisUser |> html

let detectCuponRef cuponGuidString = warbler (fun _ ->
    let lookedUpCupon = LiteDb.findCuponByGuid cuponGuidString
    let now = System.DateTime.Now
    let nextAvailableSlot =  LiteDb.nextSlotStartingFrom now

    let (slotsForCupon:Slot list) = match lookedUpCupon with
        | Some X -> X.Slots |> List.sortBy (fun x -> x.DateTime) |> List.filter (fun x -> System.DateTime.Compare(now,x.DateTime) <=0)
        | _ -> []

    let ownerEmail = match lookedUpCupon with
        | Some X -> match X.OwnerEmail with | Some Y -> Y | _ -> ""
        | _ -> ""

    let slotsValidityOfCupon = slotsForCupon |> List.fold (fun acc x -> acc + (x.DateTime).ToString()) ""

    let cuponId = match lookedUpCupon with | Some X -> X.Id | _ -> -1

    let cuponIsValid = match lookedUpCupon with | Some X -> not X.Used  | _ -> false

    let cuponContainsNextSlot =         
        match (lookedUpCupon,nextAvailableSlot) with
        | (Some X,Some Y) -> List.contains Y (X.Slots)
        | _ -> false

    let myModel = {recognize = cuponContainsNextSlot;serverAddress=Settings.ServerAddress; 
        validSlotsTime = slotsValidityOfCupon; isValidCupon = cuponIsValid; ownerEmail = ownerEmail; 
        cuponId=cuponId; suaveService = (Settings.SuaveService |> string); nodeService= (Settings.NodeService |> string)    }
    DotLiquid.page("recognizedCupon.html") myModel
)



let adminExcursions = warbler (fun _ ->
    let periodicalExcursions = LiteDb.getAllPeriodicalExcursions() 
    View.adminExcursions periodicalExcursions |> html
)

type GroupOfNewCupons = {
    Quantity: int
    NumberOfPeople: int
    DiscountType: string
    DiscountAmount: decimal
    NumberOfCupons: int
    PickableSlots: Slot list
    DateInit: string
    DateEnding: string
}



let adminCupons = 
    choose [
        GET >=> warbler (fun _ ->
            DotLiquid.page("timePeriodPickerForCupon.html")  ({ a = ""})
        )
        POST >=> bindToForm Form.cuponPickerInterval (fun form ->

            let initTime = System.DateTime.Parse(form.DateInit,CultureInfo.CreateSpecificCulture("en-US"))
            let endingTime = System.DateTime.Parse(form.DateEnding,CultureInfo.CreateSpecificCulture("en-US"))
            let adjustedEndingTime = endingTime.AddDays((float)1)

            let slots = LiteDb.getExistingSlotsOfAPeriod initTime  adjustedEndingTime |> Seq.toList
            log.Debug(sprintf "grandezza slot size: %d" (List.length slots))
            let newCuponGroup = { Quantity=(int)form.NumberOfCupon; NumberOfPeople=(int)form.NumberOfPeople;
                DiscountType =  form.TypeOfDiscount; DiscountAmount = form.DiscountValue; NumberOfCupons = (int)form.NumberOfCupon; 
                PickableSlots = slots; DateInit = form.DateInit; DateEnding = form.DateEnding}

            DotLiquid.page("assignCupon.html") newCuponGroup



            // Redirection.FOUND Path.home
            // let dateInit = System.DateTime.Parse(form.DateInit, CultureInfo.CreateSpecificCulture("en-US"))
            // let dateEnd = System.DateTime.Parse(form.DateEnding, CultureInfo.CreateSpecificCulture("en-US"))

        )

            // Redirection.FOUND Path.home
    ]

let removePeriodicalExcursion id =
    // let excursion = LiteDb.getPeriodicalExcursion id
    let _ = LiteDb.deletePeriodicalExcursion id
    Redirection.FOUND Path.Admin.adminExcursions


let lookForCuponPage lang = warbler ( fun _ ->

    // let customLocal = match Map.tryFind(lang) mapOfLocals  with | Some X -> X | _ -> defaultLocal

    let now = System.DateTime.Now
    let unclaimedCupons = LiteDb.getUnclaimedCuponsWithSomSlotsStartingFrom now |> Seq.toList

    View.lookForCuponPage unclaimedCupons lang |> html
)


type OrderId = {OrderID: string}

let paypalTransactionComplete = 
    choose [
             POST >=> request (fun (x:Suave.Http.HttpRequest) ->
                 (
                    let passedFormKeys = x.form |> List.map (fun (x,_) -> x)

                    let _ = passedFormKeys |> List.iter (fun x -> printf "%s\n" x)

                    let orderCaptured = passedFormKeys |> List.filter (fun x -> 
                                try 
                                    let jsonob2 = Newtonsoft.Json.JsonConvert.DeserializeObject x
                                    printf "1 %s" (jsonob2.ToString())
                                    let jsonob3 = jsonob2 :?> Newtonsoft.Json.Linq.JObject
                                    printf "2 %s" (jsonob3.ToString())
                                    printf "3%s\n" (jsonob3.GetType().ToString())
                                    jsonob3.ContainsKey("orderID") 
                                with | _ -> false
                    )

                    printf "%s\n" "order captured1"

                    let _ = orderCaptured |>  List.iter (fun x -> printf "%s\n" (x.ToString()))

                
                    let _ =  printf "%s order captured 2"

                    let orderCapturedWrapped = orderCaptured |> List.map (fun x -> 
                        let jsonObject = Newtonsoft.Json.JsonConvert.DeserializeObject x
                        printf "here\n"
                        let jsonOb2 = jsonObject :?> Newtonsoft.Json.Linq.JObject 
                        printf "here2\n"
                        {OrderID = jsonOb2.GetValue("orderID").ToString()}
                        )

                    let firstOrderCaptured = orderCapturedWrapped |> List.tryHead



                    let _ = match firstOrderCaptured with
                        | Some X -> printf "%sXXX\n" (X.ToString())
                        | None -> printf "no order captured\n"

                    let orderId = match firstOrderCaptured with
                        | Some X -> Some X.OrderID
                        | None -> None



                    let _ = match orderId with
                    | Some X -> 
                        printf "eccolo %s"  X
                        let request = new OrdersGetRequest(X)

                        let executeResp = async {
                             let res = Utils.PayPalClient.client().Execute(request) 
                            //  res.Exception
                             printf "%s\n" ((res.Status).ToString())
                             printf "idd   : %s\n" ((res.Id).ToString())
                             printf "idd   : %s\n" ((res.Exception).ToString())

                            // do response.RunSynchronously()
                        }
                        Async.Start executeResp

                        printf "dopo async\n"


                        let event = new System.Threading.AutoResetEvent(false)
                        let timer = new System.Timers.Timer(2000.0)
                        timer.Elapsed.Add (fun _ -> event.Set() |> ignore )
                        timer.Start()
                        event.WaitOne() |> ignore

                        // printf " status: %s\n" (response.Status.ToString())
                        // printf " result: %s\n" (response.RunSynchronously())

                        // let response = async { 
                        //         let me = Utils.PayPalClient.client().Execute(request)  
                        //         let _ = printf "called client\n"
                        //         // let _ = printf "%s headers: \n" (me.Result.Headers.ToString())
                        //         // let _ = printf "%s\n" (me.Result.StatusCode.ToString())
                        //         me
                        // }
                        // let me = Async.Start(response)


                        // printf "%b complete here xxx \n"  response.IsCompleted
                        // let result = response.
                        // let _ = result.Headers.ToList() |>  Seq.iter (fun x -> (printf "head: %s" (x.Key)))
                        // let b = result.StatusCod

                        ()
                    | None -> ()




                    
                    Redirection.FOUND Path.home
                 )
             )
            
             GET >=> (
                Redirection.FOUND Path.home
             )
    ]

        


// warbler (fun (x:HttpContext) ->
//     log.Debug("transaction called")
//     do x.request.form |> List.iter (fun x -> printf "%s\n" (x.ToString()))
//     Redirection.FOUND Path.home
// )


// let daysOfWeekFromFormOption daysOptions accumul  = 
//     match daysOptions with
//         | Some X::T -> 



// let daysOfWeekFromOption daysOption accumul =

// let myDate = System.DateTime.Parse(date,CultureInfo.CreateSpecificCulture("en-US"))

type QrDisplayer = {EncodedQrImage: string}

let displayQrOfCupon id =
    let cupon = LiteDb.getCupon id 

    let qrGenerator = new QRCoder.QRCodeGenerator(); 
    let qrCodeData  = qrGenerator.CreateQrCode(cupon.CuponGuid.ToString(), QRCodeGenerator.ECCLevel.Q)
    let qrCode = new QRCode(qrCodeData);
    let qrCodeImage = qrCode.GetGraphic(20)
    let stream = new System.IO.MemoryStream()
    qrCodeImage.Save(stream,qrCodeImage.RawFormat)
    let arrayOfQrCode = stream.ToArray()
    let encoded = System.Convert.ToBase64String (arrayOfQrCode)
    let model = {EncodedQrImage=encoded}
    DotLiquid.page("cuponQrdisplay.html") model


    

let rec datesOfAnInterval (visitTime: System.DateTime) (startingDate:System.DateTime) (endingDate:System.DateTime) (daysOfWeek: Set<DayOfWeek>)  (accumulator: System.DateTime list ) =
    if (System.DateTime.Compare(startingDate,endingDate) >= 0)  
        then accumulator
    else (
        let current =  DateTime(startingDate.Year,startingDate.Month,startingDate.Day,visitTime.Hour,visitTime.Minute,visitTime.Second)
        if (Set.contains current.DayOfWeek daysOfWeek) 
            then datesOfAnInterval visitTime (startingDate.AddDays((float)1)) endingDate daysOfWeek (current::accumulator)
            else datesOfAnInterval visitTime (startingDate.AddDays((float)1)) endingDate daysOfWeek  accumulator
    )


let viewCupons =  warbler (fun _ ->
        let allCupons = getAllValidCupons() |> Seq.toList
        View.viewCupons allCupons |> html 
    )

let cuponToSlots = warbler (fun x ->
    let initDate = x.request.queryParam("dateInit")
    let endingDate = x.request.queryParam("dateEnding")
    
    let initDateVal = 
        match initDate with   
        | Choice1Of2 X -> X
        | _ -> log.Error("init date parsing error using default") 
               "5/1/2019"

    let endDateVal = 
        match endingDate with   
        | Choice1Of2 X -> X
        | _ -> log.Error("end date parsing error using default") 
               "11/1/2019"

    let numberOfPeople = 
        match x.request.queryParam("numberOfPeople") with
        | Choice1Of2 X -> ((int)X)
        | _ -> log.Error("can't parse param number of people")     
               1

    let discountValue = 
        match x.request.queryParam("discountValue") with
        | Choice1Of2 X -> ((decimal)X)
        | _ -> log.Error("can't parse param discount value")
               10M

    let discountType =
        match x.request.queryParam("typeOfDiscount") with
        | Choice1Of2 X -> X
        | _ -> log.Error("can't parse param typeOfDiscount")
               Utils.PERCENT

    let numberOfCupons =
        match x.request.queryParam("numberOfCupon") with
        | Choice1Of2 X -> (int)X
        | _ -> log.Error("can't parse param numberOfCupon")
               1 

    // now parse all the slots:
    let parsedInitDate = System.DateTime.Parse(initDateVal,CultureInfo.CreateSpecificCulture("en-US"))
    let parsedEndDate = System.DateTime.Parse(endDateVal,CultureInfo.CreateSpecificCulture("en-US"))

    let candidateSlots = LiteDb.getExistingSlotsOfAPeriod parsedInitDate parsedEndDate

    let candidateSlotsIdQueriesParams = candidateSlots |> Seq.map (fun x -> "SLOT_"+ (string)x.Id) 

    let actualSlotsForCuponOptions = candidateSlotsIdQueriesParams |> Seq.map (fun slotPar ->
        match x.request.queryParam(slotPar) with 
        | Choice1Of2 _ -> Some (slotPar.Substring("SLOT_".Length))
        | _ -> None
        ) 

    let actualSlotsIdsForCupons = actualSlotsForCuponOptions |> Seq.filter (fun z -> z.IsSome) |> Seq.map (fun z -> (int)z.Value) |> Seq.toList
    let slotsForCupons = getSlotsGivenIds actualSlotsIdsForCupons |> Seq.toList

    let theDiscount = match discountType with
        | Utils.PERCENT -> LiteDb.Percentage discountValue
        | _ -> LiteDb.Amount discountValue

    //  of LiteDb.Amount  10M


    [1 .. numberOfCupons] |> List.iter (fun _ -> (
        let guid = Guid.NewGuid()
        
        // let qrGeneraor = new QRCoder.QRCodeGenerator()
        // let qrCodeData = qrGeneraor.CreateQrCode(guid.ToString(),QRCodeGenerator.ECCLevel.Q)
        // let qrCode = new QRCode(qrCodeData)
        // let qrCodeImage = qrCode.GetGraphic(20)
        // let stream = new System.IO.MemoryStream()
        // qrCodeImage.Save(stream,qrCodeImage.RawFormat)

        // // let arrayOfQrCode = stream.ToArray()

        
        createCupon numberOfPeople theDiscount slotsForCupons guid) |> ignore
        
        )

    // now create all the cupons:
    Redirection.FOUND Path.Admin.adminCupons

    // View.cuponToSlots |> html
)



// let getNextSlotTime (excursion:LiteDb.PeriodicalExcursion) (currentTime:DateTime) =
//     let getNextDay excursion =


// let creatSlotsOfExcursion (excursion:LiteDb.PeriodicalExcursion) =

let claimCupon id =
    let cupon = LiteDb.getCupon id
    choose [
        GET >=> warbler ( fun _ ->
            DotLiquid.page("claimCupon.html") {a=""}
        )
        POST >=> bindToForm Form.subscribeForCupon (fun form ->

            let cupon = {cupon with OwnerEmail = Some form.Email}


            let areadyAskedCupon = LiteDb.thereAreActiveCuponByThisEmail form.Email

            if (areadyAskedCupon) then
                Redirection.FOUND Path.Cupon.noMoreCuponByThisUser
            else  
            
            // must update only if email is sent
            LiteDb.updateCupon (cupon) |> ignore
            let cuponQrCode64Encoded = Utils.stringToBase64Qr (cupon.CuponGuid.ToString())
            let cuponImg = Utils.stringToImageQr (cupon.CuponGuid.ToString())

// <img src="data:image/bmp;base64, {{model.qr_base64_encoded_url}} " width="400"/>
            // remove spooled bmp files
            let filesToBeRemoved = System.IO.Directory.GetFiles(".","*.bmp")
            let _ = filesToBeRemoved |> Array.iter  (fun x -> System.IO.File.Delete(x))

            let cuponTimes = cupon.Slots |> List.map (fun (x:LiteDb.Slot) -> x.DateTime)
            let cuponText = cuponTimes |> List.fold (fun acc x -> acc + (x.ToString())+", " ) ""

            let sendEmail = 
                async {
                    let smtpClient = new SmtpClient(Settings.SmtpAddress,Settings.SmtpPort)
                    smtpClient.Credentials <- System.Net.NetworkCredential(Settings.SmtpCredentialsName,Settings.SmtpCredentialsPassword)
                    smtpClient.DeliveryMethod <- SmtpDeliveryMethod.Network
                    smtpClient.EnableSsl <- true
                    let mail = new MailMessage()
                    mail.From <- MailAddress(Settings.OwnerEmail,"Capri Island Tour")
                    mail.To.Add(form.Email)
                    mail.Subject <- "cupon"
                    // mail.IsBodyHtml <- true

                    let bitmapImage = new Bitmap(cuponImg)
                    
                    // let attachment = new Attachment(bigtapImage,Mime.MediaTypeNames.Image.Tiff)
                    // let outFile =  new System.IO.StreamWriter("out.bmp")

                    // bitmapImage.Save("out.bmp")
                    bitmapImage.Save(cupon.CuponGuid.ToString()+".bmp")
                
                    // outFile.Write(cuponImg.ToArray())
                    // outFile.Flush()

                    let attachment = new Attachment(cupon.CuponGuid.ToString()+".bmp")
                    
                    // let attachment = new Attachment(cuponImg,Mime.ContentType)

                    mail.Attachments.Add(attachment)

                    mail.Body <- "https://fioreseaexcursionscapri.com. Cupon richiesto in allegato valido per le seguenti date/orari :"  + cuponText + 
                    "Here enclosed the personal cupon valid for the following dates/times. 
                    Print it or show it when buying the ticket 
                      "
                        // <img src=\"data:image/bmp;base64,"+cuponQrCode64Encoded+"\" width=\"400\"/>"
                    smtpClient.Send(mail)
                    LiteDb.updateCupon (cupon) |> ignore
                }
            try 
                let _ = Async.RunSynchronously sendEmail
                Redirection.FOUND Path.home
            with err ->
                log.Error ((err.ToString())+"errore nel tentativo di spedire il cupon")
                Redirection.FOUND Path.Cupon.error
        )
    ]


let voidCupon id =
    let cupon = LiteDb.getCupon id
    let usedCupon = {cupon with Used = true}
    LiteDb.storeCupon usedCupon |> ignore
    Redirection.FOUND Path.home

let removeCupon id = 
    LiteDb.removeCupon id |> ignore
    Redirection.FOUND Path.Admin.viewCupons

let createPeriodicalExcursion = 
    choose [
        GET >=> warbler (fun _ ->
            let myDummy = {a = "excursioni"}
            DotLiquid.page("periodicalExcursion.html") myDummy
        )   
        POST >=> bindToForm Form.periodicalExcursion ( fun form ->
            // let periodicalExcursion = LiteDb.createPeriodicalExcursion (System.DateTime.Parse(form.Time)) [] form.Seats
            let dateInit = System.DateTime.Parse(form.DateInit,CultureInfo.CreateSpecificCulture("en-US"))
            let dateEnding = System.DateTime.Parse(form.DateEnding,CultureInfo.CreateSpecificCulture("en-US"))
            let id = LiteDb.createPeriodicalExcursion form.Name dateInit dateEnding (System.DateTime.Parse(form.Time)) [] ((int)form.Seats)
            let _ = match form.Monday with
                    | Some X -> LiteDb.addDayToPeriodicalExcursion id DayOfWeek.Monday
                    | None -> false
            let _ = match form.Tuesday with
                    | Some X -> LiteDb.addDayToPeriodicalExcursion id DayOfWeek.Tuesday
                    | None -> false
            let _ = match form.Wednesday with
                    | Some X -> LiteDb.addDayToPeriodicalExcursion id DayOfWeek.Wednesday
                    | None -> false
            let _ = match form.Thursday with
                    | Some X -> LiteDb.addDayToPeriodicalExcursion id DayOfWeek.Thursday
                    | None -> false
            let _ = match form.Friday with
                    | Some X -> LiteDb.addDayToPeriodicalExcursion id DayOfWeek.Friday
                    | None -> false
            let _ = match form.Saturday with
                    | Some X -> LiteDb.addDayToPeriodicalExcursion id DayOfWeek.Saturday
                    | None -> false
            let _ = match form.Sunday with
                    | Some X -> LiteDb.addDayToPeriodicalExcursion id DayOfWeek.Sunday
                    | None -> false

            let excursion = LiteDb.getPeriodicalExcursion id

            let timesSlots  =  datesOfAnInterval excursion.Time (excursion.DateInit) (excursion.DateEnding) (excursion.DaysOfWeek |> Set.ofList) []

            let _ = timesSlots |> List.iter (fun x -> LiteDb.createSlot excursion x )
            // let slots = LiteDb.findSlotOfAnExcursionByExcursionId excursion.Id
            let slots = LiteDb.findSlotsByExcursion excursion
            // log.Debug(sprintf "slots size %d " (Seq.length slots))



            // debug

        
            Redirection.FOUND Path.Admin.adminExcursions)

    ]
    // choose [
    //     GET >=>
    //         let myDummy = {a = "excursioni"}
    //         DotLiquid.page("periodicalExcursion.html") myDummy
    //     POST >=> bindToForm Form.periodicalExcursion ( fun form ->
    //         // let daysOptions = [form.Monday;form.Tuesday;form.Wedensday;form.Thurdsay;form.Friday;form.Saturday;form.Sunday]
    //         // let allDays = daysOptions |> List.fold (fun acc x -> (fun x -> match x with Some X -> ) )

        
    //         Redirection.FOUND Path.home
    //     )
    // ]





type CourseIdModel = {CourseId: int}
let imageCourseUploader id =
    let o = {CourseId= id}
    DotLiquid.page("courseImgUploader.html") o


let webPart = 
    choose [

        // pathScan Path.Service.changeLocal (fun local -> (changeLocal local ))
        pathScan Path.Service.changeLocal (fun lang_code -> (changeLocal lang_code))

        path Path.Admin.adminExcursions >=> adminExcursions
        // pathScan Path.Cupon.detectCupon (fun id -> admin (detectCupon id) )
        pathScan Path.Cupon.detectCupon (fun id -> admin (detectCuponRef id) )

        path Path.Cupon.error >=> error
        path Path.Cupon.noMoreCuponByThisUser >=> noMoreCuponByThisUser

        pathScan Path.Cupon.makeCuponAsUsed (fun id -> admin (makeCuponAsUsed id))

        pathScan Path.Cupon.claimCupon (fun id -> (claimCupon id))
        pathScan Path.Cupon.removeCupon (fun id -> (removeCupon id))

        pathScan Path.Cupon.voidCupon (fun id -> admin (voidCupon id))

        pathScan Path.Cupon.lookForCuponPage (fun lang -> lookForCuponPage lang)

        pathScan Path.Admin.removePeriodicalExcursion (fun id -> admin (removePeriodicalExcursion id))
        path Path.Admin.createPeriodicalExcursion >=> createPeriodicalExcursion
        path Path.Admin.adminCupons >=> adminCupons

        path Path.Admin.cuponToSlots >=> cuponToSlots

        path Path.Admin.viewCupons >=> viewCupons

        pathScan Path.Cupon.displayQrOfCupon (fun id -> admin (displayQrOfCupon id))

        path Path.Sandbox.imageView >=> imageView

        path Path.Orders.paypalTransactionComplete  >=> paypalTransactionComplete

        pathScan Path.Admin.imageCourseUploader (fun id -> admin (imageCourseUploader id))

        pathScan Path.Admin.courseImageUpload (fun id -> admin (courseImageUpload id))


        pathScan Path.Orders.payWithPayPal (fun id -> (payWithPayPal id))

        path Path.home >=> anyUserLoggedOn home
        pathScan Path.Admin.imageCourseRemover (fun id -> admin (imageCourseRemover id))

        path Path.Account.logon >=> logon
        path Path.Account.logoff >=> reset
        path Path.Account.createNewAccount >=> admin createNewAccount
        path Path.Account.manageQrCodeUsers >=> admin manageQrCodeUsers

        pathScan Path.Account.editUser (fun id -> editUser id)

        path Path.Account.autoRegisterUser >=> autoRegisterUser

        path Path.Account.usersList >=> admin usersList

        path Path.Orders.newOrder >=> loggedOn newOrder

        pathScan Path.Admin.removeCourse  (fun id -> admin (removeCourse id))
        pathScan Path.Admin.editCourse (fun id -> admin (editCourse id ""))
        path Path.Admin.addNew >=> admin (addCourse "")
        path Path.Admin.manageWelcomeMessage >=> admin (manageWelcomeMessage)

        pathScan Path.Admin.removeWelcomeMessage (fun id -> admin (deleteWelcomeMessage id))
        pathScan Path.Admin.makeMessageAsDefault (fun id -> admin (makeMessageAsDefaut id))
        pathScan Path.Admin.makeMessageAsNotDefault (fun id -> admin (makeMessageAsNotDefault id))

        pathScan Path.Admin.aminCourses  (fun id -> admin (adminCourses id))

        path Path.Orders.allConfirmedOrders >=> admin allConfirmedOrders

        path Path.Orders.newOrderRef >=> loggedOn newOrderRef

        path Path.Account.autoRegisterUser >=> autoRegisterUser

        // pathScan Path.Orders.editOrder (fun id -> editOrder id )

        pathScan Path.Orders.editOrderRef (fun id -> editOrderRef id)

        pathScan Path.Orders.goToConfirmOrder (fun id -> goToConfirmOrder id)
        pathScan Path.Orders.confirmOrder (fun id -> confirmOrder id)

        pathScan Path.Orders.makeOrderAsOngoingRef (fun id -> admin (makeOrderAsOngoingRef id))
        pathScan Path.Orders.makeOrderAsDoneRef (fun id -> admin (makeOrderAsDoneRef id))

        // pathScan Path.Orders.completeMenuView (fun id -> (completeMenuView id))

        pathScan Path.Orders.completeMenuViewPaged (fun (id,pageNumber) -> (completeMenuViewPaged id pageNumber))


        pathScan Path.Orders.decreaseOrderItemQuantity (fun (id,pageNumber) -> (decreaseOrderItemQuantity id pageNumber))
        pathScan Path.Orders.increaseOrderItemQuantity (fun (id,pageNumber) -> (increaseOrderItemQuantity id pageNumber))


        pathScan Path.Orders.addItemFromDetailedMenu (fun (orderId,courseId,pageNumber) -> (addItemFromDetailedMenu orderId courseId pageNumber))


        pathScan Path.Orders.editOrderItemFromMenuItem (fun id -> anyUserLoggedOn (editOrderItemFromMenuItem id))


        pathScan Path.Account.removeUser (fun id -> admin (deleteUser id))
        pathScan Path.Account.changePassword (fun id -> (changeMyPassword id))

        pathScan Path.Orders.cloneOrder (fun id -> cloneOrder id)

        path Path.Orders.aggregatedOpenAndOngoingOrders >=> admin aggregatedOpenAndOngoingOrdersWithScomposition

        path Path.Orders.aggregatedOnlyOpenOrders >=> admin aggregatedOnlyOpenOrdersWithScomposition

        pathScan Path.Orders.viewOrder (fun id -> admin (viewOrder id))
        pathScan Path.Orders.rejectOrder (fun id -> admin (rejectOrder id))

        path Path.Orders.unauthorized >=> unauthorized
        pathScan Path.Orders.removeOrderItem  removeOrderItem
        pathScan Path.Orders.editOrderItem (fun id -> anyUserLoggedOn (editOrderItem id))
        pathScan Path.Orders.editOrderItemRef (fun id -> anyUserLoggedOn (editOrderItemRef id))
        pathScan Path.Orders.removeOrderItemRef (fun id -> admin (removeOrderItemRef id))

        pathScan Path.Orders.removeOrderItemFromCompleteMenu (fun (id,pageNumber) -> (removeOrderItemFromCompleteMenu id pageNumber))


        pathScan Path.Orders.manageConfirmedOrder (fun id -> admin (manageConfirmedOrder id))
        pathScan Path.Orders.manageConfirmedOrderWithEmail (fun id -> admin (manageConfirmedOrderWithEmail id))

        
        path Path.Orders.myOrders >=> anyUserLoggedOn  myOrders

        pathScan Path.Orders.removeOrder (fun id -> anyUserLoggedOn (removeOrder id))

        pathScan Path.Orders.makeOrderAsOnGoing (fun id -> admin (makeOrderAsOngoing id))
        pathScan Path.Orders.makeOrderAsDone (fun id -> admin (makeOrderAsDone id))
        pathScan Path.Orders.makeOrderAsAchived (fun id -> admin (makeOrderAsArchived id))

        path Path.Orders.allOrders >=> admin allOrders
        path Path.Orders.allOpenOrders >=> admin allOpenOrders



        pathRegex "(.*)\.(css|png|gif|js|jpeg)" >=> Files.browseHome
        html View.notFound
    ]


let _ = [] |> System.Linq.Enumerable.Count
    
let cfg =
  { defaultConfig with
      bindings = [ HttpBinding.createSimple HTTP "0.0.0.0" 8083  ] }


DotLiquid.setTemplatesDir ("liquidtemplates")

startWebServer cfg webPart