module DigitalCupons.LiteDb

open System
open System.Linq.Expressions
open LiteDB
open LiteDB.FSharp
open LiteDB.FSharp.Extensions
open Control.LazyExtensions 
open FSharp.Configuration
open DotLiquid.Tags


type Settings = AppSettings<"App.config">

let mapper = FSharpBsonMapper()
let liteDb = new LiteDatabase("simple.db",mapper);

type UserType = Admin | Ordinary | Customer
type OrderState = Editing | Confirmed | Rejected | Accepted | Ongoing | Done | Archived
type UnityOfMeasures = Pezzo | Kilo | Etto | Confezione


// type DayOfWeek = Monday | Tuesday | Wedensday | Thursday | Friday | Saturday | Sunday

let log = log4net.LogManager.GetLogger(System.Reflection.MethodBase.GetCurrentMethod().DeclaringType);



// tonyx DEBUG
// ============
// let _ = liteDb.FileStorage.Upload("file_id2","/Users/Tonyx/piatto.png")

let get_test_image_as_stream() = 
    let toRet = liteDb.FileStorage.OpenRead("file_id2")
    toRet
// ===========

let getImage name =
    liteDb.FileStorage.OpenRead(name)

let removeImage name =
    liteDb.FileStorage.Delete(name)

let roundDecimalGivenUnityOfMeasure (unityOfMeasure:UnityOfMeasures) (theValue:decimal) =
    match unityOfMeasure with 
        | (Pezzo|Confezione) -> Decimal.Floor(theValue)
        | _ -> theValue


let textOfOrderState (orderState:OrderState) =
    match orderState with
        | Editing -> "in modifica"
        | Confirmed -> "confermato"
        | Rejected -> "respinto"
        | Accepted -> "approvato"
        | Ongoing -> "in lavorazione"
        | Done -> "finito"
        | Archived -> "archiviato"



let unityOfMeasureFromString unityOfMeasure =
    match unityOfMeasure with
    | "Pezzo" -> Pezzo
    | "Kilo" -> Kilo
    | "Etto" -> Etto
    | "Confezione" -> Confezione
    | _ -> failwith ("unita' di misura " + unityOfMeasure+" non ammessa")

let passHash (pass: string) =
    use sha = Security.Cryptography.SHA256.Create()
    Text.Encoding.UTF8.GetBytes(pass)
    |> sha.ComputeHash
    |> Array.map (fun b -> b.ToString("x2"))
    |> String.concat ""


[<CLIMutable>]
type User = {
    Id: int
    UserType : UserType
    UserEmail: string option
    UserPhone: string option
    UserName : string
    Address: string option
    HashPassword : string
}

let users = liteDb.GetCollection<User>("users")

[<CLIMutable>]
type Order = {
    Id: int
    User: User
    OrderDate: DateTime
    OrderState: OrderState
    AdminComment: string option
}


let orders = liteDb.GetCollection<Order>("orders")

[<CLIMutable>]
type OrderStateMapping = {
    Id: int
    Order: Order
    OrderState: OrderState
    CreationTime: DateTime
}
let orderStateMappings = liteDb.GetCollection<OrderStateMapping>("orderStateMapping")


[<CLIMutable>]
type Courses = {
    Id: int
    Name: string
    Price: decimal
    Available: bool
    Description: string option
    UnityOfMeasure : UnityOfMeasures
    ImageName: String option
    EncodedImage: string option
}

let courses = liteDb.GetCollection<Courses>("courses")

let _ = courses.EnsureIndex(fun x -> x.Name)

[<CLIMutable>]
type OrderItem = {
    Id: int
    OrderId: int
    // ItemQuantity: int
    ItemQuantity: decimal
    Course: Courses
    CourseId: int
    Comment: string option
    CreationTime: DateTime
}
let orderItems = liteDb.GetCollection<OrderItem>("orderItems")


[<CLIMutable>]
type OrderItemStateMapping = {
    Id: int
    OrderItem: OrderItem
    OrderItemState: OrderState
    CreationTime: DateTime
}
let orderItemStateMapping = liteDb.GetCollection<OrderItemStateMapping>("orderItemStateMapping")


[<CLIMutable>]
type WelcomeMessage = {
    Id: int
    Message: string
    IsCurrentMessage: bool
}
let welcomeMessages = liteDb.GetCollection<WelcomeMessage>("welcomeMessages")



[<CLIMutable>]
type AutoSubscribedUsers = {
    Id: int
    GuidCode: string
    CreationTime: DateTime
    // Taken: bool
    // ConnectedUser: User option
}

let autoSubscribedUsers = liteDb.GetCollection<AutoSubscribedUsers>("autoSubscribedUsers")

[<CLIMutable>]
type PeriodicalExcursion = {
    Id: int
    Name: string
    DateInit: System.DateTime
    DateEnding: System.DateTime
    Time: DateTime
    DaysOfWeek: DayOfWeek list
    Seats: int
}

let periodicalExcursions = liteDb.GetCollection<PeriodicalExcursion>("periodicalExcursion")

type OneShotExcursion = {
    Id: int
    DateTime: DateTime
    Seats: int
}

let oneSchotExcursions = liteDb.GetCollection<OneShotExcursion>("oneShotExcursion")

type Discount = Percentage of decimal | Amount of decimal

[<CLIMutable>]
type Slot = {
    Id: int
    Excursion: PeriodicalExcursion
    DateTime: DateTime
    DayOfWeek: String
}

let slots = liteDb.GetCollection<Slot>("slots")



[<CLIMutable>]
type Cupon = {
    Id: int
    Slots: Slot list
    NumberOfPeople: int
    Discount: Discount
    OwnerEmail: string option
    Used: bool
    CuponGuid: Guid
}

let cupons = liteDb.GetCollection<Cupon>("offers")


let findCuponByGuid gui =
    let filterSearch = cupons.fullSearch <@ fun x -> x.CuponGuid @> (fun guid -> guid.ToString() = gui )
    filterSearch |> Seq.tryHead

    // cupon.findMany<@ fun x -> x.CuponGuid.ToString() = gui@> |> Seq.tryHead


let createSlot (excursion: PeriodicalExcursion) (dateTime: DateTime) =
    log.Debug("creating slot for time: " + dateTime.ToString())
    let newSlot = {
        Id=0
        Excursion=excursion
        DateTime=dateTime
        DayOfWeek = dateTime.DayOfWeek.ToString()
    }
    let v = slots.Insert(newSlot)
    log.Debug("slot insert "+v.ToString())
    ()

let findSlotOfAnExcursionByExcursionId id =
    log.Debug("finding slot")
    slots.findMany <@ fun x -> x.Excursion.Id = id@>

let findSlotsByExcursion excursion =
    log.Debug("finding slots by excursion")
    slots.findMany <@ fun x -> x.Excursion = excursion@>


let getExistingSlotsOfAPeriod (starting:DateTime) (ending:DateTime) =
    // let bsonStarting = starting |> BsonValue
    // let bsonEnding = ending |> BsonValue
    // let query = Query.Between("DateTime",bsonStarting,bsonEnding)

    let filtered = slots.fullSearch <@ fun slot -> slot.DateTime @> (fun dateTime -> (System.DateTime.Compare(dateTime,starting)>=0) && (System.DateTime.Compare(dateTime,ending)<=0) ) |> Seq.sortBy (fun x -> x.DateTime)

    filtered 


let getSlotsGivenIds (slotsIds: int list) =
    log.Debug(sprintf "getting slots for number of slots: %d " slotsIds.Length)
    let setOfSlotsIds = slotsIds |> Set.ofList
    let retSlots = slots.fullSearch <@ fun slot -> slot.Id@>  (fun id -> Set.contains id setOfSlotsIds)
    retSlots



let createCupon numberOfpeople typeOfDiscount mySlots guid=
    log.Debug("creating a new cupon")
    let newCupon = {
        Id=0
        Slots = mySlots
        NumberOfPeople = numberOfpeople
        Discount = typeOfDiscount
        OwnerEmail = None
        Used = false
        CuponGuid = guid
    }
    cupons.Insert(newCupon)


let getCupon id =
    log.Debug("getting cupon")
    cupons.findOne <@ fun x -> x.Id = id@>

let getAllValidCupons() =
    log.Debug("get all cupons")
    cupons.findMany <@ fun x -> x.Used = false @>
    // cupon.FindAll()

let storeCupon (newCupon:Cupon) =
    log.Debug("storeCupon")
    cupons.Upsert(newCupon)

let removeCupon(id:int) =
    log.Debug("removing cupon")
    cupons.delete <@ fun x -> x.Id = id @>
    
let updateCupon(cupon: Cupon) =
    log.Debug("update cupon: "+(cupon.ToString()))
    cupons.Update(cupon)


let removeAllCupons() =
    log.Debug("removing all cupons")
    let allCupons = getAllValidCupons()
    allCupons |> Seq.iter (fun x -> (removeCupon x.Id |> ignore))


let makeCuponAsUsed id =
    log.Debug(sprintf "make cupon as used %d" id)
    let cupon = getCupon id
    let newCupon = {cupon with Used = true}
    cupons.Update(newCupon)

let nextSlotStartingFrom (dateTime:DateTime) =

    let dateFrom = Utils.adjustTime(dateTime) |> BsonValue
    let query = Query.GT("DateTime",dateFrom) 
    slots.Find(query) |> Seq.sortBy (fun x -> x.DateTime) |> Seq.tryHead





// deprecated

// let createCupon (excursionAndSlots: (PeriodicalExcursion*DateTime) list)  (discount: Discount) (nPeople: int)  = 
//     let now = System.DateTime.Now
//     let lastTimeSlot = excursionAndSlots |> List.map (fun (_,y) -> y) |> List.sortDescending |> List.head

//     if (System.DateTime.Compare(now,lastTimeSlot)<0) then
//         let newOffer = {
//             Id = 0
//             ExcursionsAndSlots = excursionAndSlots 
//             NumberOrPeople = nPeople
//             Discount = discount
//             Used = false
//             Expired = false
//             OwnerEmail = None
//         }
//         cupon.Insert(newOffer) |> ignore
//     else 
//         ()


let getAllPeriodicalExcursions() = 
    periodicalExcursions.FindAll() |> Seq.sortBy (fun x -> x.Time) |> Seq.toList

let updatePeriodicalExcursion (periodicalExcursion:PeriodicalExcursion) =
    periodicalExcursions.Update(periodicalExcursion)

let createPeriodicalExcursion name dateInit dateEnding dateTime (daysOfWeek: DayOfWeek list) seats =
    let newExcursion = {
        Id = 0
        Name = name
        DateInit = dateInit
        DateEnding = dateEnding
        Time = dateTime
        DaysOfWeek = daysOfWeek
        Seats = seats
    }
    let bSonValue = periodicalExcursions.Insert(newExcursion)
    periodicalExcursions.FindById(bSonValue).Id

let getPeriodicalExcursion (id:int) =
    let bsonId = BsonValue(id)
    periodicalExcursions.FindById(bsonId)

let addDayToPeriodicalExcursion id (X:DayOfWeek) =
    let periodicalExcursion = getPeriodicalExcursion id
    let newPeriodicalExcursion = {periodicalExcursion with DaysOfWeek = X::periodicalExcursion.DaysOfWeek |> Set.ofList |> Set.toList }
    updatePeriodicalExcursion newPeriodicalExcursion

let getAllCupons() =
    cupons.FindAll()

let getSlotsOfAPeriodicalExcursion id =
    log.Debug("getting slots")
    slots.findMany <@ fun x -> x.Excursion.Id = id @>

let getSlot id  =
    slots.findOne <@ fun x -> x.Id = id @>

let getCuponsGivenASlot slotId =
    let slot = getSlot slotId
    cupons.FindAll() |> Seq.filter (fun x -> x.Slots |> List.contains slot  )

let removeASlotFromACupon (theCupon:Cupon) slot =
    let newCupon = {theCupon with Slots = List.filter (fun x -> not (x = slot)) theCupon.Slots}
    let _ = match theCupon.Slots with 
        | [] -> cupons.delete <@ fun x -> x.Id = theCupon.Id @>; ()
        | _ ->  cupons.Upsert(newCupon); ()
    ()



let deleteSlot id =
    log.Debug("deleting slot")
    let allCupons  = getAllCupons()
    let slot = getSlot id
    let _ = allCupons |> Seq.iter (fun x -> removeASlotFromACupon x slot)
    slots.delete <@ fun x -> x.Id = id@> |> ignore
    ()

// DELETE ALL:
// let _ = 
//     let allSlots = slots.FindAll()
//     allSlots |> Seq.iter (fun x -> deleteSlot x.Id ) |> ignore
//     ()


let deletePeriodicalExcursion id =
    let excursion = getPeriodicalExcursion id
    let slotsOfTheExcursion = findSlotsByExcursion excursion
    let _ = slotsOfTheExcursion |> Seq.iter (fun (x:Slot) -> deleteSlot x.Id)
    periodicalExcursions.delete <@ fun x -> x.Id = id@>

let addWeekDayToPeriodicalExcursion id (day:DayOfWeek) =
    let periodicalExcursion = getPeriodicalExcursion id
    let newDayAdded = (day::periodicalExcursion.DaysOfWeek )|> Set.ofList |> Set.toList
    let periodicalExcursionUpdated = { periodicalExcursion with DaysOfWeek = newDayAdded}
    periodicalExcursions.Update(periodicalExcursionUpdated)


let getAllWelcomeMessages() =
    welcomeMessages.FindAll() |> Seq.toList

// let uploadCourseImage imageName imagePath courseId =
//     let image = liteDb.FileStorage.Upload(imageName,imagePath)
//     let course = getCourse courseId
//     course.


let addWelcomMessage welcomeMessage =
    let message = {
        Id = 0
        Message = welcomeMessage
        IsCurrentMessage = false
    }
    welcomeMessages.Insert(message)

let createAutoSubscribedUser guidCode =
    let newAutosubscribedUser =
        {
            Id = 0
            GuidCode = guidCode
            CreationTime = Utils.adjustTime(System.DateTime.Now)
            // Taken = false
        }
    autoSubscribedUsers.Insert(newAutosubscribedUser)

let findAutoSubscribedUserByGuidCode guidCode =
    let expirationTimeInMinutes = (float)Settings.AutoSubscribeCodeExpirationTimeInMinutes
    let now = System.DateTime.Now
    autoSubscribedUsers.findMany <@ fun x -> x.GuidCode = guidCode @> |>  
        Seq.filter (fun (x:AutoSubscribedUsers) -> x.CreationTime.AddMinutes(expirationTimeInMinutes)>now) |> Seq.tryHead


let validateUser (username:string)  password =
    let hashedPassword = passHash password
    let userLookedUp = users.tryFindOne<@ fun x -> x.UserName = username && x.HashPassword=hashedPassword @>
    userLookedUp

// let createUser (username:string) (hashedPassword: string) =
//     let existingUser = users.Exists(fun x -> x.UserName = username)
//     match existingUser with
//     | false ->
//         let newUser = {
//             Id = 0
//             UserType= Ordinary
//             UserName = username
//             UserEmail = Some "email@mail.com"
//             UserPhone = Some "123 456"
//             HashPassword =  hashedPassword
//             Address = None
//         }
//         users.Insert(newUser) |> ignore
//         ()
//     | _ -> ()


let updateUser (user:User) =
    users.Update(user)

let updateCourse (course:Courses) =
    courses.Update(course)
    

let createUser (username:string) (hashedPassword: string) (userPhone: string option) (address: string option) (email: string option)=
    let existingUser = users.Exists(fun x -> x.UserName = username)
    match existingUser with
    | false ->
        let newUser = {
            Id = 0
            UserType= Ordinary
            UserName = username
            UserEmail = email
            UserPhone = userPhone
            HashPassword =  hashedPassword
            Address = None
        }
        users.Insert(newUser) |> ignore
        ()
    | _ -> ()

let removeAutoSubsriberCode code =
    autoSubscribedUsers.delete <@ fun x -> x.GuidCode = code @>



let createUserByGuidCode (username:string) (hashedPassword: string) (userPhone: string option) (address: string option) (email: string option) (guidCode: string)=
    let existingUser = users.Exists(fun x -> x.UserName = username)
    match existingUser with
    | false ->
        let newUser = {
            Id = 0
            UserType= Ordinary
            UserName = username
            UserEmail = email
            UserPhone = userPhone
            HashPassword =  hashedPassword
            Address = None
        }
        users.Insert(newUser) |> ignore
        removeAutoSubsriberCode guidCode  |> ignore
        Some newUser
    | _ -> None



let deleteWelcomeMessage id =
    welcomeMessages.delete <@ fun x -> x.Id = id @>

let getWelcomeMessage id =
    welcomeMessages.findOne <@ fun x -> x.Id = id @> 

let getCurrentMessages() =
    welcomeMessages.findMany <@ fun x -> x.IsCurrentMessage@> |> Seq.toList

let makeMessageAsDefault id = 
    let message = getWelcomeMessage id
    let newMessage = {message with IsCurrentMessage = true}
    let oldCurrentMessages = getCurrentMessages()   
    let replacementCurrentMessages = oldCurrentMessages |> List.map (fun x -> {x with IsCurrentMessage = false})
    let replacement = newMessage::replacementCurrentMessages

    welcomeMessages.Update(replacement)


let makeMessageAsNotDefault id =
    let message = getWelcomeMessage id
    let messageChanged = {message with IsCurrentMessage = false}
    welcomeMessages.Update(messageChanged)

let removeUser userId =
    users.delete <@ fun x -> x.Id = userId@>

let modifyCourse (courseId:int) (name:string) (description:string option) (availability:bool) (unityOfMeasure:string)=

    let compliantUnityOfMeasure = unityOfMeasureFromString unityOfMeasure
    let course = courses.findOne <@ fun x -> x.Id = courseId @>
    let updatedCourse = { course with Name = name; Available=availability; Description=description;UnityOfMeasure = compliantUnityOfMeasure} 
    let _ = courses.Upsert([updatedCourse])
    ()

let alreadyACourseWithThatNameAndDifferentId (id:int) (name: string) =
    let courses = courses.findMany <@ fun x -> (x.Name = name) @>
    let coursesExceptMe = courses |> Seq.filter (fun x -> x.Id <> id)
    (coursesExceptMe |> Seq.length) >= 1

let alreadyExistsACourseWithName (name:string) =
    let course = courses.tryFindOne <@ fun x -> x.Name = name @>
    course

let changePassword userId hashedOldPassword hashedNewPassword =
    let user = users.findOne<@fun x -> x.Id = userId@>
    let _ = if (hashedOldPassword = user.HashPassword ) then
        let updatedUser = { user with HashPassword = hashedNewPassword}
        users.Upsert([updatedUser] ) 
        ()
    ()
        
let forceSetPassword (user:User) hashedNewPassword =
    let updatedUser = { user with HashPassword = hashedNewPassword}
    let _ = users.Upsert([updatedUser] ) 
    ()

let getAllCourses() =
    courses.FindAll() |> Seq.sortBy (fun x -> x.Name)


let getAllAvailableCourses() =
    courses.findMany <@ fun x -> x.Available@> |> Seq.sortBy(fun x -> x.Name) |>  Seq.toList

let getCountOfAllAvailableCourses() =
    courses.findMany <@ fun x -> x.Available @> |> Seq.length




let getAllAvailableCoursesByPageAndNumberOfItemPerPage pageNumber itemPerPage =
    let courseCount = getCountOfAllAvailableCourses()
    let itemToStart = pageNumber*itemPerPage
    let numberOfNextToTake = (Math.Min((pageNumber+1)*Utils.ITEMS_PER_PAGE,courseCount)-pageNumber*Utils.ITEMS_PER_PAGE)
    courses.findMany <@ fun x -> x.Available@> |> Seq.sortBy(fun x -> x.Name) |> 
        Seq.skip(itemToStart) |> Seq.take(numberOfNextToTake) |>  Seq.toList


let getAllAvailableCoursesByPage pageNumber =
    let courseCount = getCountOfAllAvailableCourses()
    let itemToStart = pageNumber*Utils.ITEMS_PER_PAGE
    let numberOfNextToTake = (Math.Min((pageNumber+1)*Utils.ITEMS_PER_PAGE,courseCount)-pageNumber*Utils.ITEMS_PER_PAGE)
    courses.findMany <@ fun x -> x.Available@> |> Seq.sortBy(fun x -> x.Name) |> 
        Seq.skip(itemToStart) |> Seq.take(numberOfNextToTake) |>  Seq.toList



let getCountOfAllAvailableCourseesWithNameSearch name =
    courses.findMany <@ fun x -> x.Available && x.Name.Contains name  @> |> Seq.length

let geAllAvailableCorseByPageWithNameSearch pageNumber name =
    let courseCount = getCountOfAllAvailableCourseesWithNameSearch name
    let itemToStart = pageNumber * Utils.ITEMS_PER_PAGE
    let numberOfNextToTake = (Math.Min((pageNumber+1)*Utils.ITEMS_PER_PAGE,courseCount)-pageNumber*Utils.ITEMS_PER_PAGE)
    courses.findMany <@ fun x -> x.Available && x.Name.Contains name @> |> Seq.sortBy(fun x -> x.Name) |> 
        Seq.skip(itemToStart) |> Seq.take(numberOfNextToTake) |>  Seq.toList




let getAllOrdinaryUsers() =
    users.findMany <@ fun x -> x.UserType = Ordinary @>  |> Seq.toList

let getCourse (courseId: int) =
    let course = courses.findOne <@ fun x -> x.Id = courseId@>
    course

let getAllUsers() =
    users.FindAll()

let getOpenAndOngoingOrders() =
    orders.findMany <@ fun x -> x.OrderState = Editing || x.OrderState = Ongoing  @> |> Seq.toList

let getOnlyOpenOrders() =
    orders.findMany <@ fun x -> (x.OrderState = Editing) || (x.OrderState = Accepted) || (x.OrderState = Ongoing ) @> |> Seq.toList

let cloneOrder (orderId: int)  =
    let orderToBeCloned = orders.findOne<@ fun x -> x.Id = orderId @>
    let clonedOrder = { orderToBeCloned with Id = 0; OrderDate = Utils.adjustTime(System.DateTime.Now); OrderState = Editing}
    let orderItemsToBeCloned = orderItems.findMany <@ fun x -> x.OrderId = orderId @> |> Seq.toList
    let insertedCloned = orders.Insert(clonedOrder)

    let clonedOrderItems = orderItemsToBeCloned |>  List.fold (fun acc x -> acc @ [{x with Id = 0; OrderId = (insertedCloned |> int)  }]  ) []
    let _ = clonedOrderItems |> List.iter (fun x -> orderItems.Insert(x)|> ignore)
    insertedCloned

let getUser (userId: int)     =
    users.findOne <@ fun x -> x.Id = userId@>

let createOrder (user: User) (userName: string) =
    let adjustedNow = Utils.adjustTime(System.DateTime.Now)
    let newOrder = {
        Id =0
        OrderDate = adjustedNow
        OrderState = Editing
        AdminComment = None
        User = user
        
    }
    let insertedOrder = orders.Insert(newOrder)
    insertedOrder


let getOrderById (orderId: int) =
    orders.findOne<@ fun x -> x.Id = orderId@>

let getOrderByBsonId (bsonValueId: BsonValue) =
    orders.FindById(bsonValueId)

let getOrderItemsOfOrder (orderId: int) =
    orderItems.findMany<@ fun x -> x.OrderId = orderId@>


// let createOrderItem (orderId: int) (name:string) (quantity:int) =
//     let newOrderItem = {
//         Id =0
//         OrderId = orderId
//         ItemName = name
//         ItemQuantity = quantity
//         CourseId = 1
//         Comment = ""
//     }
//     orderItems.Insert(newOrderItem)




    


let createOrderItemById (orderId: int) (courseId:int ) (quantity:decimal) (comment: string option) =
    let course =
        getCourse courseId
    let roundedQuantity = roundDecimalGivenUnityOfMeasure course.UnityOfMeasure quantity

    let newOrderItem = {
        Id =0
        OrderId = orderId
        ItemQuantity = roundedQuantity
        CourseId = courseId
        Course = course
        Comment = comment
        CreationTime = Utils.adjustTime(System.DateTime.Now)
    }
    orderItems.Insert(newOrderItem)

let getAdministrator() =
    try
        users.findOne <@ fun x -> x.UserType = Admin @>
    with | err -> failwith ("error "+err.ToString())

let createCourse (name:string) (description: string option) (available:bool) (price:decimal) (unityOfMeasure:string)=

    let compliantUnityOfMeasure = unityOfMeasureFromString unityOfMeasure

    let newCourse = {
        Id = 0
        Name=name
        Available=available
        Description = description
        Price = price
        UnityOfMeasure = compliantUnityOfMeasure
        ImageName = None
        EncodedImage = None
    }
    courses.Insert(newCourse);



let getOrderItem (orderItemId: int) =   
    // let orderItem = orderItems.findOne <@ fun x -> x.Id = orderItemId  @>
    let bsonValue = BsonValue(orderItemId)
    let orderItem = orderItems.FindById(bsonValue)
    orderItem

let getUserByName (userName: string) =
    users.tryFindOne <@ fun x -> x.UserName = userName@>

let getUserAndOrderIdOfOrderItem (orderItemId: int) =
    let orderItem = getOrderItem orderItemId
    let order = getOrderById orderItem.OrderId
    (order.User.Id, orderItem.OrderId)

let removeOrderItem (orderItemId: int) =
    orderItems.delete <@ fun x -> x.Id = orderItemId @>


let removeAllOrders() =
    orders.delete <@fun x -> x.Id <999999 @>

let removeCourse (courseId: int) =
    let course = getCourse courseId
    let _ = match course.ImageName with
        | Some X -> removeImage X |> ignore
        | _ -> ()
    courses.delete <@ fun x -> x.Id = courseId @>

let updatOrderItemByOrderitemRecord (orderItem: OrderItem) =
    orderItems.Update(orderItem)

let updateOrderItem (orderItem:OrderItem)  (courseId: int) (quantity: decimal) (comment: string option) =
    let course = getCourse courseId
    let roundedQuantity = roundDecimalGivenUnityOfMeasure course.UnityOfMeasure quantity

    let overWrittenOrderItem = {
        orderItem with
            Id = orderItem.Id
            OrderId = orderItem.OrderId
            ItemQuantity = roundedQuantity
            CourseId = courseId
            Course = course;
            Comment = comment
            CreationTime = Utils.adjustTime(System.DateTime.Now)
    }

    // orderItems.Update(overWrittenOrderItem) // see later

    let _ = orderItems.delete <@ fun x -> x.Id = orderItem.Id@>
    orderItems.Insert(overWrittenOrderItem)

let getAllOrdersOfAUser userId =
    orders.findMany<@ fun x -> x.User.Id = userId @> |> Seq.sortBy(fun x -> x.OrderDate) |> Seq.toList

let getAllUnarchivedOrdersOfAUser (user:User)  = 
    orders.findMany<@ fun x -> x.User = user @> |> Seq.filter (fun (x:Order) -> x.OrderState <> Archived) |>   Seq.sortBy(fun x -> x.OrderDate) |> Seq.toList


let getAllOrders() =
    // orders.FindAll() |> Seq.sortBy(fun x -> x.OrderDate) |>  Seq.toList
    orders.FindAll() |> Seq.toList

let getAllOpenOrders() =
    // orders.findMany<@ fun x -> not (x.OrderState = Archived) @> |> Seq.sortBy(fun x -> x.OrderDate) |> Seq.toList
    orders.findMany<@ fun x -> not (x.OrderState = Archived) && not (x.OrderState = Editing) @> |> Seq.sortBy(fun x -> x.OrderDate) |> Seq.toList

let getAllConfirmedOrders() =
    orders.findMany<@ fun x ->  (x.OrderState = Confirmed) @> |> Seq.sortBy(fun x -> x.OrderDate) |> Seq.toList
 
let makeOrderAsOngoing orderId =
    let order = orders.findOne <@ fun x -> x.Id = orderId@>
    let replacementOrder = {order with OrderState = Ongoing } 
    let _ = orders.Delete(BsonValue(orderId))
    let _ = orders.Insert(replacementOrder)
    ()

let makeOrderAsRejected orderId =
    let order = orders.findOne <@ fun x -> x.Id = orderId@>
    let replacementOrder = {order with OrderState = Rejected } 
    let _ = orders.Update(replacementOrder)
    ()



let makeOrderAsDone orderId =
    let order = orders.findOne <@ fun x -> x.Id = orderId@>
    let replacementOrder = {order with OrderState = Done } 
    let _ = orders.Delete(BsonValue(orderId))
    let _ = orders.Insert(replacementOrder)
    ()

let makeOrderAsConfirmed orderId =
    let order = orders.findOne <@ fun x -> x.Id = orderId @>
    let replacementOrder = {order with OrderState = Confirmed}
    let _ = orders.Update(replacementOrder)
    ()


let makeOrderAsArchived orderId =
    let order = orders.findOne <@ fun x -> x.Id = orderId@>
    let replacementOrder = {order with OrderState = Archived } 
    let _ = orders.Delete(BsonValue(orderId))
    let _ = orders.Insert(replacementOrder)
    ()


let removeOrder orderId =
    let _ = orders.delete <@ fun x -> x.Id = orderId @>
    let _ = orderItems.delete <@ fun x -> x.OrderId = orderId @>
    ()

let removeAllOrderItems() = 
    let allOrderItems = orderItems.FindAll()
    let _ = allOrderItems |> Seq.iter (fun x -> 
        orderItems.Delete(BsonValue(x.Id))|> ignore 
        )
    ()

let updateOrderStateAndAdminComment (orderId:int) (state:OrderState) (adminComment:string option) =
    let order = getOrderById orderId
    let theAdminComment  = match adminComment with Some X -> X | None -> ""
    let orderReplacement = {order with AdminComment = adminComment; OrderState = state }
    orders.Update(orderReplacement)

let uploadCourseImage imageName imagePath courseId =
    liteDb.FileStorage.Upload(imageName,imagePath)

    // let course = getCourse courseId
    // let newCourse = {course with ImageName = Some imageName}
    // courses.Update(newCourse)



let getUnclaimedCuponsWithSomSlotsStartingFrom (dateTime:DateTime)=
    let adjustedDateTime = Utils.adjustTime(dateTime)

    let someSlotsAreStillOpen   (slots: Slot list) = 
        let openSlots = slots |> List.filter (fun x -> System.DateTime.Compare(x.DateTime,adjustedDateTime)>=0) |> List.length 
        openSlots > 0

    let availableCupon = cupons.FindAll() |> Seq.filter (fun (x:Cupon) -> x.OwnerEmail.IsNone ) |>  Seq.filter (fun (x:Cupon) -> (someSlotsAreStillOpen x.Slots  )) 

    availableCupon




