module DigitalCupons.Path


type StringFloat =  PrintfFormat<(string-> float -> string),unit,string,string,(string*float)>
type StringDecimal =  PrintfFormat<(string-> decimal -> string),unit,string,string,(string*decimal)>
type PrintFloat =  PrintfFormat<(float -> string),unit,string,string,float>

type IntPath = PrintfFormat<(int -> string),unit,string,string,int>
type Int2Path = PrintfFormat<(int -> int  -> string),unit,string,string,(int*int)>



type IntPath3 = PrintfFormat< (int  -> int -> int ->  string),unit,string,string,(int*int*int)>
type IntPath4 = PrintfFormat< (int  -> int -> int -> int -> string),unit,string,string,(int*int*int*int)>

type StrPath = PrintfFormat<(string -> string),unit,string,string,string>
type IntStrPath = PrintfFormat<(int -> string -> string),unit,string,string,(int*string)>
type Int2StrPath = PrintfFormat<(int -> int -> string -> string),unit,string,string,(int*int*string)>
type Int3StrPath = PrintfFormat<(int -> int -> int -> string -> string),unit,string,string,(int*int*int*string)>





let withParam (key,value) path = sprintf "%s?%s=%s" path key value

let home = "/"

module Store =

    let backUrl = "backUrl"


module Sandbox =
    let imageView = "/sandbox/imageView"

module Account =
    let logon = "/account/logon"
    let logoff = "/account/logoff"
    let register = "/account/register"
    let createNewAccount = "/account/createNewAccount"
    let usersList = "/account/usersList"
    let removeUser: IntPath = "/account/removeUser/%d"
    let changePassword: IntPath = "/account/changePassword/%d"
    let editUser: IntPath = "/account/editUser/%d"
    let manageQrCodeUsers = "/account/manageQrCodeUsers"



    let autoRegisterUser = "/account/autoRegisterUser"
    let logonUsingCode = "/account/logonUsingCode" 

module Cupon =
    let detectCupon: StrPath = "/cupon/detectCupon/%s"
    let displayQrOfCupon: IntPath = "/cupon/displayQrOfCupon/%d"
    let voidCupon: IntPath = "/cupon/voidCupon/%d"
    let lookForCuponPage = "/cupon/lookForCuponPage"
    let claimCupon: IntPath = "/cupon/claimCupon/%d"
    let removeCupon: IntPath = "/cupon/removeCupon/%d"
    let makeCuponAsUsed: IntPath = "/cupon/makeCuponAsUsed/%d"
    let error = "/cupon/error"


module Admin =

    let adminExcursions = "/admin/adminExcursions"
    let removePeriodicalExcursion: IntPath = "/admin/removeExcursion/%d"
    let createPeriodicalExcursion = "/admin/createPeriodicalExcursion"
    let adminCupons = "/admin/adminCupons"
    let cuponsForPeriodicalExcursion: IntPath = "/admin/cuponsForPeriodicalExcursion/%d"
    let cuponToSlots = "/admin/cuponsToSlots"
    let viewCupons = "/admin/viewCupons"



    let aminCourses: IntPath = "/courses/adminCourses/%d"
    let removeCourse: IntPath = "/courses/deletCourse/%d"
    let imageCourseUploader: IntPath = "/courses/imageCourseUploader/%d"
    let imageCourseRemover: IntPath = "/courses/imageCourseRemover/%d"

    let courseImageUpload: IntPath = "/courses/courseImageUpload/%d"

    let editCourse: IntPath = "/courses/editCourse/%d"
    let addNew = "/courses/addNew"
    let manageWelcomeMessage = "/admin/manageWelcomeMessage"
    let removeWelcomeMessage: IntPath = "/admin/removeWelcomeMessage/%d"
    let makeMessageAsDefault: IntPath = "/admin/makeMessageAsDefault/%d"
    let makeMessageAsNotDefault: IntPath = "/admin/makeMessageAsNotDefault/%d"


module Orders =

    let paypalTransactionComplete = "/paypal-transaction-complete"
    let newOrder = "/orders/newOrder"
    let createNewOrder = "/orders/createNewOrder"

    // let editOrder: IntPath = "/orders/editOrder/%d"
    let editOrderRef: IntPath = "/orders/editOrderRef/%d"

    let confirmOrder: IntPath = "/orders/confirmOrder/%d"
    let makeOrderAsOngoingRef: IntPath = "/orders/makeOrderAsOngoingRef/%d"
    let makeOrderAsDoneRef: IntPath = "/orders/makeOrderAsDoneRef/%d"
    let makeOrderAsArchivedRef: IntPath = "/orders/makeOrderAsArchivedRef/%d"

    let payWithPayPal: IntPath = "/orders/payWithPayPal/%d"


    let addItemFromDetailedMenu: IntPath3 = "/orders/addItemFromDetailedMenu/%d/%d/%d"

    let goToConfirmOrder: IntPath = "/orders/goToConfirmOrder/%d"

    // let completeMenuView: IntPath = "/orders/completeMenuView/%d"

    let completeMenuViewPaged: Int2Path = "/orders/completeMenuView/%d/%d"

    let decreaseOrderItemQuantity: Int2Path = "/orders/decreaseOrderItemQuantity/%d/%d"
    let increaseOrderItemQuantity: Int2Path = "/orders/increaseOrderItemQuantity/%d/%d"


    let removeOrderItemFromCompleteMenu: Int2Path = "/orders/removeOrderItemFromCompleteMenu/%d/%d"

    let addCourseToOrder: Int2Path = "/orders/addCourseToOrder/%d/%d"

    let rejectOrder: IntPath = "/orders/rejectOrder/%d"
    let viewOrder: IntPath = "/orders/viewOrder/%d"

    let manageConfirmedOrder: IntPath = "/orders/manageConfirmedOrder/%d"

    let manageConfirmedOrderWithEmail: IntPath = "/orders/manageConfirmedOrderWithEmail/%d"


    let editOrderItem: IntPath = "/orders/editOrderItem/%d"

    let editOrderItemRef: IntPath = "/orders/editOrderItemRef/%d"

    let editOrderItemFromMenuItem: IntPath = "/orders/editOrderItemFromMenuItem/%d"



    let unauthorized = "/orders/unauthorized"

    let removeOrderItem: IntPath = "/orders/removeOrderItem/%d"
    let removeOrderItemRef: IntPath = "/orders/removeOrderItemRef/%d"

    let myOrders = "/orders/myOrders"
    let aggregatedOpenAndOngoingOrders = "/orders/aggregatedOpenAndOngoingOrders"
    let aggregatedOnlyOpenOrders = "/orders/aggregatedOnlyOpenOrders"
    let removeOrder: IntPath = "/orders/removeOrder/%d"
    let cloneOrder: IntPath = "/orders/cloneOrder/%d"
    let makeOrderAsOnGoing: IntPath = "/orders/makeOrderAsOngoing/%d"
    let makeOrderAsDone: IntPath = "/orders/makeOrderAsDone/%d"
    let makeOrderAsAchived: IntPath = "/orders/makeOrderAsArchived/%d"
    let allOrders = "/orders/allOrders"
    let allOpenOrders = "/orders/allOpenOrders"

    let allConfirmedOrders = "/orders/allConfirmedOrders"

    let newOrderRef = "/orders/newOrderRef"





