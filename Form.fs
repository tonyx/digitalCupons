module DigitalCupons.Form

open Suave.Form

type Album = {
    ArtistId : decimal
    GenreId : decimal
    Title : string
    Price : decimal
    ArtUrl : string
}

let album : Form<Album> = 
    Form ([ TextProp ((fun f -> <@ f.Title @>), [ maxLength 100 ])
            TextProp ((fun f -> <@ f.ArtUrl @>), [ maxLength 100 ])
            DecimalProp ((fun f -> <@ f.Price @>), [ min 0.01M; max 100.0M; step 0.01M ])
            ],
          [])

type Logon = {
    Username : string
    Password : Password
}

let logon : Form<Logon> = Form ([],[])


type GalardoLogon = {
    Username : string
    Password : Password
}


let galardoLogon : Form<Logon> = Form ([],[])

type OrderItem = {
    CourseId: decimal 
    Quantity: decimal
}

let orderItem: Form<OrderItem> = Form([],[])

type OrderApprovalWithEmail = {
    AdminComment: string option
    Approval: string
    SendConfirmationEmail: string
}

let orderApprovalWithEmail: Form<OrderApprovalWithEmail> = Form([],[])

type OrderApproval = {
    AdminComment:  string option
    Approval: string
}

let orderApproval: Form<OrderApproval> = Form([],[])


type NameSearch = {
    Name: string option
}

let nameSearch: Form<NameSearch> = Form(
    [],[]
)


type OrderItemRef = {
    CourseId: decimal
    Quantity: decimal
    Comment: string option
}

let orderItemRef: Form<OrderItemRef> = Form(
    [
        DecimalProp((fun f -> <@ f.Quantity @>),[ min 0.01M; step 0.01M])
    ],
    [])

type Register = {
    Username : string
    Email : string
    Password : Password
    ConfirmPassword : Password
}

let pattern = passwordRegex @"(\w){6,20}"



type Course = {
    Name: string
    Description: string option
    Available: string
    Price: decimal
    UnityOfMeasure: string
}

let course: Form<Course> = Form (
    [   
        TextProp ((fun f -> <@ f.Name @>), [maxLength 30])
        TextProp ((fun f -> <@ f.Available@>), [])
        DecimalProp ((fun f -> <@ f.Price@>), [ min 0.01M; step 0.01M ])
    ],
    []
    
)

type WelcomeMessage = {
    Message: string
}

let welcomeMessage: Form<WelcomeMessage> = Form (
    [ 
        TextProp ((fun f -> <@ f.Message @>), [] )
    ],
    []
    )

type RegisterUser = {
    Username : string
    Password : Password
    ConfirmPassword : Password
    Email: string option
    PhoneNumber: string option
    Address: string option
}

// let emailPattern = true

let emailIsValid =
    (fun f -> f.Email.IsSome && (f.Email.Value.Contains("@"))|| f.Email.IsNone),"email is invalid"

let passwordsMatch = 
    (fun f -> f.Password = f.ConfirmPassword), "Passwords must match"

let registerUser : Form<RegisterUser> = Form (
    [
            TextProp ((fun f -> <@ f.Username @>), [ maxLength 30 ] )
            PasswordProp ((fun f -> <@ f.Password @>), [ pattern ] )
            PasswordProp ((fun f -> <@ f.ConfirmPassword @>), [ pattern ] )

    ],
    [ passwordsMatch;emailIsValid ]
    )


type SubscribeForCupon = {
    Email: string
}

let validateEmail =
    (fun f -> f.Email.Contains("@")), "email is invalid"

let subscribeForCupon: Form<SubscribeForCupon> = Form (
    [],[validateEmail]
)


type PeriodicalExcursion = {
    Name: string
    DateInit: string
    DateEnding: string
    Time: string
    Seats: decimal
    Monday: string option
    Tuesday: string option
    Wednesday: string option
    Thursday: string option
    Friday: string option
    Saturday: string option
    Sunday: string option
}




let periodicalExcursion: Form<PeriodicalExcursion> = Form (
    [ ], []
)


type CuponPickerInterval = {
    TypeOfDiscount: string
    DiscountValue: decimal
    NumberOfCupon: decimal
    NumberOfPeople: decimal
    DateInit: string
    DateEnding: string
}

let cuponPickerInterval: Form<CuponPickerInterval> = Form([],[])


type ModifyUser = {
    Email: string option
    PhoneNumber: string option
    Address: string option
}

let modifyUser: Form<ModifyUser> = Form ([],[])


// let register : Form<Register> = 
//     Form ([ TextProp ((fun f -> <@ f.Username @>), [ maxLength 30 ] )
//             PasswordProp ((fun f -> <@ f.Password @>), [ pattern ] )
//             PasswordProp ((fun f -> <@ f.ConfirmPassword @>), [ pattern ] )
//             ],[ passwordsMatch ])

type PayPalOrder = {
    orderID: string option
}

let payPalOrder: Form<PayPalOrder> = Form ([],[])

type Checkout = {
    FirstName : string
    LastName : string
    Address : string
    PromoCode : string option
}
type ChangePassword = {
    OldPassword: Password
    NewPassword: Password
    ConfirmNewPassword: Password
}
let newPasswordsMatch = 
    (fun f -> f.NewPassword = f.ConfirmNewPassword), "Passwords must match"
let changePassword : Form<ChangePassword> = Form ([ 
    PasswordProp ((fun f -> <@ f.NewPassword@> ),[ pattern])   
    PasswordProp ((fun f -> <@ f.ConfirmNewPassword@> ),[ pattern])   
        ],[newPasswordsMatch])
let checkout : Form<Checkout> = Form ([], [])
