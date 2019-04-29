module DigitalCupons.Utils
open System
open System.Net.Mail

open System.IO;
open System.Text;
open System.Runtime.Serialization.Json;
open System.Runtime.Serialization;

open FSharp.Configuration

open PayPalCheckoutSdk.Core
open PayPalCheckoutSdk.Orders

open BraintreeHttp
open QRCoder



let timeOffset = System.DateTime.Now.Subtract(System.DateTime.UtcNow)

let log = log4net.LogManager.GetLogger(System.Reflection.MethodBase.GetCurrentMethod().DeclaringType);

let adjustTime (dateTime: DateTime) =
    dateTime.Add(timeOffset)


let backAdjustTime(dateTime: DateTime) =
    dateTime.Subtract(timeOffset)

[<Literal>]
let YES = "Sì"

[<Literal>]
let NO = "No"

let YES_NO_SELECT_INPUT =
    [(YES,YES);(NO,NO)]

[<Literal>]
let APPROVED = "Approvato"

[<Literal>]
let REJECTED = "Respinto" 

let APPROVED_REJECTED_SELECT_INPUT =
    [(APPROVED,APPROVED);(REJECTED,REJECTED)]

[<Literal>]
let ITEMS_PER_PAGE = 3



[<Literal>]
let PERCENT = "Percent"

[<Literal>]
let VAlUE = "Value"


let stringToBase64Qr (inString:string) =
    let qrGenerator = new QRCoder.QRCodeGenerator()
    let qrCodeData = qrGenerator.CreateQrCode(inString,QRCodeGenerator.ECCLevel.Q)
    let qrCode = new QRCode(qrCodeData)
    let qrCodeImage = qrCode.GetGraphic(20)
    let stream = new System.IO.MemoryStream()
    qrCodeImage.Save(stream,qrCodeImage.RawFormat)
    let arrayOfQrCode = stream.ToArray()
    let encoded = System.Convert.ToBase64String arrayOfQrCode
    encoded


let stringToImageQr (inString: string) =
    let qrGenerator = new QRCoder.QRCodeGenerator()
    let qrCodeData = qrGenerator.CreateQrCode(inString,QRCodeGenerator.ECCLevel.Q)
    let qrCode = new QRCode(qrCodeData)
    let qrCodeImage = qrCode.GetGraphic(20)
    let stream = new System.IO.MemoryStream()
    qrCodeImage.Save(stream,qrCodeImage.RawFormat)
    stream
    // let arrayOfQrCode = stream.ToArray()
    // let encoded = System.Convert.ToBase64String arrayOfQrCode
    // encoded





type PayPalClient =
    static member environment(): PayPalEnvironment =
        SandboxEnvironment("AaZ6b1_oPlG4Ec52NcewQRKc3lJb8b4FZy0ACE5mF-U4e-_HcCxNhEIBHeVG5OnCREiyBkDn_anGnA5B","ENn_3hEq_ek6FsYMKnn5WjHGdbuvMm2hAdzC-tlC3KPIUfMeFRI7eadkzoORtL5_CbYD9IDSKBeItFao") :> PayPalEnvironment

    static member client() =
        PayPalHttpClient(PayPalClient.environment())

    static member client(refreshToken: string) =
        PayPalHttpClient(PayPalClient.environment(), refreshToken);


    // static member  ObjectToJSONString(serializableObject) =
        
    //         let  memoryStream = new MemoryStream();
    //         // let writer = JsonReaderWriterFactory.CreateJsonWriter()
    //         // let writer = JsonReaderWriterFactory.CreateJsonWriter(memoryStream, Encoding.UTF8, true, true," ")
    //         let writer = JsonReaderWriterFactory.CreateJsonWriter(memoryStream,Encoding.UTF8,true)

    //         let ser = DataContractJsonSerializer() ( serializableObject.GetType(),)
    //         (serializableObject.GetType(), new DataContractJsonSerializerSettings{UseSimpleDictionaryFormat = true});

    //         ser.WriteObject(writer, serializableObject);
    //         memoryStream.Position = 0;
    //         StreamReader sr = new StreamReader(memoryStream);
    //         return sr.ReadToEnd();


// let getEncodedQrCodeOfString stringToCode =

