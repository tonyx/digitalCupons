module DigitalCupons.Samples

open System
open System.IO;
open System.Text;
open System.Runtime.Serialization.Json;
open System.Runtime.Serialization;


open PayPalCheckoutSdk.Core
open PayPalCheckoutSdk.Orders

open BraintreeHttp

type mah = {oh:int}

// type GetOrderSample() =

   let getPayPalOrder(orderId)  =
     let request:OrdersGetRequest = new PayPalCheckoutSdk.Orders.OrdersGetRequest(orderId)
     let client = Utils.PayPalClient.client()
     let response = client.Execute(request)
     let result = response.Result
     let _ = System.Console.WriteLine("retirved orer status")
     let _ = System.Console.WriteLine("Status: {0}", result.StatusCode)
     let _ = Console.WriteLine("Order Id: {0}", result.Headers)
    //  let _ = Console.WriteLine("Intent: {0}", result.Intent)
    //  let _ = Console.WriteLine("Links:");
    // let li = result.Links

  //    result.Links |> List.iter (fun x -> (Console.WriteLine"\t{0}: {1}\tCall Type: {2}", link.Rel, link.Href, link.Method))
  //    Console.WriteLine("Total Amount: {0} {1}", amount.CurrencyCode, amount.Value)
     response

    // }


    // response.
    // let _ =  Async.Start(response)


    //2. Set up your server to receive a call from the client
    // /*
    //   You can use this method to retrieve an order by passing the order ID.
    //  */
    // static member async Task <HttpResponse> GetOrder(string orderId, bool debug = false)
    // {
    //   OrdersGetRequest request = new OrdersGetRequest(orderId);
    //   //3. Call PayPal to get the transaction
    //   var response = await PayPalClient.client().Execute(request);
    //   //4. Save the transaction in your database. Implement logic to save transaction to your database for future reference.
    //   var result = response.Result<Order>();
    //   Console.WriteLine("Retrieved Order Status");
    //   Console.WriteLine("Status: {0}", result.Status);
    //   Console.WriteLine("Order Id: {0}", result.Id);
    //   Console.WriteLine("Intent: {0}", result.Intent);
    //   Console.WriteLine("Links:");
    //   foreach (LinkDescription link in result.Links)
    //   {
    //     Console.WriteLine("\t{0}: {1}\tCall Type: {2}", link.Rel, link.Href, link.Method);
    //   }
    //   AmountWithBreakdown amount = result.PurchaseUnits[0].Amount;
    //   Console.WriteLine("Total Amount: {0} {1}", amount.CurrencyCode, amount.Value);

    //   return response;
    // }




// open BraintreeHttp;

// type PayPalClient =
//     static member environment() =
//         SandboxEnvironment("PAYPAL-SANDBOX-CLIENT-ID","PAYPAL-SANDBOX-CLIENT-SECRET");
