open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.DependencyInjection
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Logging
open FParsec
open Shared
open Microsoft.Azure.WebJobs
open System.Threading.Tasks
open Microsoft.AspNetCore.Mvc
open Newtonsoft.Json;
open Domain
open System.IO




[<FunctionName("Convert")>]
let convertFunction ([<HttpTrigger(Extensions.Http.AuthorizationLevel.Anonymous, "POST", Route = "convert")>]req: HttpRequest, log: ILogger) =
    async {

        use streamReader = new StreamReader(req.Body)

        let! requestContent = streamReader.ReadToEndAsync() |> Async.AwaitTask
        let inputClass = JsonConvert.DeserializeObject<string>(requestContent)
        let parsingResult = inputClass |> run classExpr

        match parsingResult with
        | Success (result,_,_) -> 
            let result = TreeProcessing.processTree [result] |> List.head
            let json = JsonConvert.SerializeObject(result)
            return OkObjectResult(json) :> IActionResult
        | Failure (errorMsg,_,_) ->
            return BadRequestObjectResult(printf "Failure: %s" errorMsg) :> IActionResult
        
    } |> Async.StartAsTask
    
