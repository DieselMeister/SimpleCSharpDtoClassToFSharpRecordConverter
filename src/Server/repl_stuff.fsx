#r @"C:\Users\Daniel\.nuget\packages\fparsec\1.0.3\lib\netstandard1.6\FParsecCS.dll"
#r @"C:\Users\Daniel\.nuget\packages\fparsec\1.0.3\lib\netstandard1.6\FParsec.dll"

open FParsec

let test p str =
    match run p str with
    | Success (result,_,_) -> printfn "Success: %A" result
    | Failure (errorMsg,_,_) -> printfn "Failure: %s" errorMsg

type NodeState = unit
let str s = pstring s
let str_ws s = spaces >>. str s .>> spaces
let between l r p = str_ws l >>. p .>> str_ws r

let wordWithoutSpaces:Parser<string,NodeState> = many (letter <|> digit) |>> fun x -> System.String(x |> List.toArray)
let wordWithoutSpacesAndAllowedSpecialCharacters (c:char list):Parser<string,NodeState> = many (letter <|> digit <|> (anyOf c)) |>> fun x -> System.String(x |> List.toArray)


type Expr =
    | Class of string * Expr list
    | Property of string * string
    | Scope of Expr list
    | Rest of string
    | Nothing


let publicExpr: Parser<string,NodeState> = str "public"
let privateExpr: Parser<string,NodeState> = str "private"
let classKeywordExpr:Parser<string,NodeState> = str "class"


let scopeParseExpression, scopeParsExprImpl = createParserForwardedToRef ()


let singleStatement = scopeParseExpression // |>> fun x -> [x]

let nothingParser : Parser<Expr,NodeState> = str "" |>> fun _ -> Nothing

let scopeExpr : Parser<Expr,NodeState> = 
    (singleStatement)
    |> between "{" "}"
    |>> fun x -> Scope x
    
    

//let restParser2 : Parser<Expr, NodeState> = 
//    (many1CharsTill (noneOf ['{';'}']) (str " " <|> str ";"))  .>>. opt scopeExpr
//    |>> 
//    (fun (x,y) -> 
//        let xStr = x //(System.String(x |> List.toArray))
//        match y with
//        | None -> Rest (xStr, Nothing)
//        | Some y ->Rest (xStr,y)
//    )

let restParser : Parser<Expr, NodeState> = 
    //(many1CharsTill (noneOf ['{';'}']) (str " ; " <|> str " ;" <|> str "; " <|> str " " <|> str ";" <|> (eof |>> fun _ -> "")))
    spaces >>.
    (many1 (noneOf ['{';'}';' '])) .>>
    spaces
    |>> 
    (fun x -> Rest (System.String(x |> List.toArray)))




let manyRestParser =    
    many1 (
        restParser
    )  
    
scopeParsExprImpl := many (choice [ scopeExpr ; restParser; str " " |>> Rest ]) 

// Properties
let propertyTypeExpr:Parser<string,NodeState> = spaces >>. publicExpr >>. spaces >>. (wordWithoutSpacesAndAllowedSpecialCharacters ['?';'<';'>']) .>> spaces
let propertyNameExpr:Parser<string,NodeState> = spaces >>. wordWithoutSpaces .>> spaces
let propertyExpr:Parser<Expr,NodeState> = propertyTypeExpr .>>. propertyNameExpr .>> scopeExpr |>> (fun (x,y) -> Property (x,y))
let manyPropertyExpr:Parser<Expr list,NodeState> = many1 ( propertyExpr )





test (many1 restParser) "bla ; bla"
test scopeExpr "{ get; set; } { get; set; }"
test scopeParseExpression "{{bla bla }}"
test scopeParseExpression "{ bla bla { bla } { blubb } { irgendwas { } } } { public string bla { get;set; } }"
test scopeParseExpression "{ get;set; } { get;set; }"

test propertyExpr "   public string bla { get;set; }"
test (many propertyExpr) "   public string bla { get;set; }   public string bla { get;set; }"

let classParseExpr, cassParseExprImpl = createParserForwardedToRef ()


let classFullKeywordExpr:Parser<string,NodeState> = spaces >>. opt (privateExpr <|> publicExpr) >>. spaces >>. classKeywordExpr .>> spaces 

let classInheritenceExpr:Parser<string,NodeState> =
    spaces >>.
    str ":" .>>
    skipCharsTillString "{" false 3000
    
    

let classNameExpr:Parser<string,NodeState> =
    spaces >>. 
    classFullKeywordExpr >>. 
    wordWithoutSpaces .>>
    choice [attempt classInheritenceExpr; str ""] .>>    
    spaces
    

let innerClassExpr =
    classParseExpr
    |> between "{" "}"
    //|>> fun l -> Properties l

let classExpr = 
    classNameExpr .>>.
    innerClassExpr
    |>> (fun (x,y) -> Class (x,y))

let classesExpr = 
    many1 (
        classExpr
    )




cassParseExprImpl := many1 (choice [attempt classExpr;attempt propertyExpr;attempt scopeExpr;restParser] )



test classFullKeywordExpr "class"
test classNameExpr "class Börchen"
test innerClassExpr "{ public string meh { get;set; } \r\n public string meh2 {get;set;} \r\n bla bla }"
test classExpr "class Börchen { public string meh { get;set; } \r\n public string meh2 {get;set;} \r\n bla bla }"

let test01 =
    run classExpr """
    public class BookDetailDTO
    {
        
        public int Id { get; set; }
        public string Title { get; set; }
        public int Year { get; set; }
        public decimal Price { get; set; }
        public string AuthorName { get; set; }
        public string Genre { get; set; }
    }

    """


let test02 =
    run classExpr """
    public class CreateCloudTrial : Irgendwas
    	{
    		public string CompanyName { get; set; } 
    		public string FirstName { get; set; }
    		public string LastName { get; set; }
    		public string EMail { get; set; }
    		public string Country { get; set; }
    		public string PhoneNumber { get; set; }
    		public string Datacenter { get; set; }
    		public string CustomDnsName { get; set; }
    		public string AdminUsername { get; set; }
    		public string AdminPassword { get; set; }
    		public string AdminFullname { get; set; }
    
    		
    		public string ConfirmationMailRecipient { get; set; }
    		
    		public string ConfirmationMailCulture { get; set; }
    
    	
    		public string ApplicationContext { get; set; }
    
    		public string OrganizationName { get; set; }
    
    		public string KineticSolution { get; set; }
    
    		public string KineticSolutionCulture { get; set; }
    
    		public Guid? CorrelationId { get; set; }
    		public Guid? SubjectId { get; set; }
    
    	
    		public string AzureFunctionInstanceId { get; set; }
    
    	}

    """


// Process tree

let (|EndsWith|_|) (v:string) (str:string) =
    if str.EndsWith(v) then
        Some (str.Substring(0,str.Length-v.Length))
    else
        None


let (|HasNullableNotation|_|) (str:string) =
    let nullableStart = "Nullable<"
    if str.StartsWith(nullableStart) then
        let newStr = str.Replace(nullableStart,"").Replace(">","")
        Some newStr
    else
        None

let processTree exprlist =
    let rec inner expr =
        match expr with
        | Class (name,exps)  ->
            sprintf "type %s = { %s }" 
                name 
                (
                    ("",exps)
                    ||> List.fold (fun state item -> 
                        match item with
                        | Property _ ->
                            sprintf "%s\r\n    %s" state (inner item)
                        | _ -> state
                    )
                )
        | Property (typename,name) ->
            let typename =
                match typename with
                | EndsWith "?" t                    
                | HasNullableNotation t ->
                    sprintf "%s option" t
                | _ -> typename
            sprintf "%s:%s" name typename
        | _ ->
            ""
    exprlist |> List.map (fun x -> inner x)

match test02 with
| Success (result,_,_) -> 
    processTree [result]
| Failure (errorMsg,_,_) -> [ sprintf "Failure: %s" errorMsg ]

    
    

