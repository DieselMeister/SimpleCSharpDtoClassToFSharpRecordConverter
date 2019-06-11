module Domain


open FParsec


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
    

let restParser : Parser<Expr, NodeState> = 
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

let classExpr = 
    classNameExpr .>>.
    innerClassExpr
    |>> (fun (x,y) -> Class (x,y))

let classesExpr = 
    many1 (
        classExpr
    )

cassParseExprImpl := many1 (choice [attempt classExpr;attempt propertyExpr;scopeExpr;restParser] )


module TreeProcessing =

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



    
    

