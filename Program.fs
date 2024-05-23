namespace CA2
open System
open System.Collections.Generic

    module AST =     
        type Expr =       
            | Sym of name: string       
            | Func of name: string * args: Expr list 
            with override this.ToString() =                
                    match this with                
                    |Sym(name) -> name                
                    |Func(name=n1;args=a1) ->                    
                        let args = [for a in a1 -> a.ToString()] |> String.concat ", "                    
                        $"{n1}({args})"    
        
        type Rule = {           
            In:Expr           
            Out:Expr           
            }           
            with override this.ToString() = $"[ {string this.In} => {string this.Out} ]"    
            
        type Bindings = Dictionary<string,Expr>
        
    module Program =    
        open AST    
        [<EntryPoint>]    
        let main args =
            
            let rec substitute expr (bindings:Bindings) =            
                match expr with            
                | Sym(name) ->                 
                    match bindings.TryGetValue name with                    
                        |true, v -> v                    
                        |false, _ -> expr            
                | Func(name, args) ->                 
                    let new_name =                     
                        match bindings.TryGetValue name with                        
                            |true, Sym(s) -> s                        
                            |false, _ -> name                        
                            |true, _ -> failwith "Expected symbol in the place of the functor name"                
                    
                    let  new_args = [for a in args -> substitute a bindings]
                    Func(new_name, new_args)        
                    
            let findMatches pattern value =            
                let bindings = new Bindings()            
                let rec loop pattern value =               
                    match (pattern,value) with               
                    | (Sym(name), _) ->                     
                        match bindings.TryGetValue name with                    
                            | true, v ->                         
                                v = value                    
                            | false, _ ->                         
                                bindings.Add( name, value)                        
                                true               
                    | (Func(name=name1; args=args1), Func(name=name2; args=args2)) ->                    
                        if name1 = name2 && args1.Length = args2.Length then                        
                            [0..args1.Length-1]                        
                            |> List.fold (fun acc i -> acc && loop args1[i] args2[i])  true                     
                        else                        
                            false               
                    | _ -> false                        
                        
                if loop pattern value then                
                    Some(bindings)            
                else                 
                    None        
                
            let rec apply_all rule  expr =             
                match findMatches rule.In expr with            
                | Some(bindings) ->                
                substitute rule.Out bindings            
                | None ->                
                    match expr with                    
                    |Sym(_) -> expr                    
                    |Func(name, args) ->                         
                        let new_args = [for a in args -> apply_all rule a]                        
                        Func(name, new_args)
                        
            let print rule value =             
                printfn "Rule: %O" rule            
                printfn "Value: %O" value            
                let res = apply_all rule value            
                printfn "%O" res            
                let bindings = findMatches rule.In value            
                if bindings.IsSome then                
                    for m in bindings.Value do                    
                        printfn "%O" m            
                        printfn "======="        
            
            
            let arule = {           
                            In=Sym("A")           
                            Out=Sym("A")        
                            }        
            let swap ={           
                            In=Func ("swap", [Func("pair", [Sym("A");Sym("B")])])           
                            Out=Func("pair", [Sym("B");Sym("A")])        
                            }        
            let a = Sym("X")        
            let b = Func("f", [Sym("x")])        
            let d = Func ("swap",                         
                [Func("pair",                             
                    [Sym("x");Sym("y")])])        
                    
            let e = Func ("swap",                         
                [Func("pair",                             
                    [Func("f", [Sym("x")]);                                   
                        Func("g", [Sym("y")])])])        
            let f = Func("foo",            
                [Func ("swap",                         
                    [Func("pair",                             
                        [Func("f", [Sym("a")]);                                   
                            Func("g", [Sym("b")])])]);            
                Func ("swap",                         
                    [Func("pair",                             
                        [Func("q", [Sym("c")]);                                   
                            Func("z", [Sym("d")])])])        
                            ])        
            
            print swap f
            0