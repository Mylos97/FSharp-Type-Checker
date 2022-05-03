// Assignment 2 FPLI Spring 2022
// Made by Frederik, Mads, Martin 

// Define the two different types we are working with
type typ = TYPEINT 
         | TYPEBOOLEAN

// The operators of the Abstract syntax tree
type exp = INT of int 
         | TRUE
         | FALSE  
         | ADD                  of exp * exp
         | EQUAL                of exp * exp
         | NOTEQUAL             of exp * exp
         | AND                  of exp * exp
         | OR                   of exp * exp
         | LESSTHAN             of exp * exp
         | GREATERTHAN          of exp * exp
         | LESSTHANOREQUAL      of exp * exp
         | GREATERTHANOREQUAL   of exp * exp
         | SUBTRACT             of exp * exp
         | DIVISION             of exp * exp
         | MULTIPLY             of exp * exp
         | IF                   of exp * exp * exp 

// The function to check if the types are legal
// Some of the expressions has to have the same input so we can match for multiple cases
// Outputs the resulting type of the expression and if it fails it returns an error with the expected type
// Main.EQUAL(Main.INT 2, Main.INT 1);;
// Main.check [] it;; 
// val it: Main.typ = TYPEBOOLEAN

let rec check env = function
    | INT i          -> TYPEINT
    | TRUE           -> TYPEBOOLEAN
    | FALSE          -> TYPEBOOLEAN
    | ADD (e1, e2)  | SUBTRACT (e1, e2) | MULTIPLY (e1, e2) | DIVISION (e1, e2) -> 
        let v1 = check env e1
        let v2 = check env e2
        if v1 = TYPEINT then 
            if v2 = v1 then
                TYPEINT
            else failwith $"Type error expected int but got {v2}"
        else failwith $"Type error expected int but got {v1}"
    | IF (e1,e2,e3)  -> let v1 = check env e1
                        if v1 = TYPEBOOLEAN then 
                            let v2 = check env e2
                            let v3 = check env e3
                            if v2 = v3 then v2 
                                else failwith $"type error expected same type but got {v2} and {v3}"
                        else
                            failwith $"type error expected boolean type but got {v1}"
    | LESSTHAN (e1, e2) | GREATERTHAN (e1, e2) | GREATERTHANOREQUAL (e1, e2) | LESSTHANOREQUAL (e1, e2) ->
        let v1 = check env e1 
        let v2 = check env e2
        if v1 = TYPEINT then 
            if v2 = TYPEINT then TYPEBOOLEAN else
                failwith  $"type error expected boolean type but got {v2}"
        else failwith $"type error expected boolean type but got {v1}"
    | EQUAL (e1, e2) | NOTEQUAL (e1,e2) -> 
        match check env e1 with
            | TYPEINT ->
                let v2 = check env e2
                if v2 = TYPEINT then TYPEBOOLEAN else failwith $"Type error expected int but got {v2}"
            | TYPEBOOLEAN -> 
                let v2 = check env e2
                if v2 = TYPEBOOLEAN then TYPEBOOLEAN else failwith $"Type error expected boolean but got {v2}"
    | AND (e1, e2) | OR (e1, e2) -> 
        let v1 = check env e1
        let v2 = check env e2
        if v1 = TYPEBOOLEAN && v2 = TYPEBOOLEAN then TYPEBOOLEAN else failwith $"Type error expected boolean but got {v1} {v2}"
