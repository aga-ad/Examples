module Calc.AST

open System.Collections.Generic
open Microsoft.FSharp.Core.Operators

type Var = string

let mutable returnVal = nan

let vars = new Dictionary<string, float>()

type Func =
    | Cos
    | Sin

type Op = 
    | Pow
    | Plus
    | Mult
    | Div
    | Minus

type Expr =
    | Num of float
    | EVar of Var
    | BinOp of Op*Expr*Expr*float
    | UnaryOp of Func*Expr*float

type Stmt = 
    | EqStmt of Var*Expr
    | SingleExpr of Expr

type result = list<Stmt>*Dictionary<string,float>*float //Statements, variables, return value

let getValue e = match e with
   | Num num -> num
   | BinOp (op, l, r, result) -> result
   | UnaryOp (f, arg, result) -> result
   | EVar (name) -> vars.[name]

let calcFunc l (op, r) =
   let operator = match op with
   | Plus  -> (+)
   | Minus -> (-)
   | Mult  -> (*)
   | Div   -> (/)
   | Pow   -> ( ** )
   BinOp(op,l,r, operator (getValue l) (getValue r))

let calcUnaryFunc arg f =
    let func = match f with
    | Cos -> cos
    | Sin -> sin
    UnaryOp(f, arg, func (getValue arg))
