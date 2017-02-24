module Ch1

type Id = string
type BinOp = Plus | Minus | Times | Div

type Stm = CompoundStm of Stm * Stm
            | AssignStm of Id * Exp
            | PrintStm of Exp list
    and Exp = IdExp of Id
            | NumExp of int
            | OpExp of Exp * BinOp * Exp
            | EseqExp of Stm * Exp
let prog = CompoundStm(AssignStm("a", OpExp(NumExp 5, Plus, NumExp 3)),
            CompoundStm(AssignStm("b",
                            EseqExp(PrintStm[IdExp "a"; OpExp(IdExp "a", Minus, NumExp 1)],
                                OpExp(NumExp 10, Times, IdExp "a"))),
                            PrintStm[IdExp "b"]))

let rec maxArgs = function
    | CompoundStm (s1, s2) -> maxArgs s1 + maxArgs s2
    | AssignStm (_, exp) -> maxOfExp exp
    | PrintStm exps -> 1 + (exps |> List.fold (fun i exp -> i + maxOfExp exp) 0)
and maxOfExp = function
    | IdExp _ -> 0
    | NumExp _ -> 0
    | OpExp (e1, _, e2) -> maxOfExp e1 + maxOfExp e2
    | EseqExp (stm, e) -> maxArgs stm + maxOfExp e

// let rec interp =