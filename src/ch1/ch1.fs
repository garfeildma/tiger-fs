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

type Table = (Id * int) list

let prog = CompoundStm(AssignStm("a", OpExp(NumExp 5, Plus, NumExp 3)),
            CompoundStm(AssignStm("b",
                            EseqExp(PrintStm[IdExp "a"; OpExp(IdExp "a", Minus, NumExp 1)],
                                OpExp(NumExp 10, Times, IdExp "a"))),
                            PrintStm[IdExp "b"]))

let max a b = if a > b then a else b
let rec maxArgs = function
    | CompoundStm (s1, s2) -> max (maxArgs s1) (maxArgs s2)
    | AssignStm (_, exp) -> maxOfExp exp
    | PrintStm exps -> List.length exps
and maxOfExp = function
    | IdExp _ -> 0
    | NumExp _ -> 0
    | OpExp (e1, _, e2) -> max (maxOfExp e1) (maxOfExp e2)
    | EseqExp (stm, e) -> max (maxArgs stm) (maxOfExp e)

let lookup (table: Table) (id: Id) =
    let el = table |> List.find (fun el -> fst el = id)
    snd el

let update (table: Table) (id: Id) (value: int) =
    (id, value)::table

let apply m n op =
    match op with
    | Plus -> m + n
    | Minus -> m - n
    | Times -> m * n
    | Div -> m / n

let rec interpStm stm table =
    match stm with
    | CompoundStm (s1, s2) -> interpStm s2 (interpStm s1 table)
    | AssignStm (id, exp) ->
        let (n, t1) = interpExp exp table
        update t1 id n
    | PrintStm args ->
        match args with
        | [] ->
            printfn ""
            table
        | head::rest ->
            let (n, t1) = interpExp head table
            printfn "%A" n
            interpStm (PrintStm rest) t1
and interpExp exp t =
    match exp with
    | IdExp id -> (lookup t id, t)
    | NumExp num -> (num, t)
    | OpExp (e1, op, e2) ->
        let (n, t1) = interpExp e1 t
        let (m, t2) = interpExp e2 t1
        (apply n m op, t2)
    | EseqExp (stm, e) ->
        let t1 = interpStm stm t
        interpExp e t1

// interpStm prog []