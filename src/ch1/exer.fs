module Exer

type Key = string

type Tree = Leaf | Tree of Tree * Key * Tree

let rec insert key tree =
    match tree with
    | Leaf -> Tree(Leaf, key, Leaf)
    | Tree(l, k, r) ->
        if key < k then
            Tree(insert key l, k, r)
        elif key < k then
            Tree(l, k, insert key r)
        else Tree(l, key, r)