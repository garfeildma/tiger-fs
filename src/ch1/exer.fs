module Exer

type Key = string

type Tree = Leaf | Tree of Tree * Key * Tree

type 'a GTree = GLeaf | GTree of ('a GTree) * Key * 'a * ('a GTree)

let rec insert key tree =
    match tree with
    | Leaf -> Tree(Leaf, key, Leaf)
    | Tree(l, k, r) ->
        if key < k then
            Tree(insert key l, k, r)
        elif key < k then
            Tree(l, k, insert key r)
        else Tree(l, key, r)

let rec isMember tree key =
    match tree with
    | Leaf -> false
    | Tree(l, k, r) ->
        if key = k then true
        elif key < k then isMember l key
        else isMember r key

let rec insertG gTree key value =
    match gTree with
    | GLeaf -> GTree(GLeaf, key, value, GLeaf)
    | GTree(l, k, v, r) ->
        if key < k then
            GTree(insertG l key value, k, v, r)
        elif key > k then
            GTree(l, k, v, insertG r key value)
        else GTree(l, key, value, r)

let rec lookup gTree key =
    match gTree with
    | GLeaf -> None
    | GTree(l, k, v, r) ->
        if key = k then Some(v)
        elif key < k then lookup l key
        else lookup r key