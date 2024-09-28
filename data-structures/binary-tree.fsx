// Binary tree representation using F#.
//
// Code from:
//
// https://theburningmonk.com/2016/12/implementing-a-bst-in-f/
// https://theburningmonk.com/2016/12/level-order-tree-traversal-in-fsharp/
// https://theburningmonk.com/2016/12/depth-first-tree-traversal-in-f/

/// Binary tree type definition.
type Tree<'a when 'a : comparison> =
    | Empty
    | Node of value: 'a * left: Tree<'a> * right: Tree<'a>

/// Insert a new element at the binary tree. If this element is already at the
/// tree, it is eventually ignored.
let rec insert newValue (tree: Tree<'a>) =
    match tree with
    | Empty -> Node(newValue, Empty, Empty)
    | Node(value, left, right) when newValue < value ->
        let left' = insert newValue left
        Node(value, left', right)
    | Node(value, left, right) when newValue > value ->
        let right' = insert newValue right
        Node(value, left, right')
    | _ -> tree

/// Helper function used at the delete function.
let rec findInOrderPredecessor (tree: Tree<'a>) : Tree<'a> =
    match tree with
    | Empty -> Empty
    | Node(_, _, Empty) -> tree
    | Node(_, _, right) -> findInOrderPredecessor right

/// Delete a value from a binary search tree.
let rec delete value (tree: Tree<'a>) =
    match tree with
    | Empty -> Empty
    | Node(value', left, right) when value < value' ->
        let left' = delete value left
        Node(value', left', right)
    | Node(value', left, right) when value > value' ->
        let right' = delete value right
        Node(value', left, right')
    | Node(_, Empty, Empty) -> Empty
    | Node(_, left, Empty) -> left
    | Node(_, Empty, right) -> right
    | Node(_, left, right) ->
        match findInOrderPredecessor left with
        | Empty -> Empty
        | Node(value', _, _) ->
            let left' = delete value' left
            Node(value', left', right)

/// Use this function to check if a binary tree is a binary search tree or not.
let isBST (tree: Tree<'a>) : bool =
    let rec verify lo hi tree =
        match tree with
        | Empty -> true
        | Node(value, left, right) ->
            match lo, hi with
            | Some lo, _ when value < lo -> false
            | _, Some hi when value > hi -> false
            | _ ->
                let hi' = defaultArg hi value |> min value |> Some
                let lo' = defaultArg lo value |> max value |> Some
                verify lo hi' left && verify lo' hi right

    verify None None tree

module BFS =
    let traverse (tree: Tree<'a>) : seq<'a> =
        let rec loop (trees: seq<Tree<'a>>) =
            seq {
                let values =
                    trees
                    |> Seq.choose (function
                        | Empty -> None
                        | Node(value, _, _) -> Some value)

                yield! values

                let subtrees =
                    trees
                    |> Seq.collect (function
                        | Empty -> Seq.empty
                        | Node(_value, left, right) ->
                            seq {
                                yield left
                                yield right
                            })
                    |> Seq.toArray

                if subtrees.Length > 0 then
                    yield! loop subtrees
            }

        loop <| seq { yield tree }

module DFS =
    let traverse (tree: Tree<'a>) : seq<'a> =
        let rec loop (tree: Tree<'a>) =
            seq {
                match tree with
                | Empty -> ()
                | Node(value, left, right) ->
                    yield value

                    yield! loop left
                    yield! loop right
            }

        loop tree

let main () =
    let t =
        Empty
        |> insert 10
        |> insert 3
        |> insert 2
        |> insert 1
        |> insert 5
        |> insert 6
        |> insert 7
        |> insert 3

    printfn "Is binary tree? %b" (isBST t)

    printfn "Breath-first traversal:"
    BFS.traverse t |> Seq.iter (printfn "%d")

    printfn "Depth-first traversal:"
    DFS.traverse t |> Seq.iter (printfn "%d")

main ()
