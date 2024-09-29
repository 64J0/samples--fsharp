// Binary tree representation using F#.
//
// Code from:
//
// https://theburningmonk.com/2016/12/implementing-a-bst-in-f/
// https://theburningmonk.com/2016/12/level-order-tree-traversal-in-fsharp/
// https://theburningmonk.com/2016/12/depth-first-tree-traversal-in-f/

/// Binary tree type definition.
type Tree<'a when 'a: comparison> =
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

    let traverse2 (tree: Tree<'a>) : 'a seq =
        let rec getNodeValues (acc: 'a list) (nodes: Tree<'a> list) : 'a list =
            match nodes with
            | [] -> acc
            | _ ->
                let nodes' = []

                let ans =
                    List.fold
                        (fun acc n ->
                            match n with
                            | Empty -> acc
                            | Node(value, left, right) ->
                                let acc' = value :: fst acc
                                let node' = snd acc @ [ left; right ]
                                (acc', node'))
                        (acc, nodes')
                        nodes

                getNodeValues (fst ans) (snd ans)

        match tree with
        | Empty -> Seq.empty
        | Node(value, left, right) ->
            let acc = [ value ]
            let nodes = [ left; right ]
            getNodeValues acc nodes |> List.rev |> List.toSeq

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
        |> insert 11
        |> insert 15
        |> insert 4
        |> insert 8

    printfn "Is binary tree? %b" (isBST t)

    printfn "Breath-first traversal:"
    BFS.traverse t |> Seq.iter (printfn "%d")

    printfn "Breath-first traversal 2:"
    BFS.traverse2 t |> Seq.iter (printfn "%d")

    printfn "Depth-first traversal:"
    DFS.traverse t |> Seq.iter (printfn "%d")

    // Example provided by CodeRabbit AI
    // Define the binary tree:
    //       10
    //      /  \
    //     5   15
    //         /  \
    //        6   20

    let exampleTree =
        Node(
            10,
            Node(5, Empty, Empty),
            Node(
                15,
                Node(6, Empty, Empty), // This node violates the BST property
                Node(20, Empty, Empty)
            )
        )

    let result = isBST exampleTree
    printfn "Is the valid tree a BST? %b" result // prints false

    // Define a valid BST:
    //       10
    //      /  \
    //     5   15
    //         /  \
    //        12  20

    let validBST =
        Node(
            10,
            Node(5, Empty, Empty),
            Node(
                15,
                Node(12, Empty, Empty), // This node does violates the BST property
                Node(20, Empty, Empty)
            )
        )

    // Test the isBST function
    let validResult = isBST validBST
    printfn "Is the valid tree a BST? %b" validResult

main ()
