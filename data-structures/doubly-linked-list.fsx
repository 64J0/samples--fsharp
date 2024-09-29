// Doubly-linked list data structure from:
//
// https://rosettacode.org/wiki/Doubly-linked_list/Definition#F#

module DLL =
    type DListAux<'T> =
        { mutable prev: DListAux<'T> option
          data: 'T
          mutable next: DListAux<'T> option }

    type DList<'T> =
        { mutable front: DListAux<'T> option
          mutable back: DListAux<'T> option }

    let empty () = { front = None; back = None }

    let addFront (dlist: DList<'T>) (elt: 'T) =
        match dlist.front with
        | None ->
            let e = Some { prev = None; data = elt; next = None }
            dlist.front <- e
            dlist.back <- e
        | Some e2 ->
            let e1 =
                Some
                    { prev = None
                      data = elt
                      next = Some e2 }

            e2.prev <- e1
            dlist.front <- e1

    let addBack (dlist: DList<'T>) (elt: 'T) =
        match dlist.back with
        | None -> addFront dlist elt
        | Some e2 ->
            let e1 =
                Some
                    { prev = Some e2
                      data = elt
                      next = None }

            e2.next <- e1
            dlist.back <- e1

    let addAfter (dlist: DList<'T>) (link: DListAux<'T>) (elt: 'T) =
        if link.next = None then
            addBack dlist elt
        else
            let e =
                Some
                    { prev = Some link
                      data = elt
                      next = link.next }

            link.next <- e

            match e.Value.next with
            | Some nextNode -> nextNode.prev <- e
            | None -> dlist.back <- e

    let rec printForward (node: DListAux<'T> option) =
        match node with
        | None -> ()
        | Some n ->
            printfn "%A" n.data
            printForward n.next

    let printForwardList (dlist: DList<'T>) = printForward dlist.front

    let rec printBackward (node: DListAux<'T> option) =
        match node with
        | None -> ()
        | Some n ->
            printfn "%A" n.data
            printBackward n.prev

    let printBackwardList (dlist: DList<'T>) = printBackward dlist.back

let main () : int =
    let dll = DLL.empty ()
    printfn "Empty list:"
    DLL.printForwardList dll

    printfn "\nAdding to front:"
    DLL.addFront dll 1
    DLL.addFront dll 2
    DLL.printForwardList dll

    printfn "\nAdding to back:"
    DLL.addBack dll 3
    DLL.addBack dll 4
    DLL.printForwardList dll

    printfn "\nAdding after the second element:"

    match dll.front with
    | Some f when Option.isSome f.next -> DLL.addAfter dll (Option.get f.next) 5
    | _ -> printfn "List is too short to add after the second element"

    DLL.printForwardList dll

    // Implement and use printBackward here to demonstrate reverse traversal
    printfn "\nPrinting the second example backwards:"
    DLL.printBackwardList dll

    // Create a new doubly-linked list
    let dlist = DLL.empty ()

    // Add elements to the list: 1, 2, 3
    DLL.addBack dlist 1.
    DLL.addBack dlist 2.
    DLL.addBack dlist 3.

    // Get reference to the second node (with value 2)
    let secondNode = dlist.front.Value.next.Value

    // Add 2.5 after the second node
    DLL.addAfter dlist secondNode 2.5

    // Print the list
    printfn "\nSecond example:"
    DLL.printForwardList dlist

    0

main ()
