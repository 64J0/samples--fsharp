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
        if link.next = dlist.back then
            addBack dlist elt
        else
            let e =
                Some
                    { prev = Some link
                      data = elt
                      next = link.next }

            link.next <- e

    let printForward (dlist: DList<'T>) : unit =
        match dlist.front with
        | None -> ()
        | Some(dListAux: DListAux<'T>) ->
            let mutable node = Some dListAux

            while (Option.isSome node) do
                let node' = Option.get node
                printfn "%A" node'.data
                node <- node'.next

let main () : int =
    let dll = DLL.empty ()
    DLL.addFront dll 0
    DLL.addFront dll 1
    DLL.addFront dll 2
    DLL.addFront dll 3
    DLL.addFront dll 4
    DLL.addBack dll 16
    DLL.addBack dll 17
    DLL.printForward dll

    0

main ()
