open Prelude
let lc = foldlines (fun n _ -> succ n) 0
let main () = argv |> whenever [] ["-"] |> map (within lc) |> sum |> printf "%d\n"
let () = syserrors main ()
