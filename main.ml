(* anonymizer
 * main executable module
 * (C) Matt Teichman, 2019 *)

open Prelude
open Encryption

(* exception type for if the executable accidentally gets renamed to
   something strange *)
exception Whoami of string

(* check whether program is being run as 'anonymizer' or 'decrypter' *)
let whoami
    : unit -> G.t
  = fun _ ->
  match Filename.basename argv0 with
  | "anonymizer" -> G.Encrypt
  | "decrypter" -> G.Decrypt
  | _ -> raise (Whoami argv0)

(* run program in either encrypting or decrypting mode *)
let () = if not !Sys.interactive
         then syserrors (Main.main (whoami ()) argv0) argv
         else ()
