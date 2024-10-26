(* anonymizer
 * main executable module
 * (C) Matt Teichman, 2019 *)

open Prelude
open Encryption
open Io
open Messages

(* location to check for config file *)
let path
    : string
  = ~~"~/.anonrc"

(* encryption key *)
let full_key
    : string
  = get_key path
  
(* boolean saying whether config file is at least 30 characters long *)
let key_ok
    : bool
  = String.length full_key >= G.cap

(* boolean saying whether there's a config file *)
let is_there_config
    : bool
  = full_key = default_key

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

let main
    : bool -> G.t -> string -> bool -> string -> string list -> unit
  = fun itconfig enc_or_dec key key_ok arg args ->
  if key_ok
  then match C.validate enc_or_dec arg key args with
       | Ok [] -> exit_gracefully (usage_message arg)
       | Ok lst -> mass_rename itconfig enc_or_dec key arg lst
       | Error msg -> exit_gracefully msg
  else exit_gracefully (short_config arg)

(* run program in either encrypting or decrypting mode *)
let () = if not !Sys.interactive
         then syserrors
                (main
                   is_there_config
                   (whoami ())
                   full_key
                   key_ok
                   argv0)
                argv
         else ()
