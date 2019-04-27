(* anonymizer
 * library module
 * (C) Matt Teichman, 2019 *)

open Prelude

module Globals = struct
  (* ADT representing whether app is in encrypting or decrypting mode *)
  type t = Encrypt | Decrypt

  (* maximum file prefix length *)
  let cap
      : int
    = 30
end
(* short module alias *)
module G = Globals

module Crypto = struct
  (* helper function for hashing *)
  let hash
      : (unit -> Cryptokit.hash) -> string -> string
    =
    let open Cryptokit in
    let open Hexa in
    fun hashgen str ->
    hash_string (hashgen ()) str
    |> transform_string @@ encode ()

  (* return the SHA hash of a string *)
  let sha
      : string -> string
    =
    fun str ->
    (hash Cryptokit.Hash.sha256) str
end

module Manip = struct
  (* drag cap into local namespace *)
  let cap
      : int
    = G.cap

  (* make input string into a string of length 30, padding if necessary *)
  let prep_string
      : string -> string
    = fun str ->
    let open String in
    if length str >= cap
    then take cap str
         |> rev
    else str ^ "/" ^ Crypto.sha str
         |> take cap
         |> rev
    
  (* convert string to list of ascii values of its characters *)
  let make_numeric
      : string -> int list
    =
    fun str ->
    prep_string str
    |> String.explode
    |> map int_of_char

  (* split a string into a list of two-character segments; not safe for
     odd-length strings *)
  let rec twos
          : string -> string list
    =
    fun str ->
    assert (String.length str |> even)
    ; match str with
      | "" -> []
      | s -> String.take 2 s :: (String.drop 2 s |> twos)
     
  (* prep and then XOR encrypt a file prefix using input key *)
  let encrypt
      : string -> string -> string
    =
    fun key str ->
    let lst1 = make_numeric str in
    let lst2 = make_numeric key in
    map2 (lxor) lst1 lst2
    |> map @@ Printf.sprintf "%02X"
    |> String.join ~sep:""

  (* convert string hex representation to integer *)
  let hex_to_int
      : string -> int
    =
    fun str ->
    int_of_string @@ "0x" ^ str

  (* XOR decrypt a file prefix using input key, then trim the pad to get back to
     original string *)
  let decrypt
      : string -> string -> string
    =
    fun key str ->
    let lst2 = make_numeric key in
    twos str
    |> map hex_to_int
    |> map2 (lxor) lst2
    |> map char_of_int
    |> rev
    |> map @@ String.make 1
    |> String.join ~sep:""
    |> String.split ~sep:"/"
    |> function x :: _ -> x
              | _ -> assert false

  (* encrypt a full file name, including underscore *)
  let encrypt_fn
      : string -> string -> string
    = fun key fn ->
    let pair =
      String.split ~sep:"_" fn
    in
    match pair with
    | [name;ext] -> encrypt key name ^ "_" ^ ext
    | _ -> assert false             

  (* decrypte a full file name, including underscore *)
  let decrypt_fn
      : string -> string -> string
    = fun key fn ->
    let pair =
      String.split ~sep:"_" fn
    in
    match pair with
    | [name;ext] -> decrypt key name ^ "_" ^ ext
    | _ -> assert false
end
(* alias *)
module M = Manip

module Result = struct
  (* pull in Result type from OCaml Pervasives *)
  type ('a, 'b) t = ('a, 'b) result

  (* helper function to tag a value with Error data constructor *)
  let err
      : 'b -> ('a, 'b) t
    = fun x -> Error x

  (* helper function to tag a value with Ok data constructor *)
  let ok
      : 'a -> ('a, 'b) t
    = fun x -> Ok x

  (* helper function for sequencing in Result monad *)
  let rsmush
      : ('a list, 'b) t -> ('a, 'b) t -> ('a list, 'b) t
    = fun r1 r2 ->
    match r1, r2 with
    | Ok _, Error msg -> Error msg
    | Ok lst, Ok str -> Ok (lst @ [str])
    | Error msg, Ok _ -> Error msg
    | Error msg1, Error _ -> Error msg1

  (* Haskell-style sequence function for Result monad *)
  let sequence
      : ('a, 'b) t list -> ('a list, 'b) t
    = fun list_of_rs ->
    foldl rsmush (Ok []) list_of_rs

  (* Haskell-style fmap function for Result monad *)
  let fmap
      : ('a -> 'b) -> ('a, 'c) t -> ('b, 'c) t
    = fun f rlist ->
    match rlist with
    | Ok lst -> Ok (f lst)
    | Error msg -> Error msg

  (* Haskell-style join function for Result monad *)
  let join
      : (('a, 'b) t, 'b) t -> ('a, 'b) t
    = fun rrlist ->
    match rrlist with
    | Ok (Ok x) -> Ok x
    | Ok (Error msg) -> Error msg
    | Error msg -> Error msg

  (* Haskell-style pure function for Result monad *)
  let pure
      : 'a -> ('a, 'b) t
    = ok

  (* turn a validation function for a single value into the corresponding
     validation function over a list of values *)
  let lift
      : ('a -> ('b, 'c) t) -> 'a list -> ('b list, 'c) t
    = fun mf lst ->
    map mf lst
    |> sequence

  (* Haskell-style bind function for Result monad *)
  let bind
      : ('a, 'b) t -> ('a -> ('c, 'b) t) -> ('c, 'b) t
    = fun rlst f ->
    fmap f rlst
    |> join

  (* infix version of bind *)
  let (>>=) = bind
end

module Io = struct
  open Result

  (* pull cap into local namespace *)
  let cap :
        int
    = G.cap
  
  (* location to check for config file *)
  let path
      : string
    = ~~"~/.anonrc"

  (* default key for when there isn't a config file *)
  let default_key
      : string
    = "There's such a sad love/deep in your eyes/\
       a kind of pale jewel/open and closed/\
       within your eyes/I'll place the sky"

  (* grab key from config file if it exists; otherwise use default *)
  let get_key
      : string -> string
    =
    fun fn ->
    match readfile fn with
    | exception _ -> default_key
    | x -> x

  (* encryption key *)
  let full_key
      : string
    = get_key path

  (* error messages *)
  module Messages = struct
    let usage_message
        : string
      = Printf.sprintf "USAGE: %s PAPER ..." argv0

    let exit_gracefully 
        : string -> unit
      = fun msg ->
      print msg
      ; exit 1

    let too_long
        : string -> int -> int -> string
      = fun fn wanted got ->
      sprintf "%s: file prefix must be fewer than %i characters long\n\
               this prefix is %i characters long" fn wanted got

    let wrong_length
        : string -> int -> int -> string
      = fun fn wanted got ->
      sprintf "%s: file prefix must be exactly %i characters long\n\
               this prefix is %i characters long" fn wanted got

    let no_config
        : string
      = "warning: no config file \n\
         please create a config file called ~/.anonrc \n\
         any string will work as a key, as long as it's \
         30 characters long"

    let short_config
        : string
      = sprintf "%s: ~/.anonrc config file must be at least 30 \
                 characters long." argv0
  end
  module Msg = Messages

  module Checks = struct
    (* check whether a string has exactly one underscore somewhere not on the
       periphery *)
    let underscore
        : string -> (string, string) t
      = fun fn ->
      let ucount =
        String.foldl (fun acc c -> if c = '_'
                                   then succ acc
                                   else acc) 0
      in
      let errm =
        err @@ sprintf "%s: file name must contain two parts \
                        separated by exactly one underscore" fn
      in
      match String.split ~elide:false ~sep:"_" fn with
      | ["";_] | [_;""] -> errm
      | _ -> if ucount fn = 1
             then ok fn
             else errm

    (* map string with exactly one underscore to its prefix; not safe for
       strings that don't pass the 'underscore' validation *)
    let prefix
        : string -> string
      = fun str ->
      match String.split ~sep:"_" str with
      | [pfx;_] -> pfx
      | _ -> assert false

    (* check whether string is the correct length, depending on whether the
       program is encrypting or decrypting *)
    let long_enough
        : G.t -> string -> (string, string) t
      = fun enc_or_dec str ->
      let do_check msg cap str
        = if String.length @@ prefix str <= cap
          then ok str
          else err
               @@ msg str cap
               @@ String.length
               @@ prefix str
      in
      match enc_or_dec with
      | Encrypt -> do_check Msg.too_long G.cap str
      | Decrypt -> do_check Msg.wrong_length (G.cap * 2) str

    (* list of all validations to be performed on argv, in the order they are to
       be performed *)
    let check_list
        : G.t -> (string -> (string, string) t) list
      = fun enc_or_dec -> [
          underscore
        ; long_enough enc_or_dec
        ]

    (* turn a list of individual validation functions into one big validation
       function that sequences all of them over the entire argv *)
    let mk_validate
        : string list ->
          (string -> (string, string) t) list ->
          (string list, string) t
      = fun strings clist ->
      map lift clist 
      |> foldl (>>=) (strings |> rev |> pure)
  end
  module C = Checks

  (* rename a file to its encrypted/decrypted version *)
  let rename
      : G.t -> string -> unit
    = fun which fn ->
    let open G in
    let rename' func_name =
      Sys.rename fn (func_name full_key fn)
      ; func_name full_key fn
        |> Printf.printf "%s >> %s\n" fn
    in
    match which with
    | Encrypt -> rename' M.encrypt_fn
    | Decrypt -> rename' M.decrypt_fn

  (* function that performs all validations on all files in the argv *)
  let validate
      : G.t -> string list -> (string list, string) Result.t
    = fun enc_or_dec strings ->
    C.mk_validate strings @@ C.check_list enc_or_dec
end

module Main = struct
  open Io
  open Messages

  (* check to make sure config file is at least 30 characters long *)
  let key_ok
      : bool
    = String.length full_key >= G.cap

  (* boolean saying whether there's a config file *)
  let configless
      : bool
    = full_key = default_key

  (* rename all files in an input list, with warning if there's no config *)
  let mass_rename
      : bool -> G.t -> string list -> unit
    = fun configless enc_or_dec lst ->
    let doit () =
      iter (rename enc_or_dec) lst
      ; exit 0
    in
    if configless
    then (print no_config ; doit ())
    else doit ()

  let main
      : G.t -> unit
    = fun enc_or_dec ->
    if key_ok
    then match validate enc_or_dec argv with
         | Ok [] -> exit_gracefully usage_message
         | Ok lst -> mass_rename configless enc_or_dec lst
         | Error msg -> exit_gracefully msg
    else exit_gracefully short_config
end