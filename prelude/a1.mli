(* Prelude.A1 1-based array
 * a1.mli
 * Keith WACLENA <http://www.lib.uchicago.edu/keith/>
 *
 * Copyright 2017 Keith Waclena. All rights reserved.
 * Distributed under the GPL2 license, see terms at the end of the file.
 *)

(** *)
(** {1 Types and Values} *)

type 'a t = private 'a array (** The type of 1-indexed arrays. *)

(** The empty array. *)
val empty : 'a t

(** [(of_array a)] converts a 0-based ['a array] to an ['a t]. *)
val of_array : 'a array -> 'a t
(** [!] is [of_array].  *)
val (!) : 'a array -> 'a t

(** {1 Functions with Changed Semantics}

    The following functions all use 1-based indexing where OCaml's
    [Array] uses 0-based indexing. *)

(** [(get a n)] is the element number [n] of array [a].

    The first element has number [1].  The last element has number [(len a)].

    Raise [(Invalid_argument _)] if [n] is outside the range [1] to
    [(len a)].

    Examples:
    - [(get [|1|] 1) = 1]
    - [(get [|1|] 0)] raises [(Invalid_argument _)]
  *)
val get : 'a t -> int -> 'a

(** [(set a n x)] modifies array [a] in place, replacing element number [n] with [x].

    Raise [(Invalid_argument _)] if [n] is outside the range [1] to [(len a)].
  *)
val set : 'a t -> int -> 'a -> unit

(** [(init n f)] returns a fresh array of length [n], with element
    number [i] initialized to the result of [(f i)].

    Raise [Invalid_argument] if [n < 0] or [n > Sys.max_array_length]. If the
    return type of [f] is [float], then the maximum size is only
    [Sys.max_array_length / 2].
 *)
val init : int -> (int -> 'a) -> 'a t

(** [(iteri f a)] is the same as {!iter}, but the function is applied
    with the 1-based index of the element as first argument, and the
    element itself as second argument.  *)
val iteri : (int -> 'a -> unit) -> 'a t -> unit

(** [(mapi f a)] is the same as {!map}, but the function is applied
    with the 1-based index of the element as first argument, and the
    element itself as second argument.  *)
val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t

(** [(sub a start len)] returns a fresh array of length [len],
    containing the elements number [start] to [start + len - 1] of
    array [a].

    Raise [Invalid_argument _] if [start] and [len] do not
    designate a valid subarray of [a]; that is, if [start < 0 ||
    len < 0 ||  start + len > len a].
 *)
val sub : 'a t -> int -> int -> 'a t

(** [(fill a ofs len x)] modifies the array [a] in place, storing [x]
    in elements number [ofs] to [ofs + len - 1].

    Raise [Invalid_argument _] if [ofs] and [len] do not designate a
    valid subarray of [a].*)
val fill : 'a t -> int -> int -> 'a -> unit

(** [(blit v1 o1 v2 o2 len)] copies [len] elements from array [v1],
    starting at element number [o1], to array [v2], starting at
    element number [o2].

    It works correctly even if [v1] and [v2] are the same array, and
    the source and destination chunks overlap.

    Raise [Invalid_argument _] if [o1] and [len] do not designate a
    valid subarray of [v1], or if [o2] and [len] do not designate a
    valid subarray of [v2]. *)
val blit : 'a t -> int -> 'a t -> int -> int -> unit

(** {1 Functions Unchanged from [Array]}

    The following functions are all exactly those of [Array] (with
    [Prelude] extensions), because they don't involve indexing. *)

val length : 'a t -> int
val len : 'a t -> int
val make : int -> 'a -> 'a t
val create_float : int -> float t
val make_matrix : int -> int -> 'a -> 'a t t
val append : 'a t -> 'a t -> 'a t
val concat : 'a t list -> 'a t
val copy : 'a t -> 'a t
val to_list : 'a t -> 'a list
val of_list : 'a list -> 'a t
val iter : ('a -> unit) -> 'a t -> unit
val map : ('a -> 'b) -> 'a t -> 'b t
val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
val fold_right : ('b -> 'a -> 'a) -> 'b t -> 'a -> 'a
val iter2 : ('a -> 'b -> unit) -> 'a t -> 'b t -> unit
val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
val for_all : ('a -> bool) -> 'a t -> bool
val exists : ('a -> bool) -> 'a t -> bool
val mem : 'a -> 'a t -> bool
val memq : 'a -> 'a t -> bool
val sort : ('a -> 'a -> int) -> 'a t -> unit
val stable_sort : ('a -> 'a -> int) -> 'a t -> unit
val fast_sort : ('a -> 'a -> int) -> 'a t -> unit
val foldl : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
val foldr : ('a -> 'b -> 'b) -> 'b -> 'a t -> 'b


(*
 * Copyright 2017 Keith Waclena
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301, USA.
 *)
