(* immutable array
 * IA.mli
 * Keith WACLENA <http://www.lib.uchicago.edu/keith/>
 *
 * Copyright 2017 Keith Waclena. All rights reserved.
 * Distributed under the GPL2 license, see terms at the end of the file.
 *)

(** *)
(** {2 Types and Values} *)

type 'a t = private 'a array (** The type of immutable arrays. *)

(** The empty array. *)
val empty : 'a t

(** [(of_array a)] converts a mutable ['a array] to an immutable ['a t]. *)
val of_array : 'a array -> 'a t
(** [!] is [of_array].  *)
val (!) : 'a array -> 'a t

(** The remainder of these functions are identical to those in
    {!Prelude.Array} except that they are immutable. *)

val get : 'a t -> int -> 'a
val init : int -> (int -> 'a) -> 'a t
val iteri : (int -> 'a -> unit) -> 'a t -> unit
val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t
val sub : 'a t -> int -> int -> 'a t
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
