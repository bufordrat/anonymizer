(* 1-based arrays
 * a1.ml is part of prelude
 * Keith WACLENA <http://www.lib.uchicago.edu/keith/>
 *
 * Copyright 2017 Keith Waclena. All rights reserved.
 * Distributed under the GPL2 license, see terms at the end of the file.
 *)
include Array

type 'a t = 'a array            (* we need a private type *)

let empty = [||]
(*$= empty
  empty (make 0 0)
  (len empty) 0
 *)

let of_array a = a
let (!) = of_array

(*$inject open Prelude open A1 *)

(* the modified functions *)
let get a i = get a (pred i)
(*$Q get
   Q.(small_int) (fun i -> not @@ succeeds (get empty) i)
   Q.(int_range 1 100) (fun n -> let i = Random.int n + 1 in get (init n id) i = i)
   Q.(int_range 1 100) (fun n -> not @@ succeeds (get (init n id)) 0)
 *)
let set a i x = set a (pred i) x
(*$Q set
  Q.(small_int) (fun i -> not @@ succeeds (set empty i) 48376)
  Q.(int_range 1 100) (fun n -> let a,i,x = (make n 0), (Random.int n + 1), Random.(int maxint) in set a i x; get a i = x)
  Q.(int_range 1 100) (fun n -> not @@ succeeds (set (init n id) 0) 82376)
 *)
let init n f = init n (fun i -> f (succ i))
(*$Q init
  Q.small_nat (fun n -> init n id |> len = n)
  Q.small_nat (fun n -> init n id |> to_list = 1--n)
 *)
let iteri f a = iteri (fun i x -> f (succ i) x) a
(*$Q iteri
  Q.small_nat (fun n -> let m = ref true in init n id |> iteri (fun i x -> m := m.contents && i=x); m.contents)
 *)
let mapi f a = mapi (fun i x -> f (succ i) x) a
(*$Q mapi
  Q.small_nat (fun n -> init n id |> mapi (fun i x -> i=x) |> to_list |> all ((=) true))
 *)
let sub a i j = sub a (pred i) j
(*$Q sub
  Q.small_nat (fun n -> let a = init n id in sub a 1 n = a)
 *)
let fill a i len x = fill a (pred i) len x
(*$Q fill
  Q.small_nat (fun n -> let a,m = make n 0, Random.int ~++n in fill a 1 n 1; sub a 1 m = make m 1)
 *)
let blit a i a' j len = blit a (pred i) a' (pred j) len
(*$Q blit
  Q.small_nat (fun n -> let a,a',m = make n 1, make n 0, Random.int ~++n in blit a 1 a' 1 m; sub a' 1 m = make m 1)
 *)

(* Prelude extensions *)
let len = length
let foldl = fold_left
let foldr f z a = fold_right f a z


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
