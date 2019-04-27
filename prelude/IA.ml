(* immutable arrays
 * IA.ml is part of prelude
 * Keith WACLENA <http://www.lib.uchicago.edu/keith/>
 *
 * Copyright 2017 Keith Waclena. All rights reserved.
 * Distributed under the GPL2 license, see terms at the end of the file.
 *)

include Array

type 'a t = 'a array            (* we need a private type *)

let empty = [||]
let of_array a = a
let (!) = of_array

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
