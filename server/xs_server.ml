(*
 * Copyright (C) Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

open Lwt
open Xs_packet

module type TRANSPORT = sig
  type server
  val listen: unit -> server Lwt.t

  type t
  val read: t -> string -> int -> int -> int Lwt.t
  val write: t -> string -> int -> int -> int Lwt.t
  val destroy: t -> unit Lwt.t

  val accept_forever: server -> (t -> unit Lwt.t) -> 'a Lwt.t
end

module Server = functor(T: TRANSPORT) -> struct
  module PS = PacketStream(T)

  let handle_connection t =
    let channel = PS.make t in
    lwt request = PS.recv channel in
    let reply = Xs_packet.create (get_tid request) (get_rid request) Op.Error "Not implemented\000" in
    lwt () = PS.send channel reply in
    T.destroy t

  let serve_forever () =
    lwt server = T.listen () in
    T.accept_forever server handle_connection
end