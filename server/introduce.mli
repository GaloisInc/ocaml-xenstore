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

val introduce: Domain.address -> unit Lwt.t
(** [introduce address] should be called whenever an introduce message
    is received from the toolstack. *)

val forget: Domain.address -> unit Lwt.t
(** [forget address] should be called whenever a domain is destroyed *)

val stream: Domain.address Lwt_stream.t
(** A stream of introduced addresses *)
