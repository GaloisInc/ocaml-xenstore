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

(** A byte-level transport over the xenstore Unix domain socket *)

(* The unix domain socket may not exist, or we may get a connection
   refused error if the server is in the middle of restarting. *)
let initial_retry_interval = 0.1 (* seconds *)
let max_retry_interval = 5.0 (* seconds *)
let retry_max = 100 (* attempts *)

let xenstored_socket = ref "/var/run/xenstored/socket"

open Lwt

type 'a t = 'a Lwt.t
let return x = return x
let ( >>= ) m f = m >>= f

(* Individual connections *)
type channel = {
  fd: Lwt_unix.file_descr;
  sockaddr: Lwt_unix.sockaddr;
  in_buffer: Cstruct.t;
  mutable in_seq: int64;
  out_buffer: Cstruct.t;
  mutable out_seq: int64;
}

let alloc (fd, sockaddr) =
  let in_buffer = Cstruct.create 4096 in
  let out_buffer = Cstruct.create 4096 in
  let in_seq = 0L in
  let out_seq = 0L in
  return { fd; sockaddr; in_buffer; in_seq; out_buffer; out_seq }

let create () =
  let sockaddr = Lwt_unix.ADDR_UNIX(!xenstored_socket) in
  let fd = Lwt_unix.socket Lwt_unix.PF_UNIX Lwt_unix.SOCK_STREAM 0 in
  let start = Unix.gettimeofday () in
  let rec retry n interval =
    if n > retry_max
    then fail (Failure (Printf.sprintf "Failed to connect to xenstore after %.0f seconds" (Unix.gettimeofday () -. start)))
    else
      try_lwt
        Lwt_unix.connect fd sockaddr
      with _ ->
        lwt () = Lwt_unix.sleep interval in
        retry (n + 1) (interval +. 0.1) in
  retry 0 initial_retry_interval >>= fun () ->
  alloc (fd, sockaddr)

let destroy { fd } = Lwt_unix.close fd

let complete op fd buf =
  let ofs = buf.Cstruct.off in
  let len = buf.Cstruct.len in
  let buf = buf.Cstruct.buffer in
  let rec loop acc fd buf ofs len =
    op fd buf ofs len >>= fun n ->
    let len' = len - n in
    let acc' = acc + n in
    if len' = 0 || n = 0
    then return acc'
    else loop acc' fd buf (ofs + n) len' in
  loop 0 fd buf ofs len >>= fun n ->
  if n = 0 && len <> 0
  then fail End_of_file
  else return ()

module Reader = struct
  type t = channel

  let next t =
    Lwt_bytes.read t.fd t.in_buffer.Cstruct.buffer t.in_buffer.Cstruct.off t.in_buffer.Cstruct.len >>= fun n ->
    return (t.in_seq, Cstruct.sub t.in_buffer 0 n)

  let ack t seq =
    t.in_seq <- seq;
    return ()
end

module Writer = struct
  type t = channel

  let next t =
    return (t.out_seq, t.out_buffer)

  let ack t seq =
    complete Lwt_bytes.write t.fd (Cstruct.sub t.out_buffer 0 Int64.(to_int (sub seq t.out_seq))) >>= fun () ->
    t.out_seq <- seq;
    return ()
end

let read { fd } = complete Lwt_bytes.read fd
let write { fd } = complete Lwt_bytes.write fd

let int_of_file_descr fd =
	let fd = Lwt_unix.unix_file_descr fd in
	let (fd: int) = Obj.magic fd in
	fd

let address_of { fd } =
	let creds = Lwt_unix.get_credentials fd in
	let pid = creds.Lwt_unix.cred_pid in
	lwt cmdline =
			Lwt_io.with_file ~mode:Lwt_io.input
				(Printf.sprintf "/proc/%d/cmdline" pid)
				(fun ic ->
					lwt cmdline = Lwt_io.read_line_opt ic in
					match cmdline with
						| Some x -> return x
						| None -> return "unknown") in
	(* Take only the binary name, stripped of directories *)
	let filename =
		try
			let i = String.index cmdline '\000' in
			String.sub cmdline 0 i
		with Not_found -> cmdline in
	let basename = Filename.basename filename in
	let name = Printf.sprintf "%d:%s:%d" pid basename (int_of_file_descr fd) in
	return (Uri.make ~scheme:"unix" ~path:name ())

let domain_of _ = 0

(* Servers which accept connections *)
type server = Lwt_unix.file_descr

let _ =
	(* Make sure a write to a closed fd doesn't cause us to quit
	   with SIGPIPE *)
	Sys.set_signal Sys.sigpipe Sys.Signal_ignore

let listen () =
  let sockaddr = Lwt_unix.ADDR_UNIX(!xenstored_socket) in
  let fd = Lwt_unix.socket Lwt_unix.PF_UNIX Lwt_unix.SOCK_STREAM 0 in
  lwt () = try_lwt Lwt_unix.unlink !xenstored_socket with _ -> return () in
  Lwt_unix.bind fd sockaddr;
  Lwt_unix.listen fd 5;
  return fd

let rec accept_forever fd process =
  lwt conns, _ (*exn_option*) = Lwt_unix.accept_n fd 16 in
  let (_: unit Lwt.t list) = List.map (fun x -> alloc x >>= process) conns in
  accept_forever fd process

module Introspect = struct
  let read { fd } = function
    | [ "readable" ] -> Some (string_of_bool (Lwt_unix.readable fd))
    | [ "writable" ] -> Some (string_of_bool (Lwt_unix.writable fd))
    | _ -> None

  let ls t = function
    | [] -> [ "readable"; "writable" ]
    | _ -> []

  let write _ _ _ = false
end
