(*
 * Copyright (C) 2013, Galois, Inc.
 * All Rights Reserved.
 *
 * Written by Patrick Colp <pjcolp@galois.com>
 *)

type domid = int
type path = string
type context = string

type value =
  | DOMID
  | PATH
  | NONE

type xssm_operations = {
  read : domid -> path -> context -> unit;
  write : domid -> path -> context -> unit;
  create : domid -> path -> context -> unit;
  delete : domid -> path -> context -> unit;
  chmod : domid -> path -> context -> string -> unit;
  relabelfrom : domid -> path -> context -> unit;
  relabelto : domid -> path -> context -> unit;
  override : domid -> path -> context -> unit;
  bind : domid -> path -> context -> path -> context -> unit;
  transition : domid -> path -> context -> context -> unit;
  introduce : domid -> domid -> unit;
  stat : domid -> domid -> unit;
  release : domid -> domid -> unit;
  resume : domid -> domid -> unit;
  chown_from : domid -> domid -> unit;
  chown_to : domid -> domid -> unit;
  chown_transition : domid -> domid -> unit;
  retain_owner : domid -> domid -> bool;
  make_priv_for : domid -> domid -> unit;
  set_as_target : domid -> domid -> unit;
  set_target : domid -> domid -> unit;
  new_node_label : path -> context -> context;
  get_value_type : path -> value;
  check_domid : domid -> bool;
}

let ops = ref None

let set_security sec_ops =
  ops := Some sec_ops

let get_security () =
  !ops

let xssm_read dom_id node_path node_label =
  match !ops with
  | Some ops -> ops.read dom_id node_path node_label
  | None -> ()

let xssm_write dom_id node_path node_label =
  match !ops with
  | Some ops -> ops.write dom_id node_path node_label
  | None -> ()

let xssm_create dom_id node_path node_label =
  match !ops with
  | Some ops -> ops.create dom_id node_path node_label;
  | None -> ()

let xssm_delete dom_id node_path node_label =
  match !ops with
  | Some ops -> ops.delete dom_id node_path node_label
  | None -> ()

let xssm_chmod dom_id node_path node_label dac_perms =
  match !ops with
  | Some ops -> ops.chmod dom_id node_path node_label dac_perms
  | None -> ()

let xssm_relabelfrom dom_id node_path node_label =
  match !ops with
  | Some ops -> ops.relabelfrom dom_id node_path node_label
  | None -> ()

let xssm_relabelto dom_id node_path node_label =
  match !ops with
  | Some ops -> ops.relabelto dom_id node_path node_label
  | None -> ()

let xssm_override dom_id node_path node_label =
  match !ops with
  | Some ops -> ops.override dom_id node_path node_label
  | None -> ()

(** Node-Node Operations *)

let xssm_bind dom_id parent_path parent_label child_path child_label =
  match !ops with
  | Some ops -> ops.bind dom_id parent_path parent_label child_path child_label
  | None -> ()

let xssm_transition dom_id path old_label new_label =
  match !ops with
  | Some ops -> ops.transition dom_id path old_label new_label
  | None -> ()

(** Domain Accesses *)

let xssm_introduce sdomid tdomid =
  match !ops with
  | Some ops -> ops.introduce sdomid tdomid
  | None -> ()

let xssm_stat sdomid tdomid =
  match !ops with
  | Some ops -> ops.stat sdomid tdomid
  | None -> ()

let xssm_release sdomid tdomid =
  match !ops with
  | Some ops -> ops.release sdomid tdomid
  | None -> ()

let xssm_resume sdomid tdomid =
  match !ops with
  | Some ops -> ops.resume sdomid tdomid
  | None -> ()

let xssm_chown_from sdomid tdomid =
  match !ops with
  | Some ops -> ops.chown_from sdomid tdomid
  | None -> ()

let xssm_chown_to sdomid tdomid =
  match !ops with
  | Some ops -> ops.chown_to sdomid tdomid
  | None -> ()

let xssm_chown_transition sdomid tdomid =
  match !ops with
  | Some ops -> ops.chown_transition sdomid tdomid
  | None -> ()

let xssm_retain_owner sdomid tdomid =
  match !ops with
  | Some ops -> ops.retain_owner sdomid tdomid
  | None -> false

let xssm_make_priv_for sdomid tdomid =
  match !ops with
  | Some ops -> ops.make_priv_for sdomid tdomid
  | None -> ()

let xssm_set_as_target sdomid tdomid =
  match !ops with
  | Some ops -> ops.set_as_target sdomid tdomid
  | None -> ()

let xssm_set_target sdomid tdomid =
  match !ops with
  | Some ops -> ops.set_target sdomid tdomid
  | None -> ()

let xssm_new_node_label path parent_label =
  match !ops with
  | Some ops -> ops.new_node_label path parent_label
  | None -> String.create 0

let xssm_get_value_type path =
  match !ops with
  | Some ops -> ops.get_value_type path
  | None -> NONE

let xssm_check_domid domid =
  match !ops with
  | Some ops -> ops.check_domid domid
  | None -> true
