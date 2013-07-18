(*
 * dummy.ml --- Xenstore security dummy hooks.
 *
 * Copyright (C) 2013, Galois, Inc.
 * All Rights Reserved.
 *
 * Written by Patrick Colp <pjcolp@galois.com>
 *)

open Xssm

type domid = int
type path = string
type context = string

let dummy_print str =
  let print = false in
  if print then Printf.printf "%s\n%!" str

let dummy_read dom_id node_path node_label =
  dummy_print (Printf.sprintf "XS_SERVER: DUMMY: READ: dom_id = %d; node_path = '%s'; node_label = '%s'" dom_id node_path node_label)

let dummy_write dom_id node_path node_label =
  dummy_print (Printf.sprintf "XS_SERVER: DUMMY: WRITE: dom_id = %d; node_path = '%s'; node_label = '%s'" dom_id node_path node_label)

let dummy_create dom_id node_path node_label =
  dummy_print (Printf.sprintf "XS_SERVER: DUMMY: CREATE: dom_id = %d; node_path = '%s'; node_label = '%s'" dom_id node_path node_label)

let dummy_delete dom_id node_path node_label =
  dummy_print (Printf.sprintf "XS_SERVER: DUMMY: DELETE: dom_id = %d; node_path = '%s'; node_label = '%s'" dom_id node_path node_label)

let dummy_chmod dom_id node_path node_label dac_perms =
  dummy_print (Printf.sprintf "XS_SERVER: DUMMY: CHMOD: dom_id = %d; node_path = '%s'; node_label = '%s'; dac_perms = '%s'" dom_id node_path node_label dac_perms)

let dummy_relabelfrom dom_id node_path node_label =
  dummy_print (Printf.sprintf "XS_SERVER: DUMMY: RELABELFROM: dom_id = %d; node_path = '%s'; node_label = '%s'" dom_id node_path node_label)

let dummy_relabelto dom_id node_path node_label =
  dummy_print (Printf.sprintf "XS_SERVER: DUMMY: RELABELTO: dom_id = %d; node_path = '%s'; node_label = '%s'" dom_id node_path node_label)

let dummy_override dom_id node_path node_label =
  dummy_print (Printf.sprintf "XS_SERVER: DUMMY: OVERRIDE: dom_id = %d; node_path = '%s'; node_label = '%s'" dom_id node_path node_label)

(** Node-Node Operations *)

let dummy_bind dom_id parent_path parent_label child_path child_label =
  dummy_print (Printf.sprintf "XS_SERVER: DUMMY: BIND: dom_id = %d; parent_path = '%s'; parent_label = '%s'; child_path = '%s'; child_label = '%s'" dom_id parent_path parent_label child_path child_label)

let dummy_transition dom_id path old_label new_label =
  dummy_print (Printf.sprintf "XS_SERVER: DUMMY: TRANSITION: dom_id = %d; path = '%s'; old_label = '%s'; new_label = '%s'" dom_id path old_label new_label)

(** Domain Accesses *)

let dummy_introduce sdomid tdomid =
  dummy_print (Printf.sprintf "XS_SERVER: DUMMY: INTRODUCE: sdomid = %d; tdomid = %d" sdomid tdomid)

let dummy_stat sdomid tdomid =
  dummy_print (Printf.sprintf "XS_SERVER: DUMMY: STAT: sdomid = %d; tdomid = %d" sdomid tdomid)

let dummy_release sdomid tdomid =
  dummy_print (Printf.sprintf "XS_SERVER: DUMMY: RELEASE: sdomid = %d; tdomid = %d" sdomid tdomid)

let dummy_resume sdomid tdomid =
  dummy_print (Printf.sprintf "XS_SERVER: DUMMY: RESUME: sdomid = %d; tdomid = %d" sdomid tdomid)

let dummy_chown_from sdomid tdomid =
  dummy_print (Printf.sprintf "XS_SERVER: DUMMY: CHOWN_FROM: sdomid = %d; tdomid = %d" sdomid tdomid)

let dummy_chown_to sdomid tdomid =
  dummy_print (Printf.sprintf "XS_SERVER: DUMMY: CHOWN_TO: sdomid = %d; tdomid = %d" sdomid tdomid)

let dummy_chown_transition sdomid tdomid =
  dummy_print (Printf.sprintf "XS_SERVER: DUMMY: CHOWN_TRANSITION: sdomid = %d; tdomid = %d" sdomid tdomid)

let dummy_retain_owner sdomid tdomid =
  dummy_print (Printf.sprintf "XS_SERVER: DUMMY: RETAIN_OWNER: sdomid = %d; tdomid = %d" sdomid tdomid);
  false

let dummy_make_priv_for sdomid tdomid =
  dummy_print (Printf.sprintf "XS_SERVER: DUMMY: MAKE_PRIV_FOR: sdomid = %d; tdomid = %d" sdomid tdomid)

let dummy_set_as_target sdomid tdomid =
  dummy_print (Printf.sprintf "XS_SERVER: DUMMY: SET_AS_TARGET: sdomid = %d; tdomid = %d" sdomid tdomid)

let dummy_set_target sdomid tdomid =
  dummy_print (Printf.sprintf "XS_SERVER: DUMMY: SET_TARGET: sdomid = %d; tdomid = %d" sdomid tdomid)

let dummy_new_node_label path parent_label =
  dummy_print (Printf.sprintf "XS_SERVER: DUMMY: NEW_NODE_LABEL: path = '%s'; parent_label = '%s'" path parent_label);
  String.create 0

let dummy_get_value_type path =
  dummy_print (Printf.sprintf "XS_SERVER: DUMMY: VALUE_TYPE: path = '%s'" path);
  Xssm.NONE

let dummy_check_domid domid =
  dummy_print (Printf.sprintf "XS_SERVER: DUMMY: CHECK_DOMID: domid = '%d'" domid);
  true

(** XSSM Operations Record *)
let dummy_operations: xssm_operations = {
  read = dummy_read;
  write = dummy_write;
  create = dummy_create;
  delete = dummy_delete;
  chmod = dummy_chmod;
  relabelfrom = dummy_relabelfrom;
  relabelto = dummy_relabelto;
  override = dummy_override;
  bind = dummy_bind;
  transition = dummy_transition;
  introduce = dummy_introduce;
  stat = dummy_stat;
  release = dummy_release;
  resume = dummy_resume;
  chown_from = dummy_chown_from;
  chown_to = dummy_chown_to;
  chown_transition = dummy_chown_transition;
  retain_owner = dummy_retain_owner;
  make_priv_for = dummy_make_priv_for;
  set_as_target = dummy_set_as_target;
  set_target = dummy_set_target;
  new_node_label = dummy_new_node_label;
  get_value_type = dummy_get_value_type;
  check_domid = dummy_check_domid;
}
