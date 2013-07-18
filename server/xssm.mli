(*
 * Copyright (C) 2013, Galois, Inc.
 * All Rights Reserved.
 *
 * Written by Patrick Colp <pjcolp@galois.com>
 *)

type domid = int
(** A Xen domain ID. *)

type path = string
(** A path in the Xenstore tree. *)

type context = string
(** A security context string. *)

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

val set_security : xssm_operations -> unit

val get_security : unit -> xssm_operations option

val xssm_read : domid -> path -> context -> unit
(** [xssm_read ops dom_id node_path node_label]

    Call the XSSM read operation in ops. *)

val xssm_write : domid -> path -> context -> unit
(** [xssm_write ops dom_id node_path node_label]

    Call the XSSM write operation in ops. *)

val xssm_create : domid -> path -> context -> unit
(** Create a child node with the given label. *)

val xssm_delete : domid -> path -> context -> unit
(** Delete a node and all of its children. *)

val xssm_chmod : domid -> path -> context -> string -> unit
(** Change DAC permissions on a node. *)

val xssm_relabelfrom : domid -> path -> context -> unit
(** Allow objects of this type to be relabeled. *)

val xssm_relabelto : domid -> path -> context -> unit
(** Allow objects to be relabeled to this type. *)

val xssm_override : domid -> path -> context -> unit
(** Override the DAC permissions on a node. *)

(** {2 Node-Node Accesses} *)

val xssm_bind : domid -> path -> context -> path -> context -> unit
(** Create a child node with the given label under a parent node. *)

val xssm_transition : domid -> path -> context -> context -> unit
(** Explicitly change the security label of a node. *)

(** {2 Domain Accesses} *)

val xssm_introduce : domid -> domid -> unit
(** Introduce a domain to Xenstore. *)

val xssm_stat : domid -> domid -> unit
(** Query whether a domain is introduced. *)

val xssm_release : domid -> domid -> unit
(** XS_RELEASE *)

val xssm_resume : domid -> domid -> unit
(** XS_RESUME *)

val xssm_chown_from : domid -> domid -> unit
(** Change ownership of a node, where old ownership is the
    target domain. *)

val xssm_chown_to : domid -> domid -> unit
(** Change ownership of a node, where new ownership is the
    target domain. *)

val xssm_chown_transition : domid -> domid -> unit
(** Allow a node to change ownership. *)

val xssm_retain_owner : domid -> domid -> bool
(** Return true if the DAC ownership of the parent should be inherited
    when creating a new node. *)

val xssm_make_priv_for: domid -> domid -> unit
(** Set target, where device model is the target domain. *)

val xssm_set_as_target: domid -> domid -> unit
(** Set target, where the target domain is the target domain. *)

val xssm_set_target: domid -> domid -> unit
(** Allow a domain to be a target for another domain. *)

val xssm_new_node_label : path -> context -> context
(** [new_node_label path parent_context]

    Label a newly created node based on the path database
    and context of the (already existing) parent node.
    
    TODO: This interface will probably change to remove the
    path database argument, since "Xs_flask" should be able
    to have knowledge of the path database somehow. *)

val xssm_get_value_type : path -> value
(** Return the value type of a path, return NONE for non-existant entries. *)

val xssm_check_domid : domid -> bool
(** Check if there is a VM running on the system with the given domid. *)
