(*
 * dummy.mli --- Xenstore security dummy hooks.
 *
 * Copyright (C) 2013, Galois, Inc.
 * All Rights Reserved.
 *
 * Written by Patrick Colp <pjcolp@galois.com>
 *)

(*
 * hooks.mli --- Xenstore security hooks.
 *
 * Copyright (C) 2013, Galois, Inc.
 * All Rights Reserved.
 *
 * Written by James Bielman <jamesjb@galois.com>, 8 August 2013
 *)

(** {2 Types} *)

type domid = int
(** A Xen domain ID. *)

type path = string
(** A path in the Xenstore tree. *)

type context = string
(** A security context string. *)

(** {2 Node Accesses} *)

val dummy_read : domid -> path -> context -> unit
(** Read a node, its permissions, security label, or
    list its children. *)

val dummy_write : domid -> path -> context -> unit
(** Write to a node, or create a child node. *)

val dummy_create : domid -> path -> context -> unit
(** Create a child node with the given label. *)

val dummy_delete : domid -> path -> context -> unit
(** Delete a node and all of its children. *)

val dummy_chmod : domid -> path -> context -> string -> unit
(** Change DAC permissions on a node. *)

val dummy_relabelfrom : domid -> path -> context -> unit
(** Allow objects of this type to be relabeled. *)

val dummy_relabelto : domid -> path -> context -> unit
(** Allow objects to be relabeled to this type. *)

val dummy_override : domid -> path -> context -> unit
(** Override the DAC permissions on a node. *)

(** {2 Node-Node Accesses} *)

val dummy_bind : domid -> path -> context -> path -> context -> unit
(** Create a child node with the given label under a parent node. *)

val dummy_transition : domid -> path -> context -> context -> unit
(** Explicitly change the security label of a node. *)

(** {2 Domain Accesses} *)

val dummy_introduce : domid -> domid -> unit
(** Introduce a domain to Xenstore. *)

val dummy_stat : domid -> domid -> unit
(** Query whether a domain is introduced. *)

val dummy_release : domid -> domid -> unit
(** XS_RELEASE *)

val dummy_resume : domid -> domid -> unit
(** XS_RESUME *)

val dummy_chown_from : domid -> domid -> unit
(** Change ownership of a node, where old ownership is the
    target domain. *)

val dummy_chown_to : domid -> domid -> unit
(** Change ownership of a node, where new ownership is the
    target domain. *)

val dummy_chown_transition : domid -> domid -> unit
(** Allow a node to change ownership. *)

val dummy_retain_owner : domid -> domid -> bool
(** Return true if the DAC ownership of the parent should be inherited
    when creating a new node. *)

val dummy_make_priv_for: domid -> domid -> unit
(** Set target, where device model is the target domain. *)

val dummy_set_as_target: domid -> domid -> unit
(** Set target, where the target domain is the target domain. *)

val dummy_set_target: domid -> domid -> unit
(** Allow a domain to be a target for another domain. *)

val dummy_new_node_label : path -> context -> context
(** [new_node_label path parent_context]

    Label a newly created node based on the path database
    and context of the (already existing) parent node.
    
    TODO: This interface will probably change to remove the
    path database argument, since "Xs_flask" should be able
    to have knowledge of the path database somehow. *)

val dummy_get_value_type : path -> Xssm.value
(** Return the value type of a path, return NONE for non-existant entries. *)

val dummy_check_domid : domid -> bool
(** Check if there is a VM running on the system with the given domid. *)

val dummy_operations: Xssm.xssm_operations
