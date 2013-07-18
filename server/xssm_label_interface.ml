include Namespace.Unsupported

let ( |> ) a b = b a

let get_node store path =
  let root = store.Store.root in
  let ent = Store.lookup root path in
  match ent with
  | Some node -> node
  | _ -> raise Not_found

let read t accesser perms path =
  Store.getlabel t.Transaction.store accesser perms path

let exists t accesser perms path =
  try
    ignore(read t accesser perms path);
    true
  with
  | Store.Path.Doesnt_exist _ -> false

let write t creator perms path label =
  Store.setlabel t.Transaction.store creator perms path label

let list t accesser perm path =  
  Store.ls t.Transaction.store accesser perm path
