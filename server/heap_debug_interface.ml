include Namespace.Unsupported

let debug fmt = Logging.debug "memory_interface" fmt

let ( |> ) a b = b a

let read t accesser (perms: Perms.t) (path: Store.Path.t) =
	Perms.has perms Perms.CONFIGURE;
	match Store.Path.to_string_list path with
	| [] -> ""
	| "heap_words" :: [] -> string_of_int (Gc.stat ()).Gc.heap_words
	| "live_words" :: [] -> string_of_int (Gc.stat ()).Gc.live_words
	| "free_words" :: [] -> string_of_int (Gc.stat ()).Gc.free_words
	| "symbols"    :: [] -> string_of_int (Symbol.stats ())
	| _ -> Store.Path.doesnt_exist path

let exists t accesser perms path = try ignore(read t accesser perms path); true with Store.Path.Doesnt_exist _ -> false

let list t _ perms path =
	Perms.has perms Perms.CONFIGURE;
	match Store.Path.to_string_list path with
	| [] -> [ "heap_words"; "live_words"; "free_words"; "symbols" ]
	| _ -> []


