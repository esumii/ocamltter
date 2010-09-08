exception Http_error of string
val url_encode : string -> string
type header = { code : string; fields : (string, string) Hashtbl.t; }
type params = (string * string) list
type meth = GET | POST
val params2string : (string * string) list -> string
val read_header : in_channel -> header
val read_all_and_count : in_channel -> string * int
val conn :
  ?port:int ->
  string ->
  meth ->
  ?headers:(string * string) list ->
  ?user:string ->
  ?pass:string ->
  string -> (string * string) list -> (header -> in_channel -> 'a) -> 'a
