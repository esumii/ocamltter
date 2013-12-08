open Spotlib.Spot
open Twitter
open Api11
open Orakuda.Regexp.Infix

module Consumer = Twitter.Auth.Consumer

let auth_file = match Exn.catch ~f:Sys.getenv "HOME" with
  | `Ok home -> home ^/ ".ocamltter_auths"
  | `Error _exn -> !!% "Env var HOME is not found@."; exit 1

let () =
  if not & File.Test._e auth_file then begin
    Ocauth.Auth.save_dummy auth_file;
    !!% "No auth table found. Created a dummy file: %s@." auth_file;
    exit 1
  end

let () = prerr_endline "getting oauth..."
let app, user = 
  Ocauth.Auth.find (Ocauth.Auth.load auth_file) ~app:"ocamltter" ~user:"seitekibot"
let o = Ocauth.Auth.oauth app user
let () = prerr_endline "oauth done"

let is_ocaml_misspell =
(*
  fun _ -> true
*)
  let rex = <:m<静的型言語|静的型つけ言語|静的型付け言語>> in
  let rec loop text = 
    begin match text =~ rex with
    | None -> false
    | Some res ->
        begin match res#_0 with
        | "静的型言語" | "静的型つけ言語" | "静的型付け言語" -> 
            (* String.length (<:s<\s//g>> text) > 5 *)
	    true
        | _ -> loop res#_right
	end
    end
  in
  loop 

let do_ocaml_misspell tw =
  let text = tw#text in
  if is_ocaml_misspell text then begin
    !!% "%Ld: %s@." tw#id text;
(*
    begin match Tweets.show o tw#id with
    | `Ok tw' ->
        !!% "%Ld: %s@." tw'#id tw'#text;
        assert (tw#id = tw'#id);
        assert (tw#text = tw'#text)
    | `Error e ->
        !!% "ERROR: @[%a@]@." Api11.Error.format_error e
    end;
*)
    match Favorites.create o tw#id with
    | `Ok _ -> !!% "OK@."
    | `Error e ->
        !!% "ERROR: @[%a@]@." Api11.Error.format_error e
  end else 
    !!% "XXX: %Ld: %s@." tw#id text

let rec loop since_id = 
  match Search.tweets o ~count:100 ?since_id
      "\"静的型付け言語\" OR \"静的型つけ言語\" OR \"静的型言語\"" with
  | `Error (`Http _) -> 
      prerr_endline "HTTP";
      Unix.sleep 60;
      loop since_id
  | `Error (`Json_parse _) -> 
      prerr_endline "JSON PARSE";
      Unix.sleep 60;
      loop since_id
  | `Error (`Json e) -> 
      Format.eprintf "Json_conv: %a@." Json_conv.format_full_error e;
      Unix.sleep 60;
      loop since_id
  | `Ok res -> 
      match res#statuses with
      | [] -> 
          Format.eprintf "no updates. scheduled@.";
          Unix.sleep 60;
          loop since_id
      | ts -> 
          let last_id = 
            ts |> List.fold_left (fun id tw ->
              begin match tw#retweeted_status with
              | Some _ -> ()
              | None -> do_ocaml_misspell tw
              end;
              max id tw#id) (Option.default since_id (fun () -> 0L))
          in
          Format.eprintf "scheduled from %Ld@." last_id;
          Unix.sleep 10;
          loop (Some last_id)

let () = loop None
