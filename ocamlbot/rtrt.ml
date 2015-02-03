open Spotlib.Spot
open OCamltter_oauth
open OCamltter_twitter
open Api11
open Ppx_orakuda.Regexp.Infix

(* begin: copied from bot.ml *)
let auth_file = match Exn.catch Sys.getenv "HOME" with
| `Ok home -> home ^/ ".ocamlrtrt_auths"
| `Error _exn -> !!% "Env var HOME is not found@."; exit 1

let () =
  if not & File.Test._e auth_file then begin
    Ocauth.Auth.save auth_file Ocauth.Auth.dummy;
    !!% "No auth table found. Created a dummy file: %s@." auth_file;
    exit 1
  end

let () = prerr_endline "getting oauth..."

let auths = Ocauth.Auth.load auth_file

let { Ocauth.Auth.consumer } = match Ocauth.Auth.find_app auths "ocamlrtrt" with
  | Some app -> app
  | None -> failwith "no ocamlrtrt app found"

module Oauthx = Oauth_ex.Make(struct
  include OCamltter_twitter.Conf
  let app = consumer
end)

let o : Oauth.t =
  match Ocauth.Auth.find_user auths ~app:"ocamlrtrt" ~user:"esumii" with
  | `Found o -> o
  | `NoApp -> assert false
  | `NoUser _ ->
      let _res, acc_token = Oauthx.authorize_cli_interactive () in
      Ocauth.Auth.add_user auths ~app:"ocamlrtrt" ~user:"esumii" acc_token;
      Ocauth.Auth.save auth_file auths;
      Oauthx.oauth Oauthx.Conf.app acc_token
    
let () = prerr_endline "oauth done"
(* end: copied from bot.ml *)

let unOk = function `Ok x -> x
  | _ -> failwith "Error"
(* let m = Timelines.mentions_timeline o *)
let r = Timelines.retweets_of_me ~trim_user:true ~include_entities:false ~include_user_entities:false o
let () = Format.eprintf "[DEBUG] got %d retweets of user@." (List.length (unOk r)) (* "got mentions and retweets" *)

let foreach l f = List.iter f l
let () =
  foreach (unOk r) (fun t1 ->
    Format.eprintf "[DEBUG] t1#id = %Ld@." t1#id;
(*
    let lim = Help.rate_limit_status o in
    Format.eprintf "[DEBUG] got limit@.";
    Format.eprintf "[DEBUG] lim#resources#application#rate_limit_status = %a@." (Ocaml.format_with Api_intf.Rate_limit_status.ocaml_of_limit) (unOk lim)#resources#application#rate_limit_status;
    Format.eprintf "[DEBUG] lim#resources#statuses#retweets = %a@." (Ocaml.format_with Api_intf.Rate_limit_status.ocaml_of_limit) (unOk lim)#resources#statuses#retweets;
    Format.eprintf "[DEBUG] lim#resources#statuses#user_timeline = %a@." (Ocaml.format_with Api_intf.Rate_limit_status.ocaml_of_limit) (unOk lim)#resources#statuses#user_timeline;
*)
    let rts = Tweets.retweets o t1#id in
    Format.eprintf "[DEBUG] got %d retweets of tweet@." (List.length (unOk rts));
    foreach (unOk rts) (fun t2 ->
      Format.eprintf "[DEBUG] t2#id = %Ld@." t2#id;
      (match t2#user#details with None -> () | Some d ->
	Format.printf "*** %s@." d#screen_name;
	let after = Timelines.user_timeline ~count:3 ~since_id:t2#id ~user_id:t2#user#id (* ~screen_name:d#screen_name *) ~include_rts:true ~exclude_replies:false ~trim_user:true ~contributor_details:false o in
	Format.eprintf "[DEBUG] got after@.";
	let before = Timelines.user_timeline ~count:3 ~max_id:t2#id ~user_id:t2#user#id (* ~screen_name:d#screen_name *) ~include_rts:true ~exclude_replies:false ~trim_user:true ~contributor_details:false o in
	Format.eprintf "[DEBUG] got before@.";
(*
        (match t2#user#details with
        | None -> Format.printf "*** #%Ld@." t2#user#id
        | Some d -> Format.printf "*** %s@." d#screen_name);
*)
	foreach (List.rev (unOk before) @ List.rev (unOk after)) (fun t3 ->
	  Format.eprintf "[DEBUG] t3#id = %Ld@." t3#id;
	  Format.printf "%s@." t3#text))))
