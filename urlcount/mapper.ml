let input_file  = ref None

let set_input_file file = begin input_file := Some (file) end

let scheme = "http(s)?"
let colon = ":"
let alpha = "[a-zA-Z]"
let alphanum = "[a-zA-Z0-9]"
let id = "[a-zA-Z0-9\\.\\?\\+=&_-]"
let repeat s = "(" ^ s ^ ")+" (* Should I call this 'repeat1'? *)
let star s = "(" ^ s ^ ")*" (* Is 'repeat0' better? *)
let opt s = "(" ^ s ^ ")?" (* Is 'optional' better? *)
let choice ss = "(" ^ (String.concat "|" ss) ^ ")"
let slash = "/"
let slash2 = "//"
let vbar = "|"
let identifier = repeat id

(* Read string concatenator '^' as 'folowed by' *)

let url_pat = 
  scheme ^ colon ^ slash2 ^ identifier ^ (star (slash ^ identifier)) ^ (opt slash)

let url_regexp = Pcre.regexp ~flags:[`CASELESS] url_pat 

let print_urls_in_file ic = 
  let line_number_ref = ref 0 in 
  let print_urls_in_string line = 
    let _ = incr line_number_ref in 
    try 
      let matches = Pcre.extract_all ~rex:url_regexp line in 
      begin
	(* Printf.printf "line[%3d]\n" !line_number_ref; *)
	Array.iter (fun a -> Printf.printf "%s\t1\n" (Array.get a 0)) matches
      end
    with Not_found -> ()
  in
  begin
    Pcre.foreach_line ~ic:ic print_urls_in_string;
  end

let main () = 
  let options = 
    [("-input_file", Arg.String set_input_file, "input text file with URLs")] in 
  let files : string list ref = ref [] in 
  let add_file s = files := s :: !files in
  let usage = Printf.sprintf "Usage: %s <options> <files>" Sys.argv.(0) in 
  begin
    Arg.parse options add_file usage;
    match !input_file with 
      Some s -> 
	let ic = Pervasives.open_in s in 
	begin
	  Printf.printf "Main.main: input_file=%s\n" s;
	  print_urls_in_file ic;
	  close_in ic;
	end
    | None -> (* List.iter (fun s -> print_urls_in_file s) !files *)
	print_urls_in_file stdin
  end

let () = main ()
