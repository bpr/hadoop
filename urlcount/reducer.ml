let input_line () = try Some(input_line stdin) with End_of_file -> None

let split_line line = 
  let len = String.length line in 
  try 
    let tab_pos = String.index line '\t' in 
    (String.sub line 0 tab_pos, String.sub line (tab_pos+1) (len-tab_pos-1))
  with Not_found -> 
    (line,"")

let main () = 
  let rec loop last_seen_key key_count = 
    match input_line () with 
      None -> Printf.printf "%s\t%d\n" last_seen_key key_count
      
    | Some line -> 
      let (k,v) = split_line line in 
      if last_seen_key <> "" && last_seen_key <> k then 
	begin
	  Printf.printf "%s\t%d\n" last_seen_key key_count;
	  loop k 1
	end
      else
	loop k (key_count + 1)
  in loop "" 0

let ()  = main ()
