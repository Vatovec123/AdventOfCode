open Printf

let ime_datoteke = "Day_2/day_2.in"

let read_file filename = 
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true; do
      lines := input_line chan :: !lines
    done; !lines
  with End_of_file ->
    close_in chan;
    List.rev !lines ;;

(* Naloga 1*)

let naloga1 vsebina_datoteke = string_of_int(10)
    

let naloga2 vsebina_datoteke = string_of_int(10)
  

let _ =
  let izpisi_datoteko ime_datoteke vsebina =
  let chan = open_out ime_datoteke in
  output_string chan vsebina;
  close_out chan
  in
  let vsebina_datoteke = read_file "Day_2/day_2.in" in
  let odgovor1 = naloga1 vsebina_datoteke
  and odgovor2 = naloga2 vsebina_datoteke
  in
  izpisi_datoteko "Day_2/day_2_1.out" odgovor1;
  izpisi_datoteko "Day_2/day_2_2.out" odgovor2
