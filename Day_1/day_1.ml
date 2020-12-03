open Printf

let ime_datoteke = "Day_1/day_1.in"

let read_file filename = 
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true; do
      lines := int_of_string (input_line chan) :: !lines
    done; !lines
  with End_of_file ->
    close_in chan;
    List.rev !lines ;;


(* Naloga 1*)

(* Preverimo ali je vsota prvega elementa in kateregakoli drugega elementa iz seznama enaka s. Če je vrnemo produkt *)
let rec preveri_vsoto_vrne_produkt seznam s=
  match seznam with
  | [] -> None
  | _ :: [] -> None
  | x :: y :: ys ->  if x + y = s then Some( x * y) else preveri_vsoto_vrne_produkt ( x :: ys) s


let rec preveri seznam = 
    match seznam with
    | [] -> None
    | x :: xs -> if preveri_vsoto_vrne_produkt seznam 2020 != None then preveri_vsoto_vrne_produkt seznam 2020 else preveri xs
 
(* Naloga 2*)

(*Preverimo ali je vsota x-a, y-a in pa elementa z iz seznama 2020. Če je vrnemo produkt teh treh števil, sicer pa preverimo za preostanek seznama.*)
let rec preveri_vsoto_x_y_z_vrne_prodekt x y seznam =
  match seznam with
  | [] -> None
  | z :: zs -> if x + y + z = 2020 then Some( x * y * z) else preveri_vsoto_x_y_z_vrne_prodekt x y zs

(* Rekurzivna funkcija, kjer se sklicujemo na zgornjo funkcijo*)
let rec preveri2 x seznam =
  match seznam with
  | [] -> None
  | y :: ys -> if preveri_vsoto_x_y_z_vrne_prodekt x y ys != None then preveri_vsoto_x_y_z_vrne_prodekt x y ys else preveri2 x ys 

(*Rekurzivna funkcija, kjer se sklicujemo na zgornjo funkcijo*)
let rec preveri1 seznam =
  match seznam with
  | [] -> None
  | x :: xs -> if preveri2 x xs != None then preveri2 x xs else preveri1 xs

let naloga1 vsebina_datoteke =
    match preveri (vsebina_datoteke) with
    | None -> "V seznamu ni dveh števil, ki se seštejeta v 2020."
    | Some x -> string_of_int x


let naloga2 vsebina_datoteke =
  match preveri1 (vsebina_datoteke) with
  | None -> "V seznamu ni treh števil, katerih vsota je 2020"
  | Some x -> string_of_int x


let _ =
  let izpisi_datoteko ime_datoteke vsebina =
  let chan = open_out ime_datoteke in
  output_string chan vsebina;
  close_out chan
  in
  let vsebina_datoteke = read_file "Day_1/day_1.in" in
  let odgovor1 = naloga1 vsebina_datoteke
  and odgovor2 = naloga2 vsebina_datoteke
  in
  izpisi_datoteko "Day_1/day_1_1.out" odgovor1;
  izpisi_datoteko "Day_1/day_1_2.out" odgovor2

