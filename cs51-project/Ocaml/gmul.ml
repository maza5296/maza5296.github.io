let gadd a b = 
  !a lxor !b
;;
let gmul (a:int ref) (b:int ref)= 
  let p = ref 0 in 
  for i=0 to 8 do 
    (
      if (!b land 1 = 1) then p:=(!p lxor !a);
      a := !a lsl 1;
      if (!a land 0x100 = 1) (* detect if x^8 term is generated *)
      then a := (!a lxor 0x11b); (* XOR with x^8 + x^4 + x^3 + x + 1 *)
      b := !b lsr 1;
    ) done;
  p
;;
let gmod (a:int) (b:int) = 
  let numer = toBin a in
  let denom = toBin b in
  failwith "unimplemented"
;;


let toHex = (fun num -> "0X" ^Printf.sprintf "%x" num)
;;
let toBin (i:int) : string =
  let rec strip_bits i s =
    match i with
    | 0 -> s
    | _ -> strip_bits (i lsr 1) ((string_of_int (i land 0x01)) ^ s) 
  in
  strip_bits i ""
;;

let a = ref 0x00;;
let b = ref 14;;
let prod = !(gmul a b);;
toBin prod;;
let modded = toBin (prod lxor 0b100011011);;
