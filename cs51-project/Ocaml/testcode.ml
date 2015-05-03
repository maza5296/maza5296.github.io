String.iter;;

String.map;;


type bit = Zero | One

type byte_t = bit array

type block = byte_t array

type state = byte_t array 
(* a word is 4 bytes *)
type word = bit array


let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) [];;
explode "hello";;

let rlist = "aksdf234ag6s457dfv5l79aeyv86k9jhasdv235kdas76vkf";;

explode rlist;;

let asciiList = List.map int_of_char (explode rlist);;

let int_to_bin (i:int) : string =
  let rec strip_bits i s =
    match i with
    | 0 -> s
    | _ -> strip_bits (i lsr 1) ((string_of_int (i land 0x01)) ^ s) 
  in
  (*add leading zeroes - my addition to the function *)
  let rec add_zeroes (s:string) : string = 
   if String.length s = 8 then s 
   else add_zeroes ("0" ^ s)
  in 
  add_zeroes(strip_bits i "");;

int_to_bin 5;;

let y = List.map int_to_bin asciiList;;

int_to_bin (0b01101011 lxor 0b00110101);;

let add_0b list = List.map (fun x -> "0b" ^ x) list;;
add_0b y;;

let get_state_strlist (s:string) =
add_0b (List.map int_to_bin (List.map int_of_char (explode s)))
;;

let string_to_encrypt = "HelloWorld123456";;
let t = get_state_strlist string_to_encrypt;;
let x = Array.of_list t;;
x.(1);;

open Array
let x11 () = create 4 (create 4 "");;
x11();;
let x2 = copy (x11());;
x2;;
(*
x2.(0).(0) <- "abc";;
x2.(0).(0);;
*)

let update x2 x =
x2.(0).(0) <- x.(0);
x2.(0).(1) <- x.(1);
x2.(0).(2) <- x.(2);
x2.(0).(3) <- x.(3);
x2.(1).(0) <- x.(4);
x2.(1).(1) <- x.(5);
x2.(1).(2) <- x.(6);
x2.(1).(3) <- x.(7);
x2.(2).(0) <- x.(8);
x2.(2).(1) <- x.(9);
x2.(2).(2) <- x.(10);
x2.(2).(3) <- x.(11);
x2.(3).(0) <- x.(12);
x2.(3).(1) <- x.(13);
x2.(3).(2) <- x.(14);
x2.(3).(3) <- x.(15);
x2
;;
update x2 x;;
x2;;


let sbox : string array array = 
    [| 
      [|"63";"7c";"77";"7b";"f2";"6b";"6f";"c5";"30";"01";"67";"2b";"fe";"d7";"ab";"76"|];
      [|"ca";"82";"c9";"7d";"fa";"59";"47";"f0";"ad";"d4";"a2";"af";"9c";"a4";"72";"c0"|];
      [|"b7";"fd";"93";"26";"36";"3f";"f7";"cc";"34";"a5";"e5";"f1";"71";"d8";"31";"15"|];
      [|"04";"c7";"23";"c3";"18";"96";"05";"9a";"07";"12";"80";"e2";"eb";"27";"b2";"75"|];
      [|"09";"83";"2c";"1a";"1b";"6e";"5a";"a0";"52";"3b";"d6";"b3";"29";"e3";"2f";"84"|];
      [|"53";"d1";"00";"ed";"20";"fc";"b1";"5b";"6a";"cb";"be";"39";"4a";"4c";"58";"cf"|];
      [|"d0";"ef";"aa";"fb";"43";"4d";"33";"85";"45";"f9";"02";"7f";"50";"3c";"9f";"a8"|];
      [|"51";"a3";"40";"8f";"92";"9d";"38";"f5";"bc";"b6";"da";"21";"10";"ff";"f3";"d2"|];
      [|"cd";"0c";"13";"ec";"5f";"97";"44";"17";"c4";"a7";"7e";"3d";"64";"5d";"19";"73"|];
      [|"60";"81";"4f";"dc";"22";"2a";"90";"88";"46";"ee";"b8";"14";"de";"5e";"0b";"db"|];
      [|"e0";"32";"3a";"0a";"49";"06";"24";"5c";"c2";"d3";"ac";"62";"91";"95";"e4";"79"|];
      [|"e7";"c8";"37";"6d";"8d";"d5";"4e";"a9";"6c";"56";"f4";"ea";"65";"7a";"ae";"08"|];
      [|"ba";"78";"25";"2e";"1c";"a6";"b4";"c6";"e8";"dd";"74";"1f";"4b";"bd";"8b";"8a"|];
      [|"70";"3e";"b5";"66";"48";"03";"f6";"0e";"61";"35";"57";"b9";"86";"c1";"1d";"9e"|];
      [|"e1";"f8";"98";"11";"69";"d9";"8e";"94";"9b";"1e";"87";"e9";"ce";"55";"28";"df"|];
      [|"8c";"a1";"89";"0d";"bf";"e6";"42";"68";"41";"99";"2d";"0f";"b0";"54";"bb";"16"|]
    |]
;;




sprintf "%x" (int_of_string "0b11111111");;


let string_to_encrypt = "Juaquin7364859hg";;
let duplicate = copy (get_state string_to_encrypt);;


let hex = duplicate.(0).(0) in
let z = hex.[2] in
let g = hex.[3] in 
match (z, g) with
|(‘0’..’9’,’0’..’9’) -> (duplicate.(a).(b) <-  sbox.(z).(g))
|(’0’..’9’,’A’..’F’) -> (match g with
  |’A’ -> (duplicate.(a).(b) <- sbox.(z).(10) )
  |’B’ -> (duplicate.(a).(b) <- sbox.(z).(11) )
  |’C’ -> (duplicate.(a).(b) <- sbox.(z).(12) )
  |’D’ -> (duplicate.(a).(b) <- sbox.(z).(13) )
  |’E’ -> (duplicate.(a).(b) <- sbox.(z).(14) )
  |’F’ -> (duplicate.(a).(b) <- sbox.(z).(15) ))
|(‘A’..’F’, _) -> (match (z,g) with
  |(_, ‘0’..’9’) -> (match z with 
    |’A’ -> (duplicate.(a).(b) <- sbox.(10).(g))
    |’B’ -> (duplicate.(a).(b) <- sbox.(11).(g))
    |’C’ -> (duplicate.(a).(b) <- sbox.(12).(g))
    |’D’ -> (duplicate.(a).(b) <- sbox.(13).(g))
    |’E’ -> (duplicate.(a).(b) <- sbox.(14).(g))
    |’F’ -> (duplicate.(a).(b) <- sbox.(15).(g)))
  |(_, _) -> failwith "NotImplemented")
;;
