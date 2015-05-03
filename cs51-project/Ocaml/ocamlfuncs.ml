(* Expand Key *)

open Array

type bit = Zero | One ;;

type byte_t = bit array ;; 

type block = byte_t array;;

(* I don't really use state, it's like a word though *)
type state = byte_t array ;;

(* a word is 4 bytes *)
type word = byte_t array;;
(*(* first we need our key in the form of 128 bits, or a 128 bit array *)
module type KeyExpand = 
struct *)

module type KEYEXPAND = 
  sig 
  val int_to_bin:  int -> string

  val hex_to_int: string -> int

  val int_to_byte: int -> byte_t 

  val hex_to_byte: string -> byte_t

  val hex_to_words: string -> byte_t array

  val init_key_to_words: byte_t array -> word array

  val rot_word: word -> word

  val sbox: string array array

  val get_sub_value: string -> string

  val byte_to_bin: byte_t -> string

  val bin_to_hex: string -> string

  val byte_to_hex: byte_t -> string

  val sub_word: word -> word

  val byte_to_int: byte_t -> int

  val xor: word -> word -> word

  val round_constant: int -> word

  val word_to_hex: word -> string

  val exp_key_offset: word array -> int -> word

  (* actually expands the key *)
  val expanded_key: string -> word array

  (* print all the words in the key to test/check *)
  val print_loop : int -> unit
end

module KeyExpand : KEYEXPAND = 
struct 

(* from http://pleac.sourceforge.net/pleac_ocaml/numbers.html
 * this returns a binary string of a decimal, but doesn't prepend
 * leading zeroes, so I add them *)
let int_to_bin (i:int) : string =
  if i > 255 || i < 0 then (failwith "number out of range!") else
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

(*testing int_to_bin*)
(* let _ = 
   assert(int_to_bin 1 = "00000001");
   assert(int_to_bin 0 = "00000000");
   assert(int_to_bin 9 = "00001001");
   assert(int_to_bin 255 = "11111111");
   assert(int_to_bin 150 = "10010110")
 ;; *)

  (* assume our key is passed in in hex, we need to convert to binary, then
   * convert binary to bit array *)
  let hex_to_int (str:string) : int = 
    Scanf.sscanf str "%x" (fun x -> x);;

  (* testing hex_to_int *)
  (* let _ = 
    assert(hex_to_int "FF" = 255);
    assert(hex_to_int "0F" = 15);
    assert(hex_to_int "01" = 1);
    assert(hex_to_int "15" = 21);
    assert(hex_to_int "00" = 0)
  ;;*)

  (* convert to bytes by first converting to a binary string, then matching
   * each character '0' or '1' with the proper bit *)
  let int_to_byte (n:int) : byte_t = 
    let bit_string : string = int_to_bin n in
    let init_array : bit array = [|Zero;Zero;Zero;Zero;Zero;Zero;Zero;Zero;|] in
    let rec convert (n:int) (str:string) : unit =
      if n < 0 then () else 
      let c = String.get bit_string n in 
      (match c with 
      | '0' -> init_array.(n) <- Zero
      | '1' -> init_array.(n) <- One  
      | _ -> failwith "the converter failed!"
      ); 
      convert (n-1) str
    in 
    (* thanks to the invariant we create in int_to_bin we know stringlen is 8
     * so pass in 7 to convert *)
    convert 7 bit_string; 
    init_array
   ;;

  (* let _ =
    assert(int_to_byte 1 = [|Zero;Zero;Zero;Zero;Zero;Zero;Zero;One|]);
    assert(int_to_byte 2 = [|Zero;Zero;Zero;Zero;Zero;Zero;One;Zero|]);
    assert(int_to_byte 3 = [|Zero;Zero;Zero;Zero;Zero;Zero;One;One|]);
    assert(int_to_byte 255 = [|One;One;One;One;One;One;One;One|]);
    assert(int_to_byte 0 = [|Zero;Zero;Zero;Zero;Zero;Zero;Zero;Zero|])
  ;;*)


  (* with our helpers, we can make this next one easy. It presumes/invariant is
   * that you pass in only a valid hexadecimal pair, so it's hidden in
   * the abstraction barrier *)
  let hex_to_byte (str:string) : byte_t = 
    int_to_byte (hex_to_int str);;

  (* let _ = 
    assert(hex_to_byte "FF" = [|One;One;One;One;One;One;One;One|]);
    assert(hex_to_byte "00" = [|Zero;Zero;Zero;Zero;Zero;Zero;Zero;Zero|]);
    assert(hex_to_byte "03" = [|Zero;Zero;Zero;Zero;Zero;Zero;One;One|])
  ;; *)

  (*takes in a string of multiples of 4 bytes in hex form,
   * converts to an array of arrays of 4bytes *)
  let hex_to_words (s:string) : byte_t array = 
    (* for 128-bit key *)
    (*if String.length str <> 32 then failwith "invalid key!"*) 
    (*initialize an array *)
    let key_array : byte_t array  =
      Array.init ((String.length s)/2) (*~f:*)(fun i -> int_to_byte i)
    in 
    let rec convert (n:int) (str:string) : unit =
      if (n*2) > (String.length str - 2) then () else 
      let hex = String.sub str (n*2) 2 in 
      key_array.(n) <- (hex_to_byte hex); 
      convert (n+1) str
    in 
    convert 0 s;
    key_array
  ;;

  (* this will be tested later using the sample input from the .gov
   * specification, so we create a key from the sample here *)
  let test_key = hex_to_words "2b7e151628aed2a6abf7158809cf4f3c";;

  (* this stores our initial key in words, for 128-bit keys, 4 words
   * are created, or 128/32 (since 32 bits is the size of each word *)
  let init_key_to_words (key: byte_t array) : word array = 
    let size = (Array.length key) in 
    if (size mod 4 = 0) then 
    (let num_init_words = size/4 in 
    (* create an array of words, since each word is 4 bytes *)
    let init_word_array = 
       Array.init num_init_words 
         (*~f:*)(fun _ -> (Array.init 4 (*~f:*)(fun k -> int_to_byte k) ) ) 
    in 
    let rec create (n:int) (k:byte_t array) : unit = 
       if n >= (num_init_words) then () 
       else 
       (init_word_array.(n) <- Array.sub k (n*4) 4;
       create (n+1) k)
    in 
    create 0 key; 
    init_word_array)
    else failwith "key length wrong!"
   ;;

  let first_four = init_key_to_words test_key;;
  (* let _ = 
     assert (first_four.(0).(0) = test_key.(0));
     assert (first_four.(0).(1) = test_key.(1));
     assert (first_four.(3).(2) = test_key.(14))
  ;; *)

  (* Rotate so [b1,b2,b3,b4] becomes [b2,b3,b4,b1] as per the spec.
   * Returns a new byte_t array rather than modifying the old one *)
  let rot_word (wrd: word) : word = 
    let temp = Array.init 4 (*~f:*)(fun i -> int_to_byte i) in
    let _ = 
      temp.(0) <- wrd.(1);
      temp.(1) <- wrd.(2);
      temp.(2) <- wrd.(3);
      temp.(3) <- wrd.(0);
      (* don't need this now
      wrd.(0) <- temp.(0);
      wrd.(1) <- temp.(1);
      wrd.(2) <- temp.(2);
      wrd.(3) <- temp.(3); *)
      ()
    in 
    temp
  ;;
  
  (* testing rot_word *)
  (* let _ = 
    assert(first_four.(0).(0) = (rot_word first_four.(0)).(3));
    assert(first_four.(0).(2) = (rot_word first_four.(0)).(1));
    assert(first_four.(0).(1) = (rot_word first_four.(0)).(0));
    assert(first_four.(1).(3) = (rot_word first_four.(1)).(2))
  ;; *)
    
  (* Beginning of the implementation of sub word *)

  (* NOTE: for formatting purposes, I expanded my window and violated the 80 
     character line width restrition.  I didn't see a better way to just type
     in this massive box of values and keep it pretty - sorry. These sub box
     values can be found in the .gov specification *)
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

  (* creating the sub box function. 
   * Invariants: the hex must be a cap letter/int combo. Note we can't just use
   * an easier int_of_string function for the 0-9 cases efficiently because
   * it wouldn't know what to do with cases like "FF". We'd have to add
   * even more filters, so it seemed most straighforward to run a char match *)
  let get_sub_value (hex:string) : string = 
    let row_num : int = 
    match String.get hex 0 with 
    | '0' -> 0 
    | '1' -> 1
    | '2' -> 2
    | '3' -> 3
    | '4' -> 4
    | '5' -> 5
    | '6' -> 6
    | '7' -> 7 
    | '8' -> 8
    | '9' -> 9
    | 'A' -> 10
    | 'B' -> 11
    | 'C' -> 12
    | 'D' -> 13
    | 'E' -> 14
    | 'F' -> 15
    | _ -> failwith "You didn't pass in a hex value correctly!"
    in 
    let col_num :int = 
    match String.get hex 1 with 
    | '0' -> 0 
    | '1' -> 1
    | '2' -> 2
    | '3' -> 3
    | '4' -> 4
    | '5' -> 5
    | '6' -> 6
    | '7' -> 7 
    | '8' -> 8
    | '9' -> 9
    | 'A' -> 10
    | 'B' -> 11
    | 'C' -> 12
    | 'D' -> 13
    | 'E' -> 14
    | 'F' -> 15
    | _ -> failwith "You didn't pass in a hex value correctly!"
    in 
    sbox.(row_num).(col_num)
  ;;
   
  (* testing get_sub_value w/some values from the table *)
  (* let _ = 
    assert(get_sub_value "19" = "d4");
    assert(get_sub_value "FF" = "16");
    assert(get_sub_value "E5" = "d9");
    assert(get_sub_value "2C" = "71")
  ;; *)
  

 (* for own testing purposes learning to use array notation
 (*note intar.(1).(3) results in 8 *)
 let intar : int array array = 
   [|
   [| 0; 1; 2; 3; 4 |];
   [| 5; 6; 7; 8; 9 |]
   |]
 ;;
 *)

 (* Now implementing reverse type functions so we can 
  * actually use the sbox, which is in hex rather than bytes *)
 let byte_to_bin (b:byte_t) : string = 
   Array.fold_right (*~f:*)(fun bt res -> (match bt with 
   | Zero -> ("0" ^ res)
   | One -> ("1" ^ res))) b (*~init:*)""
 ;; 
 
 (* testing byte_to_bin *)
 (* let _ = 
   assert(byte_to_bin [|Zero;Zero;Zero;Zero;Zero;Zero;Zero;Zero;|] ="00000000");
   assert(byte_to_bin [|Zero;Zero;Zero;Zero;Zero;Zero;Zero;One;|] = "00000001");
   assert(byte_to_bin [|Zero;Zero;Zero;Zero;Zero;One;Zero;Zero;|] = "00000100");
   assert(byte_to_bin [|One;Zero;One;Zero;One;Zero;One;Zero;|] = "10101010");
   assert(byte_to_bin [|One;One;One;One;One;One;One;One|] = "11111111")
 ;; *)

 (* had to implement from scratch. Split up the binary into 
  * the two groups of four, convert each to a 0-15 int, then just 
  * convert that int to a hex value *)
 let bin_to_hex (b_str:string) : string = 
   let first_four = String.sub b_str 0 4 in 
   let second_four = String.sub b_str 4 4 in 
   let first_int = int_of_string ("0b" ^ first_four) in 
   let second_int = int_of_string ("0b" ^ second_four) in 
   let int_to_hex (n:int) : string = 
     if n <= 9 then string_of_int n 
     else 
       (match n with  
        | 10 -> "A"
        | 11 -> "B"
        | 12 -> "C"
        | 13 -> "D"
        | 14 -> "E"
        | 15 -> "F"
        | _ -> failwith "You didn't pass in a valid value"
       )
   in 
   (int_to_hex first_int) ^ (int_to_hex second_int)
 ;;

 (* Now just use that helper to create this type converter *)
 let byte_to_hex (b:byte_t) : string = 
   bin_to_hex (byte_to_bin b)
 ;;

 (* testing byte_to_hex *)
 (* let _ = 
   assert(byte_to_hex [|Zero;Zero;Zero;Zero;Zero;Zero;Zero;Zero;|] = "00");
   assert(byte_to_hex [|Zero;Zero;Zero;Zero;Zero;Zero;Zero;One;|] = "01");
   assert(byte_to_hex [|One;One;One;One;Zero;One;Zero;Zero;|] = "F4");
   assert(byte_to_hex [|One;One;Zero;Zero;One;Zero;One;One;|] = "CB");
   assert(byte_to_hex [|Zero;One;One;Zero;One;One;Zero;One;|] = "6D")
 ;;  *)

 (* sub_word returns a new word *)
 let sub_word (wrd:word) : word = 
   let subbed_wrd: word = Array.init 4 (*~f:*)(fun i -> int_to_byte i) in
   let sub_byte_val (b:byte_t) : byte_t = 
     hex_to_byte (get_sub_value (bin_to_hex (byte_to_bin b)))
   in   
   (* since only 4 iterations/lines, just as fast to code by hand as an
    * algorithm or loop. It will always be 4 since a word's size doesn't
    * change regardless of the size of the key used *)
   subbed_wrd.(0) <- sub_byte_val (wrd.(0)); 
   subbed_wrd.(1) <- sub_byte_val (wrd.(1)); 
   subbed_wrd.(2) <- sub_byte_val (wrd.(2)); 
   subbed_wrd.(3) <- sub_byte_val (wrd.(3)); 
   subbed_wrd
  ;; 

 (* testing based on examples give in the .gov spec *)
 (* let _ = 
   assert(hex_to_words "8a84eb01" = sub_word (hex_to_words "cf4f3c09"));
   assert(hex_to_words "50386be5" = sub_word (hex_to_words "6c76052a"));
   assert(hex_to_words "2b9563b9" = sub_word (hex_to_words "0bad00db"))
 ;; *)

 (* since ocaml has a built in XOR operator - but it only takes in integers -
  * I just convert each byte in the word to an integer with a helper function,
  * and then convert back to a byte after calling lxor
  *)
 let byte_to_int (b:byte_t) : int = 
    int_of_string ("0b" ^ (byte_to_bin b))
 ;;

 let xor (wrd1:word) (wrd2:word) : word = 
   let return_wrd  = 
       Array.init 4 (*~f:*)(fun k -> int_to_byte k) 
   in 
   let rec per_byte_xor (wrd1:word) (wrd2:word) (n:int) : word = 
     if n >= 4 then return_wrd else
     (let int1: int = byte_to_int wrd1.(n) in
     let int2: int = byte_to_int wrd2.(n) in 
     return_wrd.(n) <- int_to_byte (int1 lxor int2);
     per_byte_xor wrd1 wrd2 (n+1))
   in 
   per_byte_xor wrd1 wrd2 0
 ;;

 (* testin XOR function, some values ae from examples from .gov spec *)
 (* let _ = 
   assert(xor (hex_to_words "8b84eb01") (hex_to_words "2b7e1516") = 
                                                     (hex_to_words "a0fafe17"));
   assert(xor (hex_to_words "00000000") (hex_to_words "00000000") = 
                                                     (hex_to_words "00000000"));
   assert(xor (hex_to_words "11111111") (hex_to_words "00000000") = 
                                                     (hex_to_words "11111111"));
   assert(xor (hex_to_words "11111111") (hex_to_words "11111111") = 
                                                     (hex_to_words "00000000"));
   assert(xor (hex_to_words "d2c4e23c") (hex_to_words "3d80477d") = 
                                                     (hex_to_words "ef44a541"));
 ;;*)


 (*let byte_to_int (b:byte_t) : int = 
    int_of_string ("0b" ^ (byte_to_bin b))
 ;;
 
 (* quick test *)
let _ = assert((byte_to_int [|Zero;Zero;Zero;One;Zero;One;Zero;One|]) = 21);; *)
 
 (* n is the round we are on. First I create an empty word and then fill 
  * just the first byte as per the algorithm spec. I also hard coded values
  * above n = 8 b/c of the overflow issue and my lack of understanding 
  * Galois field multiplication; for a challenge if I have time I will
  * fix this up.
  * (values from https://www.ime.usp.br/~rt/cranalysis/AESSimplified.pdf)
  *)
 let round_constant (n:int) : word = 
   let rd_constant = Array.init 4 (*~f:*)(fun _ -> int_to_byte 0) in 
   let rec left_bytes (n:int) : byte_t = 
     if n = 0 then (int_to_byte 1)
     else if n < 8 then int_to_byte (0x02 * byte_to_int (left_bytes (n-1)))
     else if n = 8 then (hex_to_byte "1B")
     else if n = 9 then (hex_to_byte "36")
     else if n = 10 then (hex_to_byte "6C")
     else if n = 11 then (hex_to_byte "D8")
     else if n = 12 then (hex_to_byte "AB")
     else if n = 13 then (hex_to_byte "4D")
     else if n = 14 then (hex_to_byte "9A")
     else if n > 14 then (failwith "You don't need that many round keys for 
     128 bit!")
     else (failwith "something went wrong with round_constant!")
   in 
   rd_constant.(0) <- left_bytes n;
   rd_constant
;;

  (* testing round_constant *)
 (* let _ = 
   assert(round_constant 0 = hex_to_words "01000000");
   assert(round_constant 5 = hex_to_words "20000000");
   assert(round_constant 13 = hex_to_words "4d000000")
  ;; *)

 (* NOTE: The module sig has this not take an argument; in implementing
  * later I need to go back and hardcode in expanded key, or make it so it
  * takes in a key in the sig. Also note this can be used for Key-offset as
  * well, if you pass in the key instead of the expanded key *)
 let exp_key_offset (exp_key:word array) (n:int) : word = 
   let wrd_arr = Array.sub exp_key n 1 in 
   wrd_arr.(0)
  ;;
 
 (* coded this up for testing *)
 let word_to_hex (wrd: word) : string = 
   let rec loop_word (wrd:word) (n:int) (s:string) : string = 
     if n >= Array.length wrd then s
     else 
       (let new_s = (s ^ (byte_to_hex wrd.(n))) in 
        loop_word wrd (n+1) new_s)
   in
   loop_word wrd 0 ""
  ;;

 (* here goes nothing, an attempt to implement the whole thing *)
 let expanded_key (key_string:string) : word array = 
   (* number of words in the expanded key. If 128 bit/16 bytes
      we need 44 words. Still working on how to expand this out 
      Key size       Block size      Expanded Key size
        16             16               176 (or 16 * (16+1))
        24             16               208 
        32             16               240  
     *)
   let num_words = 44 in 
   let exp_key : word array = 
      Array.init num_words (*~f:*) (fun i -> 
         Array.init 4 (fun _ -> int_to_byte i))
   in 
   (* get the first four words from the key itself. This could
    * be looped if we went back to implement larger keys, but
    * since we only update 4 values, it's roughly the same amount
    * of code to code it manually  *)
   let k_first_four = init_key_to_words (hex_to_words key_string) in 
   exp_key.(0) <- k_first_four.(0);
   (*Printf.printf "exp_key.(0) is %s \n" (word_to_hex (exp_key.(0)));*)
   exp_key.(1) <- k_first_four.(1);
   exp_key.(2) <- k_first_four.(2);
   exp_key.(3) <- k_first_four.(3);
   (* now start implementing the actual algorithm for 128, 
    * if going back to implement others then n has to change
    *
    * (this might work, if it's always 128 to 4, 192 to 6, etc)
    *)
   let nk : int = (String.length key_string) / 8 in
   (* let n be the word # itself, so starting in this case at 4,
    * nk being 4, 6, or 8 depending on key size. Thanks to the
    * mutable nature of arrays we don't need to pass in the array
    * each time. You need the round constant ((n/nk)-1) because of the
    * 0 index I used in round_constant implementation since I was
    * working off of multiple specs *)
   let rec next_word (n: int) (nk: int) : unit = 
      if n >= num_words then ()
      else if (n mod nk = 0) then
       (let inter_steps : word = 
           xor (sub_word (rot_word (exp_key.(n-1)))) (round_constant ((n/nk)-1))
	in 
        (*Printf.printf "inter_steps n:%i is %s, round constant was %s \n" n 
        (word_to_hex inter_steps) (word_to_hex (round_constant ((n / nk) -1)))*)
        exp_key.(n) <- (xor inter_steps exp_key.(n-nk));
        next_word (n+1) nk)
      else 
        (exp_key.(n) <- (xor exp_key.(n-1) exp_key.(n-nk));
         next_word (n+1) nk)
   in 
   next_word 4 nk;
   exp_key
  ;;

 let expanded_key_test = expanded_key "2b7e151628aed2a6abf7158809cf4f3c";;
  
 (* coded this up for printing and testing *)
 

 let rec print_loop (n:int) : unit = 
   if n >= Array.length expanded_key_test then () 
   else 
   (Printf.printf "Word %i is: %s \n" n (word_to_hex (expanded_key_test.(n)));
   print_loop (n+1))
 ;;

(* let _ = print_loop 0;;*)

end 

(* ================================================================================= *)

(* encrypt/decrypt *)

(* ================================================================================= *)


(* expands a string into a list of chars 
* source: http://stackoverflow.com/questions/10068713/string-to-list-of-char
*)
let explode (s:string) : char list =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []
;;

(*
 * http://caml.inria.fr/pub/old_caml_site/Examples/oc/basics/explode.ml
 *)
let implode l =
  let result = String.create (List.length l) in
  let rec imp i = function
    | [] -> result
    | c :: l -> result.[i] <- c; imp (i + 1) l in
  imp 0 l
;;

(* Integer to binary with padding zeroes - from Joel *)
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
  add_zeroes(strip_bits i "")
;;

(* appends a hexadecimal identifier *)
(*let add_0x_list lst = List.map (fun x -> "0X" ^ x) lst;;*)

let int_to_hex_str = (fun num -> "0X" ^ Printf.sprintf "%02x" num);;

(* halfway helper for getting the State; formats input 
 * as a list of 8 bit strings -> hex format *)
let get_state_strlist (s:string) : string list =
  (* explodes string so that we can use map over chars; "to hex" function *)
  let s = List.map (int_to_hex_str) (List.map int_of_char (explode s)) in 
  if (List.length s > 16) then failwith "string too long"
  else 
    let rec add_padding (slist: string list) : string list = 
       if (List.length slist  < 16) then add_padding ("0X20"::slist) 
       else slist 
    in 
  add_padding s 
;;

(* Injects State (array format) into a matrix (array x array) *)
let state_inject (x2:string array array) (x:string array) =
  x2.(0).(0) <- x.(0); x2.(0).(1) <- x.(4); x2.(0).(2) <- x.(8);  x2.(0).(3) <- x.(12);
  x2.(1).(0) <- x.(1); x2.(1).(1) <- x.(5); x2.(1).(2) <- x.(9);  x2.(1).(3) <- x.(13); 
  x2.(2).(0) <- x.(2); x2.(2).(1) <- x.(6); x2.(2).(2) <- x.(10); x2.(2).(3) <- x.(14); 
  x2.(3).(0) <- x.(3); x2.(3).(1) <- x.(7); x2.(3).(2) <- x.(11); x2.(3).(3) <- x.(15) 
;;

(* empty matrix to inject into; ran into issues with copying directly, so needed to 
 * specify the format *)
let new_state () = [|[|""; ""; ""; ""|]; 
		     [|""; ""; ""; ""|]; 
		     [|""; ""; ""; ""|]; 
		     [|""; ""; ""; ""|]
		   |];;

(* GET_STATE *)
let get_state (s:string) : string array array =
  (* transforms semi-state (list) into a (mutable) array *)
  let x = Array.of_list (get_state_strlist s) in
  let x2 = new_state () in
  (* inject string into state *)
  state_inject x2 x; 
  x2
;;

(*
(* testing *)
(* 16-byte string *)
let string_to_encrypt = "ABCDEFGHIJKLMNOP";;
(* semi-state as a string *)
get_state_strlist string_to_encrypt;;
(* proper State as a 4x4 string array array (matrix) *)
get_state string_to_encrypt;;
*)


(* ADD ROUND KEY *)

open KeyExpand;;
let new_emptyarr() = [|0;0;0;0|];;

let bitToString (b:bit) = 
  match b with 
  |Zero -> "0"
  |One -> "1"
;;

(* changes from words to ints*)
let int_round_keys (manywords:word array) : int array array = 
  let col_key (wrd:word) = 
    let wrd2 = new_emptyarr() in
    for i=0 to 3 do (
      let subword = wrd.(i) in
      wrd2.(i) <- (int_of_string ("0b" ^ (Array.fold_right ( ^ ) 
					    (Array.map bitToString subword) "")))
    ) done;
    wrd2
  in
  Array.map col_key manywords
;;

let add_round_key (state:string array array) (round:int) (key:string) : string array array = 
  let state2 = new_state() in
  let intwords = int_round_keys (expanded_key key) in 
  (* for each set of 4 words *)
  for i=0 to 3 do (
    let col = intwords.(4*round + i) in
    (* for each number to lxor *)
    for j=0 to 3 do (
      (* row of expanded key lxor with column of state (constructional reasons) *)
      state2.(j).(i) <- int_to_hex_str ((int_of_string state.(j).(i)) lxor col.(j))
    ) done
  ) done;
state2
;;

(* SUB_BYTES *)

(* matches individual hex place value with decimal value*)
let hex2d (c: char): int = 
  match c with
  |'0'..'9' -> (int_of_string (Char.escaped c))
  |'a' -> 10
  |'b' -> 11
  |'c' -> 12
  |'d' -> 13
  |'e' -> 14
  |'f' -> 15
  | _ -> failwith "NotInHexFormat"
;;

(*let add_0x_saa = Array.map (Array.map (fun x -> "0X" ^ x));;*)

let sub_bytes (state: string array array) =
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
  in
  let state2 = new_state() in
  for i=0 to 3 do (
    for j=0 to 3 do (
      let hex = state.(i).(j) in
      let z = hex.[2] in
      let g = hex.[3] in 
      state2.(i).(j)  <- ((fun x -> "0X" ^ x)(sbox.(hex2d z).(hex2d g)))
    ) done
  ) done;
  state2
;;

let inv_sub_bytes (state: string array array) = 
  let inv_sbox : string array array =  
    [|
      [|"52";"09";"6a";"d5";"30";"36";"a5";"38";"bf";"40";"a3";"9e";"81";"f3";"d7";"fb"|];
      [|"7c";"e3";"39";"82";"9b";"2f";"ff";"87";"34";"8e";"43";"44";"c4";"de";"e9";"cb"|];
      [|"54";"7b";"94";"32";"a6";"c2";"23";"3d";"ee";"4c";"95";"0b";"42";"fa";"c3";"4e"|];
      [|"08";"2e";"a1";"66";"28";"d9";"24";"b2";"76";"5b";"a2";"49";"6d";"8b";"d1";"25"|];
      [|"72";"f8";"f6";"64";"86";"68";"98";"16";"d4";"a4";"5c";"cc";"5d";"65";"b6";"92"|];
      [|"6c";"70";"48";"50";"fd";"ed";"b9";"da";"5e";"15";"46";"57";"a7";"8d";"9d";"84"|];
      [|"90";"d8";"ab";"00";"8c";"bc";"d3";"0a";"f7";"e4";"58";"05";"b8";"b3";"45";"06"|];
      [|"d0";"2c";"1e";"8f";"ca";"3f";"0f";"02";"c1";"af";"bd";"03";"01";"13";"8a";"6b"|];
      [|"3a";"91";"11";"41";"4f";"67";"dc";"ea";"97";"f2";"cf";"ce";"f0";"b4";"e6";"73"|];
      [|"96";"ac";"74";"22";"e7";"ad";"35";"85";"e2";"f9";"37";"e8";"1c";"75";"df";"6e"|];
      [|"47";"f1";"1a";"71";"1d";"29";"c5";"89";"6f";"b7";"62";"0e";"aa";"18";"be";"1b"|];
      [|"fc";"56";"3e";"4b";"c6";"d2";"79";"20";"9a";"db";"c0";"fe";"78";"cd";"5a";"f4"|];
      [|"1f";"dd";"a8";"33";"88";"07";"c7";"31";"b1";"12";"10";"59";"27";"80";"ec";"5f"|];
      [|"60";"51";"7f";"a9";"19";"b5";"4a";"0d";"2d";"e5";"7a";"9f";"93";"c9";"9c";"ef"|];
      [|"a0";"e0";"3b";"4d";"ae";"2a";"f5";"b0";"c8";"eb";"bb";"3c";"83";"53";"99";"61"|];
      [|"17";"2b";"04";"7e";"ba";"77";"d6";"26";"e1";"69";"14";"63";"55";"21";"0c";"7d"|]
    |]
  in
  let state2 = new_state() in
  for i=0 to 3 do (
    for j=0 to 3 do (
      let hex = state.(i).(j) in 
      let z = hex.[2] in
      let g = hex.[3] in 
      state2.(i).(j) <- ((fun x -> "0X" ^ x)(inv_sbox.(hex2d z).(hex2d g)))
    ) done
  ) done;
  state2
;;

(*
let state = get_state string_to_encrypt;;
sub_bytes state;;
;;
*)

(*
assert (get_state string_to_encrypt = inv_sub_bytes (sub_bytes (get_state string_to_encrypt)));;
*)

(* 
* SHIFT ROWS
*)

(* for encryption - helper *)
let shift_rows_right (x:string array array) (x2:string array array) =
  for n=0 to 3 do (
    x2.(0).(n) <- x.(0).(n)
  ) done;
  x2.(1).(0) <- x.(1).(1); x2.(1).(1) <- x.(1).(2); x2.(1).(2) <- x.(1).(3);  x2.(1).(3) <- x.(1).(0); 
  x2.(2).(0) <- x.(2).(2); x2.(2).(1) <- x.(2).(3); x2.(2).(2) <- x.(2).(0);  x2.(2).(3) <- x.(2).(1); 
  x2.(3).(0) <- x.(3).(3); x2.(3).(1) <- x.(3).(0); x2.(3).(2) <- x.(3).(1);  x2.(3).(3) <- x.(3).(2);
  x2
;;

(* for decryption - helper *)
let shift_rows_left (x:string array array) (x2:string array array) =
  for n=0 to 3 do (
    x2.(0).(n) <- x.(0).(n)
  ) done;
  x2.(1).(0) <- x.(1).(3); x2.(1).(1) <- x.(1).(0); x2.(1).(2) <- x.(1).(1);  x2.(1).(3) <- x.(1).(2); 
  x2.(2).(0) <- x.(2).(2); x2.(2).(1) <- x.(2).(3); x2.(2).(2) <- x.(2).(0);  x2.(2).(3) <- x.(2).(1); 
  x2.(3).(0) <- x.(3).(1); x2.(3).(1) <- x.(3).(2); x2.(3).(2) <- x.(3).(3);  x2.(3).(3) <- x.(3).(0);
  x2
;;

(* encryption *)
let shift_rows matrix =
  shift_rows_right matrix (new_state ())
;;

(* decryption *)
let inv_shift_rows matrix =
  shift_rows_left matrix (new_state ())
;;

(*let string_to_encrypt = "ABCDEFGHIJKLMNOP";;*)
(* assert that inverse works *)
(*assert ((get_state string_to_encrypt) = (inv_shift_rows (shift_rows (get_state string_to_encrypt))))*)

(*let state = shift_rows (sub_bytes (get_state string_to_encrypt));;*)


(* MIX COLUMNS *)

(* helper functions *)

let getCol (state:string array array) (n:int) = 
  let col = [|"";"";"";""|] in
  for i=0 to 3 do (col.(i) <- state.(i).(n)) done;
  col
;;

let wrap (num:int) = 
  if (num > 0xff) then (num mod 0xff) else (num)
;;

let galMultip (a:char) (b:char) (c:int) = 
  let table_2 = [|
    [|"0x00";"0x02";"0x04";"0x06";"0x08";"0x0a";"0x0c";"0x0e";"0x10";"0x12";"0x14";"0x16";"0x18";"0x1a";"0x1c";"0x1e"|];
    [|"0x20";"0x22";"0x24";"0x26";"0x28";"0x2a";"0x2c";"0x2e";"0x30";"0x32";"0x34";"0x36";"0x38";"0x3a";"0x3c";"0x3e"|];
    [|"0x40";"0x42";"0x44";"0x46";"0x48";"0x4a";"0x4c";"0x4e";"0x50";"0x52";"0x54";"0x56";"0x58";"0x5a";"0x5c";"0x5e"|];
    [|"0x60";"0x62";"0x64";"0x66";"0x68";"0x6a";"0x6c";"0x6e";"0x70";"0x72";"0x74";"0x76";"0x78";"0x7a";"0x7c";"0x7e"|];
    [|"0x80";"0x82";"0x84";"0x86";"0x88";"0x8a";"0x8c";"0x8e";"0x90";"0x92";"0x94";"0x96";"0x98";"0x9a";"0x9c";"0x9e"|];
    [|"0xa0";"0xa2";"0xa4";"0xa6";"0xa8";"0xaa";"0xac";"0xae";"0xb0";"0xb2";"0xb4";"0xb6";"0xb8";"0xba";"0xbc";"0xbe"|];
    [|"0xc0";"0xc2";"0xc4";"0xc6";"0xc8";"0xca";"0xcc";"0xce";"0xd0";"0xd2";"0xd4";"0xd6";"0xd8";"0xda";"0xdc";"0xde"|];
    [|"0xe0";"0xe2";"0xe4";"0xe6";"0xe8";"0xea";"0xec";"0xee";"0xf0";"0xf2";"0xf4";"0xf6";"0xf8";"0xfa";"0xfc";"0xfe"|];
    [|"0x1b";"0x19";"0x1f";"0x1d";"0x13";"0x11";"0x17";"0x15";"0x0b";"0x09";"0x0f";"0x0d";"0x03";"0x01";"0x07";"0x05"|];
    [|"0x3b";"0x39";"0x3f";"0x3d";"0x33";"0x31";"0x37";"0x35";"0x2b";"0x29";"0x2f";"0x2d";"0x23";"0x21";"0x27";"0x25"|];
    [|"0x5b";"0x59";"0x5f";"0x5d";"0x53";"0x51";"0x57";"0x55";"0x4b";"0x49";"0x4f";"0x4d";"0x43";"0x41";"0x47";"0x45"|];
    [|"0x7b";"0x79";"0x7f";"0x7d";"0x73";"0x71";"0x77";"0x75";"0x6b";"0x69";"0x6f";"0x6d";"0x63";"0x61";"0x67";"0x65"|];
    [|"0x9b";"0x99";"0x9f";"0x9d";"0x93";"0x91";"0x97";"0x95";"0x8b";"0x89";"0x8f";"0x8d";"0x83";"0x81";"0x87";"0x85"|];
    [|"0xbb";"0xb9";"0xbf";"0xbd";"0xb3";"0xb1";"0xb7";"0xb5";"0xab";"0xa9";"0xaf";"0xad";"0xa3";"0xa1";"0xa7";"0xa5"|];
    [|"0xdb";"0xd9";"0xdf";"0xdd";"0xd3";"0xd1";"0xd7";"0xd5";"0xcb";"0xc9";"0xcf";"0xcd";"0xc3";"0xc1";"0xc7";"0xc5"|];
    [|"0xfb";"0xf9";"0xff";"0xfd";"0xf3";"0xf1";"0xf7";"0xf5";"0xeb";"0xe9";"0xef";"0xed";"0xe3";"0xe1";"0xe7";"0xe5"|]
		|] 
  in
  let table_3 =  [|
    [|"0x00";"0x03";"0x06";"0x05";"0x0c";"0x0f";"0x0a";"0x09";"0x18";"0x1b";"0x1e";"0x1d";"0x14";"0x17";"0x12";"0x11"|];
    [|"0x30";"0x33";"0x36";"0x35";"0x3c";"0x3f";"0x3a";"0x39";"0x28";"0x2b";"0x2e";"0x2d";"0x24";"0x27";"0x22";"0x21"|];
    [|"0x60";"0x63";"0x66";"0x65";"0x6c";"0x6f";"0x6a";"0x69";"0x78";"0x7b";"0x7e";"0x7d";"0x74";"0x77";"0x72";"0x71"|];
    [|"0x50";"0x53";"0x56";"0x55";"0x5c";"0x5f";"0x5a";"0x59";"0x48";"0x4b";"0x4e";"0x4d";"0x44";"0x47";"0x42";"0x41"|];
    [|"0xc0";"0xc3";"0xc6";"0xc5";"0xcc";"0xcf";"0xca";"0xc9";"0xd8";"0xdb";"0xde";"0xdd";"0xd4";"0xd7";"0xd2";"0xd1"|];
    [|"0xf0";"0xf3";"0xf6";"0xf5";"0xfc";"0xff";"0xfa";"0xf9";"0xe8";"0xeb";"0xee";"0xed";"0xe4";"0xe7";"0xe2";"0xe1"|];
    [|"0xa0";"0xa3";"0xa6";"0xa5";"0xac";"0xaf";"0xaa";"0xa9";"0xb8";"0xbb";"0xbe";"0xbd";"0xb4";"0xb7";"0xb2";"0xb1"|];
    [|"0x90";"0x93";"0x96";"0x95";"0x9c";"0x9f";"0x9a";"0x99";"0x88";"0x8b";"0x8e";"0x8d";"0x84";"0x87";"0x82";"0x81"|];
    [|"0x9b";"0x98";"0x9d";"0x9e";"0x97";"0x94";"0x91";"0x92";"0x83";"0x80";"0x85";"0x86";"0x8f";"0x8c";"0x89";"0x8a"|];
    [|"0xab";"0xa8";"0xad";"0xae";"0xa7";"0xa4";"0xa1";"0xa2";"0xb3";"0xb0";"0xb5";"0xb6";"0xbf";"0xbc";"0xb9";"0xba"|];
    [|"0xfb";"0xf8";"0xfd";"0xfe";"0xf7";"0xf4";"0xf1";"0xf2";"0xe3";"0xe0";"0xe5";"0xe6";"0xef";"0xec";"0xe9";"0xea"|];
    [|"0xcb";"0xc8";"0xcd";"0xce";"0xc7";"0xc4";"0xc1";"0xc2";"0xd3";"0xd0";"0xd5";"0xd6";"0xdf";"0xdc";"0xd9";"0xda"|];
    [|"0x5b";"0x58";"0x5d";"0x5e";"0x57";"0x54";"0x51";"0x52";"0x43";"0x40";"0x45";"0x46";"0x4f";"0x4c";"0x49";"0x4a"|];
    [|"0x6b";"0x68";"0x6d";"0x6e";"0x67";"0x64";"0x61";"0x62";"0x73";"0x70";"0x75";"0x76";"0x7f";"0x7c";"0x79";"0x7a"|];
    [|"0x3b";"0x38";"0x3d";"0x3e";"0x37";"0x34";"0x31";"0x32";"0x23";"0x20";"0x25";"0x26";"0x2f";"0x2c";"0x29";"0x2a"|];
    [|"0x0b";"0x08";"0x0d";"0x0e";"0x07";"0x04";"0x01";"0x02";"0x13";"0x10";"0x15";"0x16";"0x1f";"0x1c";"0x19";"0x1a"|]
		 |] 
  in
  let table_9 = [|
    [|"0x00";"0x09";"0x12";"0x1b";"0x24";"0x2d";"0x36";"0x3f";"0x48";"0x41";"0x5a";"0x53";"0x6c";"0x65";"0x7e";"0x77"|];
    [|"0x90";"0x99";"0x82";"0x8b";"0xb4";"0xbd";"0xa6";"0xaf";"0xd8";"0xd1";"0xca";"0xc3";"0xfc";"0xf5";"0xee";"0xe7"|];
    [|"0x3b";"0x32";"0x29";"0x20";"0x1f";"0x16";"0x0d";"0x04";"0x73";"0x7a";"0x61";"0x68";"0x57";"0x5e";"0x45";"0x4c"|];
    [|"0xab";"0xa2";"0xb9";"0xb0";"0x8f";"0x86";"0x9d";"0x94";"0xe3";"0xea";"0xf1";"0xf8";"0xc7";"0xce";"0xd5";"0xdc"|];
    [|"0x76";"0x7f";"0x64";"0x6d";"0x52";"0x5b";"0x40";"0x49";"0x3e";"0x37";"0x2c";"0x25";"0x1a";"0x13";"0x08";"0x01"|];
    [|"0xe6";"0xef";"0xf4";"0xfd";"0xc2";"0xcb";"0xd0";"0xd9";"0xae";"0xa7";"0xbc";"0xb5";"0x8a";"0x83";"0x98";"0x91"|];
    [|"0x4d";"0x44";"0x5f";"0x56";"0x69";"0x60";"0x7b";"0x72";"0x05";"0x0c";"0x17";"0x1e";"0x21";"0x28";"0x33";"0x3a"|];
    [|"0xdd";"0xd4";"0xcf";"0xc6";"0xf9";"0xf0";"0xeb";"0xe2";"0x95";"0x9c";"0x87";"0x8e";"0xb1";"0xb8";"0xa3";"0xaa"|];
    [|"0xec";"0xe5";"0xfe";"0xf7";"0xc8";"0xc1";"0xda";"0xd3";"0xa4";"0xad";"0xb6";"0xbf";"0x80";"0x89";"0x92";"0x9b"|];
    [|"0x7c";"0x75";"0x6e";"0x67";"0x58";"0x51";"0x4a";"0x43";"0x34";"0x3d";"0x26";"0x2f";"0x10";"0x19";"0x02";"0x0b"|];
    [|"0xd7";"0xde";"0xc5";"0xcc";"0xf3";"0xfa";"0xe1";"0xe8";"0x9f";"0x96";"0x8d";"0x84";"0xbb";"0xb2";"0xa9";"0xa0"|];
    [|"0x47";"0x4e";"0x55";"0x5c";"0x63";"0x6a";"0x71";"0x78";"0x0f";"0x06";"0x1d";"0x14";"0x2b";"0x22";"0x39";"0x30"|];
    [|"0x9a";"0x93";"0x88";"0x81";"0xbe";"0xb7";"0xac";"0xa5";"0xd2";"0xdb";"0xc0";"0xc9";"0xf6";"0xff";"0xe4";"0xed"|];
    [|"0x0a";"0x03";"0x18";"0x11";"0x2e";"0x27";"0x3c";"0x35";"0x42";"0x4b";"0x50";"0x59";"0x66";"0x6f";"0x74";"0x7d"|];
    [|"0xa1";"0xa8";"0xb3";"0xba";"0x85";"0x8c";"0x97";"0x9e";"0xe9";"0xe0";"0xfb";"0xf2";"0xcd";"0xc4";"0xdf";"0xd6"|];
    [|"0x31";"0x38";"0x23";"0x2a";"0x15";"0x1c";"0x07";"0x0e";"0x79";"0x70";"0x6b";"0x62";"0x5d";"0x54";"0x4f";"0x46"|]
		|]
  in
  let table_11 = [|
    [|"0x00";"0x0b";"0x16";"0x1d";"0x2c";"0x27";"0x3a";"0x31";"0x58";"0x53";"0x4e";"0x45";"0x74";"0x7f";"0x62";"0x69"|];
    [|"0xb0";"0xbb";"0xa6";"0xad";"0x9c";"0x97";"0x8a";"0x81";"0xe8";"0xe3";"0xfe";"0xf5";"0xc4";"0xcf";"0xd2";"0xd9"|];
    [|"0x7b";"0x70";"0x6d";"0x66";"0x57";"0x5c";"0x41";"0x4a";"0x23";"0x28";"0x35";"0x3e";"0x0f";"0x04";"0x19";"0x12"|];
    [|"0xcb";"0xc0";"0xdd";"0xd6";"0xe7";"0xec";"0xf1";"0xfa";"0x93";"0x98";"0x85";"0x8e";"0xbf";"0xb4";"0xa9";"0xa2"|];
    [|"0xf6";"0xfd";"0xe0";"0xeb";"0xda";"0xd1";"0xcc";"0xc7";"0xae";"0xa5";"0xb8";"0xb3";"0x82";"0x89";"0x94";"0x9f"|];
    [|"0x46";"0x4d";"0x50";"0x5b";"0x6a";"0x61";"0x7c";"0x77";"0x1e";"0x15";"0x08";"0x03";"0x32";"0x39";"0x24";"0x2f"|];
    [|"0x8d";"0x86";"0x9b";"0x90";"0xa1";"0xaa";"0xb7";"0xbc";"0xd5";"0xde";"0xc3";"0xc8";"0xf9";"0xf2";"0xef";"0xe4"|];
    [|"0x3d";"0x36";"0x2b";"0x20";"0x11";"0x1a";"0x07";"0x0c";"0x65";"0x6e";"0x73";"0x78";"0x49";"0x42";"0x5f";"0x54"|];
    [|"0xf7";"0xfc";"0xe1";"0xea";"0xdb";"0xd0";"0xcd";"0xc6";"0xaf";"0xa4";"0xb9";"0xb2";"0x83";"0x88";"0x95";"0x9e"|];
    [|"0x47";"0x4c";"0x51";"0x5a";"0x6b";"0x60";"0x7d";"0x76";"0x1f";"0x14";"0x09";"0x02";"0x33";"0x38";"0x25";"0x2e"|];
    [|"0x8c";"0x87";"0x9a";"0x91";"0xa0";"0xab";"0xb6";"0xbd";"0xd4";"0xdf";"0xc2";"0xc9";"0xf8";"0xf3";"0xee";"0xe5"|];
    [|"0x3c";"0x37";"0x2a";"0x21";"0x10";"0x1b";"0x06";"0x0d";"0x64";"0x6f";"0x72";"0x79";"0x48";"0x43";"0x5e";"0x55"|];
    [|"0x01";"0x0a";"0x17";"0x1c";"0x2d";"0x26";"0x3b";"0x30";"0x59";"0x52";"0x4f";"0x44";"0x75";"0x7e";"0x63";"0x68"|];
    [|"0xb1";"0xba";"0xa7";"0xac";"0x9d";"0x96";"0x8b";"0x80";"0xe9";"0xe2";"0xff";"0xf4";"0xc5";"0xce";"0xd3";"0xd8"|];
    [|"0x7a";"0x71";"0x6c";"0x67";"0x56";"0x5d";"0x40";"0x4b";"0x22";"0x29";"0x34";"0x3f";"0x0e";"0x05";"0x18";"0x13"|];
    [|"0xca";"0xc1";"0xdc";"0xd7";"0xe6";"0xed";"0xf0";"0xfb";"0x92";"0x99";"0x84";"0x8f";"0xbe";"0xb5";"0xa8";"0xa3"|]
		 |]
  in
  let table_13 = [|
    [|"0x00";"0x0d";"0x1a";"0x17";"0x34";"0x39";"0x2e";"0x23";"0x68";"0x65";"0x72";"0x7f";"0x5c";"0x51";"0x46";"0x4b"|];
    [|"0xd0";"0xdd";"0xca";"0xc7";"0xe4";"0xe9";"0xfe";"0xf3";"0xb8";"0xb5";"0xa2";"0xaf";"0x8c";"0x81";"0x96";"0x9b"|];
    [|"0xbb";"0xb6";"0xa1";"0xac";"0x8f";"0x82";"0x95";"0x98";"0xd3";"0xde";"0xc9";"0xc4";"0xe7";"0xea";"0xfd";"0xf0"|];
    [|"0x6b";"0x66";"0x71";"0x7c";"0x5f";"0x52";"0x45";"0x48";"0x03";"0x0e";"0x19";"0x14";"0x37";"0x3a";"0x2d";"0x20"|];
    [|"0x6d";"0x60";"0x77";"0x7a";"0x59";"0x54";"0x43";"0x4e";"0x05";"0x08";"0x1f";"0x12";"0x31";"0x3c";"0x2b";"0x26"|];
    [|"0xbd";"0xb0";"0xa7";"0xaa";"0x89";"0x84";"0x93";"0x9e";"0xd5";"0xd8";"0xcf";"0xc2";"0xe1";"0xec";"0xfb";"0xf6"|];
    [|"0xd6";"0xdb";"0xcc";"0xc1";"0xe2";"0xef";"0xf8";"0xf5";"0xbe";"0xb3";"0xa4";"0xa9";"0x8a";"0x87";"0x90";"0x9d"|];
    [|"0x06";"0x0b";"0x1c";"0x11";"0x32";"0x3f";"0x28";"0x25";"0x6e";"0x63";"0x74";"0x79";"0x5a";"0x57";"0x40";"0x4d"|];
    [|"0xda";"0xd7";"0xc0";"0xcd";"0xee";"0xe3";"0xf4";"0xf9";"0xb2";"0xbf";"0xa8";"0xa5";"0x86";"0x8b";"0x9c";"0x91"|];
    [|"0x0a";"0x07";"0x10";"0x1d";"0x3e";"0x33";"0x24";"0x29";"0x62";"0x6f";"0x78";"0x75";"0x56";"0x5b";"0x4c";"0x41"|];
    [|"0x61";"0x6c";"0x7b";"0x76";"0x55";"0x58";"0x4f";"0x42";"0x09";"0x04";"0x13";"0x1e";"0x3d";"0x30";"0x27";"0x2a"|];
    [|"0xb1";"0xbc";"0xab";"0xa6";"0x85";"0x88";"0x9f";"0x92";"0xd9";"0xd4";"0xc3";"0xce";"0xed";"0xe0";"0xf7";"0xfa"|];
    [|"0xb7";"0xba";"0xad";"0xa0";"0x83";"0x8e";"0x99";"0x94";"0xdf";"0xd2";"0xc5";"0xc8";"0xeb";"0xe6";"0xf1";"0xfc"|];
    [|"0x67";"0x6a";"0x7d";"0x70";"0x53";"0x5e";"0x49";"0x44";"0x0f";"0x02";"0x15";"0x18";"0x3b";"0x36";"0x21";"0x2c"|];
    [|"0x0c";"0x01";"0x16";"0x1b";"0x38";"0x35";"0x22";"0x2f";"0x64";"0x69";"0x7e";"0x73";"0x50";"0x5d";"0x4a";"0x47"|];
    [|"0xdc";"0xd1";"0xc6";"0xcb";"0xe8";"0xe5";"0xf2";"0xff";"0xb4";"0xb9";"0xae";"0xa3";"0x80";"0x8d";"0x9a";"0x97"|]
		 |]
  in
  let table_14 = [|
    [|"0x00";"0x0e";"0x1c";"0x12";"0x38";"0x36";"0x24";"0x2a";"0x70";"0x7e";"0x6c";"0x62";"0x48";"0x46";"0x54";"0x5a"|];
    [|"0xe0";"0xee";"0xfc";"0xf2";"0xd8";"0xd6";"0xc4";"0xca";"0x90";"0x9e";"0x8c";"0x82";"0xa8";"0xa6";"0xb4";"0xba"|];
    [|"0xdb";"0xd5";"0xc7";"0xc9";"0xe3";"0xed";"0xff";"0xf1";"0xab";"0xa5";"0xb7";"0xb9";"0x93";"0x9d";"0x8f";"0x81"|];
    [|"0x3b";"0x35";"0x27";"0x29";"0x03";"0x0d";"0x1f";"0x11";"0x4b";"0x45";"0x57";"0x59";"0x73";"0x7d";"0x6f";"0x61"|];
    [|"0xad";"0xa3";"0xb1";"0xbf";"0x95";"0x9b";"0x89";"0x87";"0xdd";"0xd3";"0xc1";"0xcf";"0xe5";"0xeb";"0xf9";"0xf7"|];
    [|"0x4d";"0x43";"0x51";"0x5f";"0x75";"0x7b";"0x69";"0x67";"0x3d";"0x33";"0x21";"0x2f";"0x05";"0x0b";"0x19";"0x17"|];
    [|"0x76";"0x78";"0x6a";"0x64";"0x4e";"0x40";"0x52";"0x5c";"0x06";"0x08";"0x1a";"0x14";"0x3e";"0x30";"0x22";"0x2c"|];
    [|"0x96";"0x98";"0x8a";"0x84";"0xae";"0xa0";"0xb2";"0xbc";"0xe6";"0xe8";"0xfa";"0xf4";"0xde";"0xd0";"0xc2";"0xcc"|];
    [|"0x41";"0x4f";"0x5d";"0x53";"0x79";"0x77";"0x65";"0x6b";"0x31";"0x3f";"0x2d";"0x23";"0x09";"0x07";"0x15";"0x1b"|];
    [|"0xa1";"0xaf";"0xbd";"0xb3";"0x99";"0x97";"0x85";"0x8b";"0xd1";"0xdf";"0xcd";"0xc3";"0xe9";"0xe7";"0xf5";"0xfb"|];
    [|"0x9a";"0x94";"0x86";"0x88";"0xa2";"0xac";"0xbe";"0xb0";"0xea";"0xe4";"0xf6";"0xf8";"0xd2";"0xdc";"0xce";"0xc0"|];
    [|"0x7a";"0x74";"0x66";"0x68";"0x42";"0x4c";"0x5e";"0x50";"0x0a";"0x04";"0x16";"0x18";"0x32";"0x3c";"0x2e";"0x20"|];
    [|"0xec";"0xe2";"0xf0";"0xfe";"0xd4";"0xda";"0xc8";"0xc6";"0x9c";"0x92";"0x80";"0x8e";"0xa4";"0xaa";"0xb8";"0xb6"|];
    [|"0x0c";"0x02";"0x10";"0x1e";"0x34";"0x3a";"0x28";"0x26";"0x7c";"0x72";"0x60";"0x6e";"0x44";"0x4a";"0x58";"0x56"|];
    [|"0x37";"0x39";"0x2b";"0x25";"0x0f";"0x01";"0x13";"0x1d";"0x47";"0x49";"0x5b";"0x55";"0x7f";"0x71";"0x63";"0x6d"|];
    [|"0xd7";"0xd9";"0xcb";"0xc5";"0xef";"0xe1";"0xf3";"0xfd";"0xa7";"0xa9";"0xbb";"0xb5";"0x9f";"0x91";"0x83";"0x8d"|]
		 |]
  in
  match c with 
  |1 -> int_of_string("0x" ^ (Char.escaped a) ^ (Char.escaped b)) 
  |2 -> int_of_string(table_2.(hex2d a).(hex2d b))
  |3 -> int_of_string(table_3.(hex2d a).(hex2d b))
  |9 -> int_of_string(table_9.(hex2d a).(hex2d b))
  |11 -> int_of_string(table_11.(hex2d a).(hex2d b))
  |13 -> int_of_string(table_13.(hex2d a).(hex2d b))
  |14 -> int_of_string(table_14.(hex2d a).(hex2d b))
  |_ -> failwith "Galois lookup failed"
;;

(*
    let ebox : string array array = 
    [|
      [|"01";"03";"05";"0f";"11";"33";"55";"ff";"1a";"2e";"72";"96";"a1";"f8";"13";"35"|];
      [|"5f";"e1";"38";"48";"d8";"73";"95";"a4";"f7";"02";"06";"0a";"1e";"22";"66";"aa"|];
      [|"e5";"34";"5c";"e4";"37";"59";"eb";"26";"6a";"be";"d9";"70";"90";"ab";"e6";"31"|];
      [|"53";"f5";"04";"0c";"14";"3c";"44";"cc";"4f";"d1";"68";"b8";"d3";"6e";"b2";"cd"|];
      [|"4c";"d4";"67";"a9";"e0";"3b";"4d";"d7";"62";"a6";"f1";"08";"18";"28";"78";"88"|];
      [|"83";"9e";"b9";"d0";"6b";"bd";"dc";"7f";"81";"98";"b3";"ce";"49";"db";"76";"9a"|];
      [|"b5";"c4";"57";"f9";"10";"30";"50";"f0";"0b";"1d";"27";"69";"bb";"d6";"61";"a3"|];
      [|"fe";"19";"2b";"7d";"87";"92";"ad";"ec";"2f";"71";"93";"ae";"e9";"20";"60";"a0"|];
      [|"fb";"16";"3a";"4e";"d2";"6d";"b7";"c2";"5d";"e7";"32";"56";"fa";"15";"3f";"41"|];
      [|"c3";"5e";"e2";"3d";"47";"c9";"40";"c0";"5b";"ed";"2c";"74";"9c";"bf";"da";"75"|];
      [|"9f";"ba";"d5";"64";"ac";"ef";"2a";"7e";"82";"9d";"bc";"df";"7a";"8e";"89";"80"|];
      [|"9b";"b6";"c1";"58";"e8";"23";"65";"af";"ea";"25";"6f";"b1";"c8";"43";"c5";"54"|];
      [|"fc";"1f";"21";"63";"a5";"f4";"07";"09";"1b";"2d";"77";"99";"b0";"cb";"46";"ca"|];
      [|"45";"cf";"4a";"de";"79";"8b";"86";"91";"a8";"e3";"3e";"42";"c6";"51";"f3";"0e"|];
      [|"12";"36";"5a";"ee";"29";"7b";"8d";"8c";"8f";"8a";"85";"94";"a7";"f2";"0d";"17"|];
      [|"39";"4b";"dd";"7c";"84";"97";"a2";"fd";"1c";"24";"6c";"b4";"c7";"52";"f6";"01"|]
    |]
  in
  let lbox: string array array = 
    [|
      [|  "";"00";"19";"01";"32";"02";"1a";"c6";"4b";"c7";"1b";"68";"33";"ee";"df";"03"|];
      [|"64";"04";"e0";"0e";"34";"8d";"81";"ef";"4c";"71";"08";"c8";"f8";"69";"1c";"c1"|];
      [|"7d";"c2";"1d";"b5";"f9";"b9";"27";"6a";"4d";"e4";"a6";"72";"9a";"c9";"09";"78"|];
      [|"65";"2f";"8a";"05";"21";"0f";"e1";"24";"12";"f0";"82";"45";"35";"93";"da";"8e"|];
      [|"96";"8f";"db";"bd";"36";"d0";"ce";"94";"13";"5c";"d2";"f1";"40";"46";"83";"38"|];
      [|"66";"dd";"fd";"30";"bf";"06";"8b";"62";"b3";"25";"e2";"98";"22";"88";"91";"10"|];
      [|"7e";"6e";"48";"c3";"a3";"b6";"1e";"42";"3a";"6b";"28";"54";"fa";"85";"3d";"ba"|];
      [|"2b";"79";"0a";"15";"9b";"9f";"5e";"ca";"4e";"d4";"ac";"e5";"f3";"73";"a7";"57"|];
      [|"af";"58";"a8";"50";"f4";"ea";"d6";"74";"4f";"ae";"e9";"d5";"e7";"e6";"ad";"e8"|];
      [|"2c";"d7";"75";"7a";"eb";"16";"0b";"f5";"59";"cb";"5f";"b0";"9c";"a9";"51";"a0"|];
      [|"7f";"0c";"f6";"6f";"17";"c4";"49";"ec";"d8";"43";"1f";"2d";"a4";"76";"7b";"b7"|];
      [|"cc";"bb";"3e";"5a";"fb";"60";"b1";"86";"3b";"52";"a1";"6c";"aa";"55";"29";"9d"|];
      [|"97";"b2";"87";"90";"61";"be";"dc";"fc";"bc";"95";"cf";"cd";"37";"3f";"5b";"d1"|];
      [|"53";"39";"84";"3c";"41";"a2";"6d";"47";"14";"2a";"9e";"5d";"56";"f2";"d3";"ab"|];
      [|"44";"11";"92";"d9";"23";"20";"2e";"89";"b4";"7c";"b8";"26";"77";"99";"e3";"a5"|];
      [|"67";"4a";"ed";"de";"c5";"31";"fe";"18";"0d";"63";"8c";"80";"c0";"f7";"70";"07"|]
    |]
*)

(*
  let hexstr = int_to_hex_str(
    wrap (int_of_string("0x" ^ lbox.(hex2d a).(hex2d b)) + int_of_string("0x" ^ lbox.(0).(c)))) in 
  int_of_string("0x" ^ ebox.(hex2d hexstr.[2]).(hex2d hexstr.[3]))

  ;; 
*)

let hex_unpack s = (s.[2], s.[3]);;
let break_col c = (c.(0), c.(1), c.(2), c.(3));;

let mix_columns state =
  let output = new_state () in
  for n=0 to 3 do ( 
    let col = getCol state n in
    let b0,b1,b2,b3 = break_col col in
    let x1,x2 = hex_unpack b0 in
    let x3,x4 = hex_unpack b1 in
    let x5,x6 = hex_unpack b2 in 
    let x7,x8 = hex_unpack b3 in
    output.(0).(n) <- int_to_hex_str((galMultip x1 x2 2) lxor (galMultip x3 x4 3) lxor (galMultip x5 x6 1) lxor (galMultip x7 x8 1)); 
    output.(1).(n) <- int_to_hex_str((galMultip x1 x2 1) lxor (galMultip x3 x4 2) lxor (galMultip x5 x6 3) lxor (galMultip x7 x8 1)); 
    output.(2).(n) <- int_to_hex_str((galMultip x1 x2 1) lxor (galMultip x3 x4 1) lxor (galMultip x5 x6 2) lxor (galMultip x7 x8 3)); 
    output.(3).(n) <- int_to_hex_str((galMultip x1 x2 3) lxor (galMultip x3 x4 1) lxor (galMultip x5 x6 1) lxor (galMultip x7 x8 2)); 
  ) done;
  output
;;

let inv_mix_columns state =
  let output = new_state () in
  for n=0 to 3 do ( 
    let col = getCol state n in
    let b0,b1,b2,b3 = break_col col in
    let x1,x2 = hex_unpack b0 in
    let x3,x4 = hex_unpack b1 in
    let x5,x6 = hex_unpack b2 in 
    let x7,x8 = hex_unpack b3 in
    output.(0).(n) <- int_to_hex_str((galMultip x1 x2 14) lxor (galMultip x3 x4 11) lxor (galMultip x5 x6 13) lxor (galMultip x7 x8 9)); 
    output.(1).(n) <- int_to_hex_str((galMultip x1 x2 9) lxor (galMultip x3 x4 14) lxor (galMultip x5 x6 11) lxor (galMultip x7 x8 13)); 
    output.(2).(n) <- int_to_hex_str((galMultip x1 x2 13) lxor (galMultip x3 x4 9) lxor (galMultip x5 x6 14) lxor (galMultip x7 x8 11)); 
    output.(3).(n) <- int_to_hex_str((galMultip x1 x2 11) lxor (galMultip x3 x4 13) lxor (galMultip x5 x6 9) lxor (galMultip x7 x8 14)); 
  ) done;
  output
;;

(*commenting out for benefit of JavaScript*)
(*
let _ = 
  (* 16-byte string *)
  let string_to_encrypt = "ABCDEFGHIJKLMNOP" in 
  let state = get_state string_to_encrypt in
  (* inverses *)
  assert (state = inv_sub_bytes (sub_bytes (state)));
  assert (state = inv_shift_rows (shift_rows state));
  assert (state = inv_mix_columns (mix_columns state));
  let key = "2b7e151628aed2a6abf7158809cf4f3c" in
  assert (state = add_round_key (add_round_key state 1 key) 1 key)
;;
*)

(*
let key = "2b7e151628aed2a6abf7158809cf4f3c";;
let manywords = expanded_key key;;
let wrd1 = manywords.(0);;
*)
let state_with_chars state = 
  let out = Array.map (fun x -> Array.map (fun y -> Char.chr (int_of_string y)) x) state in out
;;

let state_to_string state = 
  let state2 = new_state () in 
  for i=0 to 3 do (
    state2.(i) <- getCol state i
  ) done;
  let a = (state_with_chars state2) in 
implode (Array.to_list (Array.append (Array.append (a.(0)) (a.(1))) (Array.append (a.(2)) (a.(3)))))
;;

let encrypt (s:string) (k:string) : string  = 
  let state = get_state s in
  let r0 = add_round_key state 0 k in
  let r1 = add_round_key(mix_columns(shift_rows(sub_bytes(r0)))) 1 k in
  let r2 = add_round_key(mix_columns(shift_rows(sub_bytes(r1)))) 2 k in
  let r3 = add_round_key(mix_columns(shift_rows(sub_bytes(r2)))) 3 k in
  let r4 = add_round_key(mix_columns(shift_rows(sub_bytes(r3)))) 4 k in
  let r5 = add_round_key(mix_columns(shift_rows(sub_bytes(r4)))) 5 k in
  let r6 = add_round_key(mix_columns(shift_rows(sub_bytes(r5)))) 6 k in
  let r7 = add_round_key(mix_columns(shift_rows(sub_bytes(r6)))) 7 k in
  let r8 = add_round_key(mix_columns(shift_rows(sub_bytes(r7)))) 8 k in
  let r9 = add_round_key(mix_columns(shift_rows(sub_bytes(r8)))) 9 k in
  let r10 = add_round_key(shift_rows(sub_bytes(r9))) 10 k in
  state_to_string r10
;;

(*
let r0 = add_round_key state 0 k;;
let r0a = shift_rows(sub_bytes(r0));;
let r0b = mix_columns r0a;;
let r1 = add_round_key(mix_columns(shift_rows(sub_bytes(r0)))) 1 k;;
*)

let decrypt (s:string) (key:string) : string = 
  let state = get_state s in
  let r0 = add_round_key state 10 key in 
  let r1 = (add_round_key (inv_sub_bytes (inv_shift_rows (r0))) 9 key) in
  let r2 = (add_round_key (inv_sub_bytes (inv_shift_rows (inv_mix_columns r1))) 8 key) in
  let r3 = (add_round_key (inv_sub_bytes (inv_shift_rows (inv_mix_columns r2))) 7 key) in 
  let r4 = (add_round_key (inv_sub_bytes (inv_shift_rows (inv_mix_columns r3))) 6 key) in
  let r5 = (add_round_key (inv_sub_bytes (inv_shift_rows (inv_mix_columns r4))) 5 key) in 
  let r6 = (add_round_key (inv_sub_bytes (inv_shift_rows (inv_mix_columns r5))) 4 key) in 
  let r7 = (add_round_key (inv_sub_bytes (inv_shift_rows (inv_mix_columns r6))) 3 key) in 
  let r8 = (add_round_key (inv_sub_bytes (inv_shift_rows (inv_mix_columns r7))) 2 key) in 
  let r9 = (add_round_key (inv_sub_bytes (inv_shift_rows (inv_mix_columns r8))) 1 key) in 
  let r10 = (add_round_key (inv_sub_bytes (inv_shift_rows (inv_mix_columns r9))) 0 key) in 
  let padded_s =  state_to_string r10 in 
  String.trim (padded_s)
;;

let helper (s:string) (k:string) (i:int) = 
  match i with 
  | 0 -> encrypt s k
  | 1 -> decrypt s k
  | _ -> failwith "enter _ _ 0 for encrypt, _ _ 1 for decrypt"
;;

Js.Unsafe.global##plop <- Js.wrap_callback helper;;

(* let govstate = [|[|"0x32";"0x88";"0x31";"0xe0"|];
		 [|"0x43";"0x5a";"0x31";"0x37"|];
		 [|"0xf6";"0x30";"0x98";"0x07"|];
		 [|"0xa8";"0x8d";"0xa2";"0x34"|]|]
;;
*)
(*
let s = "hello";;
let state = get_state s;;
(*state_to_string state;;*)
let k = "2b7e151628aed2a6abf7158809cf4f3c";;
let e = encrypt s k;;
decrypt e k;; *)


(* LEFTOVER FROM DEBUGGING
let r0 = add_round_key state 10 key;;
let r1 = (add_round_key (inv_sub_bytes (inv_shift_rows (r0))) 9 key);;
let r2 = (add_round_key (inv_sub_bytes (inv_shift_rows (inv_mix_columns r1))) 8 key);;
let r3 = (add_round_key (inv_sub_bytes (inv_shift_rows (inv_mix_columns r2))) 7 key);; (* ERROR HERE on diagonal *)
let r4 = (add_round_key (inv_sub_bytes (inv_shift_rows (inv_mix_columns r3))) 6 key);; 
let r5 = (add_round_key (inv_sub_bytes (inv_shift_rows (inv_mix_columns r4))) 5 key);;
let r6 = (add_round_key (inv_sub_bytes (inv_shift_rows (inv_mix_columns r5))) 4 key);;
let r7 = (add_round_key (inv_sub_bytes (inv_shift_rows (inv_mix_columns r6))) 3 key);;
let r8 = (add_round_key (inv_sub_bytes (inv_shift_rows (inv_mix_columns r7))) 2 key);;
let r9 = (add_round_key (inv_sub_bytes (inv_shift_rows (inv_mix_columns r8))) 1 key);;
*)

(* debugging from gov doc *)
(*
let showwrd num = 
  let a = new_state() in 
  for i=0 to 3 do (
    a.(i) <- (Array.map (fun x -> int_to_hex_str x) ((int_round_keys (expanded_key key)).(4*num+i)));
  ) done;
  a
;;
*)
