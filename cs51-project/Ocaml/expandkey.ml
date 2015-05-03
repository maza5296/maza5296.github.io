open Array;;

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
let _ = 
   assert(int_to_bin 1 = "00000001");
   assert(int_to_bin 0 = "00000000");
   assert(int_to_bin 9 = "00001001");
   assert(int_to_bin 255 = "11111111");
   assert(int_to_bin 150 = "10010110")
 ;; 

  (* assume our key is passed in in hex, we need to convert to binary, then
   * convert binary to bit array *)
  let hex_to_int (str:string) : int = 
    Scanf.sscanf str "%x" (fun x -> x);;

  (* testing hex_to_int *)
  let _ = 
    assert(hex_to_int "FF" = 255);
    assert(hex_to_int "0F" = 15);
    assert(hex_to_int "01" = 1);
    assert(hex_to_int "15" = 21);
    assert(hex_to_int "00" = 0)
  ;;

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

  let _ =
    assert(int_to_byte 1 = [|Zero;Zero;Zero;Zero;Zero;Zero;Zero;One|]);
    assert(int_to_byte 2 = [|Zero;Zero;Zero;Zero;Zero;Zero;One;Zero|]);
    assert(int_to_byte 3 = [|Zero;Zero;Zero;Zero;Zero;Zero;One;One|]);
    assert(int_to_byte 255 = [|One;One;One;One;One;One;One;One|]);
    assert(int_to_byte 0 = [|Zero;Zero;Zero;Zero;Zero;Zero;Zero;Zero|])
  ;;


  (* with our helpers, we can make this next one easy. It presumes/invariant is
   * that you pass in only a valid hexadecimal pair, so it's hidden in
   * the abstraction barrier *)
  let hex_to_byte (str:string) : byte_t = 
    int_to_byte (hex_to_int str);;

  let _ = 
    assert(hex_to_byte "FF" = [|One;One;One;One;One;One;One;One|]);
    assert(hex_to_byte "00" = [|Zero;Zero;Zero;Zero;Zero;Zero;Zero;Zero|]);
    assert(hex_to_byte "03" = [|Zero;Zero;Zero;Zero;Zero;Zero;One;One|])
  ;;

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
  let _ = 
     assert (first_four.(0).(0) = test_key.(0));
     assert (first_four.(0).(1) = test_key.(1));
     assert (first_four.(3).(2) = test_key.(14))
  ;; 

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
  let _ = 
    assert(first_four.(0).(0) = (rot_word first_four.(0)).(3));
    assert(first_four.(0).(2) = (rot_word first_four.(0)).(1));
    assert(first_four.(0).(1) = (rot_word first_four.(0)).(0));
    assert(first_four.(1).(3) = (rot_word first_four.(1)).(2))
  ;; 
    
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
  let _ = 
    assert(get_sub_value "19" = "d4");
    assert(get_sub_value "FF" = "16");
    assert(get_sub_value "E5" = "d9");
    assert(get_sub_value "2C" = "71")
  ;;
  

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
 let _ = 
   assert(byte_to_bin [|Zero;Zero;Zero;Zero;Zero;Zero;Zero;Zero;|] ="00000000");
   assert(byte_to_bin [|Zero;Zero;Zero;Zero;Zero;Zero;Zero;One;|] = "00000001");
   assert(byte_to_bin [|Zero;Zero;Zero;Zero;Zero;One;Zero;Zero;|] = "00000100");
   assert(byte_to_bin [|One;Zero;One;Zero;One;Zero;One;Zero;|] = "10101010");
   assert(byte_to_bin [|One;One;One;One;One;One;One;One|] = "11111111")
 ;; 

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
 let _ = 
   assert(byte_to_hex [|Zero;Zero;Zero;Zero;Zero;Zero;Zero;Zero;|] = "00");
   assert(byte_to_hex [|Zero;Zero;Zero;Zero;Zero;Zero;Zero;One;|] = "01");
   assert(byte_to_hex [|One;One;One;One;Zero;One;Zero;Zero;|] = "F4");
   assert(byte_to_hex [|One;One;Zero;Zero;One;Zero;One;One;|] = "CB");
   assert(byte_to_hex [|Zero;One;One;Zero;One;One;Zero;One;|] = "6D")
 ;; 

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
 let _ = 
   assert(hex_to_words "8a84eb01" = sub_word (hex_to_words "cf4f3c09"));
   assert(hex_to_words "50386be5" = sub_word (hex_to_words "6c76052a"));
   assert(hex_to_words "2b9563b9" = sub_word (hex_to_words "0bad00db"))
 ;;

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
 let _ = 
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
 ;;


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
 let _ = 
   assert(round_constant 0 = hex_to_words "01000000");
   assert(round_constant 5 = hex_to_words "20000000");
   assert(round_constant 13 = hex_to_words "4d000000")
  ;; 

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

 let _ = print_loop 0;;

end 

let expand_the_key (s:string) : word array =  (KeyExpand.expanded_key s);;
