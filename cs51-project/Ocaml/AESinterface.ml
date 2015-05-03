(* Module interface for AES *)

(* In this interface, I make the following ASSUMPTIONS:
   - the types implemented include a
       - array "block" of 128 bits/16 bytes
       - byte_t or an 8 bit array
       - bit which is a value 0 or 1
       - a state which is 4 rows of bytes, each containing block length/32 bytes
         in its array
*)

module type Encrypt = 
sig
 
  (* takes in the string to encrypt and builds it into our initial state *)
  val get_state : string -> state

  (* when applied to each of the 16 bytes of the state, it transforms the byte
   * into a new byte value *)
  val sub_bytes : byte -> byte

  (* this function probably calls in sub_bytes as a helper to actually apply
   * sub_bytes to all parts of the state *)
  val sub_bytes_state : state -> state 

  (* shift_rows shifts the last 3 rows of the state *)
  val shift_rows : state -> state

  (* performs complicated iterations over the columns of the state *)
  val mix_columns : state -> state 

  (* combines the state with the next-most previously unusued portion of the 
   * expanded key, which is a bytes array *)
  val add_round_key : state -> bytes array -> state

end 

module type Decrypt = 
sig

  (* shifts the 3 rows back, the inverse of its namesake *)
  val inv_shift_rows : state -> state

  (* this helper applies the reverse substitution to the bytes *)
  val inv_sub_bytes : byte -> byte

  (* this calls inv_sub_bytes on the correct bytes within the state *)
  val inv_sub_bytes_state : state -> state

  (* this shifts everything in the state back in a column-based method *)
  val inv_mix_columns: state -> state

  (* note that add_round_key is its own inverse, so while we define it here 
   * for clarity, we can make inv_add_round_key = Encrypt.add_round_key *)
  val inv_add_round_key : state -> bytes array -> state
end

module type KeyExpand = 
sig

  (* takes in 4 bytes and outputs them as a type word *)
  val bytes_to_word : byte_t -> byte_t -> byte_t -> byte_t -> word

  (* takes in a 4 byte input word and returns it rotated *)
  val rot_word : word -> word

  (* substitutes from the "Sbox" as our previous functions substitute *)
  val sub_word : word -> word

  (* where int is the round # we are on, and word is the returned that is then
   * XOR'ed with our key *)
  val round_constant : int -> word

  (* returns 4 bytes of the expanded key after the specified offset *)
  val exp_key_offset : int -> word 

  (* same as exp_key but uses the key bytes instead *)
  val key_offset : int -> word

end 
