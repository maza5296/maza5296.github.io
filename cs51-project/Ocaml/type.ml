type bit = Zero | One

type byte_t = bit array

type block = byte_t array

type state = byte_t array 

(* a word is 4 bytes *)
type word = bit array

(*
The type ‘bit’ is defined as either a Zero of Int or One of Int; during implementing the functionality, we will match Zero and One so as to store the bit as 0 or 1 in the final byte.

The type byte_t is defined as a collection of 8 bits data type.

The type block is defined as a collection of 8 bytes_t data type.

(* Please see: We realize that this way of representing bytes and bits is bad style and would require comprehensive matching during implementation; however, after extensive research on the internet, we could not figure out a more streamlined, elegant approach. We even approached Allison during Office Hours, and she told us to submit this in the specs, and get pointers for better representational style from our TF. 
Some resources we consulted:
http://caml.inria.fr/pub/old_caml_site/FAQ/FAQ_EXPERT-eng.html#semantique
http://caml.inria.fr/pub/docs/manual-ocaml/libref/Bytes.html 
http://caml.inria.fr/pub/docs/manual-ocaml/libref/String.html
http://caml.inria.fr/pub/docs/manual-ocaml/libref/Pervasives.html *)
*)
