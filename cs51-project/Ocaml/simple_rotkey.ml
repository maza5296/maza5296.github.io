let caesar (str:string) (key:int) = 
  let cae_char (c:char) : char = 
    char_of_int ((int_of_char c) + key)
  in 
  String.map cae_char str
;;

(* let decaesar str key = 
  let cae_char (c:char) : char = 
    char_of_int ((int_of_char c) - key)
  in 
  String.map cae_char str
;; *)

Js.Unsafe.global##plop <- Js.wrap_callback caesar;;
(* decaesar (caesar "Juaquin" 76) 76;;     *)
