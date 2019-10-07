(*

HOMEWORK 4

Due: Wen Oct 9, 2019 (23h59)

Name:

Email:

Remarks, if any:

*)



(*
 * Type for deterministic Turing machines
 *
 * Parameterized by type for states
 *)

type symbol = string

type 'a tm = { states : 'a list;
               input_alphabet : symbol list;
               tape_alphabet : symbol list;
               left_marker : symbol;
               blank : symbol;
               delta : ('a * symbol) -> ('a * symbol * int);   (* 0 = Left, 1 = Right *)
               start : 'a;
               accept : 'a;
               reject : 'a }

type 'a config = { state : 'a;
                   tape: symbol list;
                   position: int }


(*
 * Some sample deterministic Turing machines
 *
 * asbs is the regular language {a^m b^n | m,n >= 0}
 * anbn is the non-regular language {a^n b^n | n >= 0}
 * anbncn is the non-regular language {a^n b^n c^n | n >= 0}
 *
 *)

let asbs =
  let d inp = (match inp with
      | ("start", "a") -> ("start", "a", 1)
      | ("start", "b") -> ("q1", "b", 1)
      | ("start", ">") -> ("start", ">", 1)
      | ("start", "_") -> ("acc", "_", 1)
      | ("q1", "b") -> ("q1", "b", 1)
      | ("q1", "_") -> ("acc", "_", 1)
      | ("acc", "a") -> ("acc", "a", 1)
      | ("acc", "b") -> ("acc", "b", 1)
      | ("acc", ">") -> ("acc", ">", 1)
      | ("acc", "_") -> ("acc", "_", 1)
      | (_,c) -> ("rej",c,1))
  in { states = ["start"; "q1"; "acc"; "rej"];
       input_alphabet = ["a";"b"];
       tape_alphabet = ["a";"b";"_";">"];
       blank = "_";
       left_marker = ">";
       start = "start";
       accept = "acc";
       reject = "rej";
       delta = d }

let anbn =
  let d inp = (match inp with
      | ("start", "a") -> ("start", "a", 1)
      | ("start", "b") -> ("q1", "b", 1)
      | ("start", "|") -> ("start", "|", 1)
      | ("start", "/") -> ("q2", "/", 1)
      | ("q1", "b") -> ("q1", "b", 1)
      | ("q1", "/") -> ("q2", "/", 1)
      | ("q2", "|") -> ("q3", "|", 1)
      | ("q2", "a") -> ("q2", "a", 0)
      | ("q2", "b") -> ("q2", "b", 0)
      | ("q2", "X") -> ("q2", "X", 0)
      | ("q2", "/") -> ("q2", "/", 0)
      | ("q3", "X") -> ("q3", "X", 1)
      | ("q3", "/") -> ("acc", "/", 1)
      | ("q3", "a") -> ("q4", "X", 1)
      | ("q4", "a") -> ("q4", "a", 1)
      | ("q4", "X") -> ("q4", "X", 1)
      | ("q4", "b") -> ("q2", "X", 1)
      | ("acc", "a") -> ("acc", "a", 1)
      | ("acc", "b") -> ("acc", "b", 1)
      | ("acc", "|") -> ("acc", "|", 1)
      | ("acc", "X") -> ("acc", "X", 1)
      | ("acc", "/") -> ("acc", "/", 1)
      | (_,c) -> ("rej",c,1))
  in { states = ["start"; "q1"; "q2"; "q3"; "q4"; "acc"; "rej"];
       input_alphabet = ["a";"b"];
       tape_alphabet = ["a";"b";"X";"/";"|"];
       blank = "/";
       left_marker = "|";
       start = "start";
       accept = "acc";
       reject = "rej";
       delta = d }


let anbncn =
  let d inp = (match inp with
      | ("start", "a") -> ("start", "a", 1)
      | ("start", "b") -> ("q1", "b", 1)
      | ("start", "c") -> ("q6", "c", 1)
      | ("start", ">") -> ("start", ">", 1)
      | ("start", "_") -> ("q2", "_", 1)
      | ("q1", "b") -> ("q1", "b", 1)
      | ("q1", "c") -> ("q6", "c", 1)
      | ("q1", "_") -> ("q2", "_", 1)
      | ("q2", ">") -> ("q3", ">", 1)
      | ("q2", "a") -> ("q2", "a", 0)
      | ("q2", "b") -> ("q2", "b", 0)
      | ("q2", "c") -> ("q2", "c", 0)
      | ("q2", "_") -> ("q2", "_", 0)
      | ("q2", "X") -> ("q2", "X", 0)
      | ("q3", "X") -> ("q3", "X", 1)
      | ("q3", "_") -> ("acc", "_", 1)
      | ("q3", "a") -> ("q4", "X", 1)
      | ("q4", "a") -> ("q4", "a", 1)
      | ("q4", "X") -> ("q4", "X", 1)
      | ("q4", "b") -> ("q5", "X", 1)
      | ("q5", "b") -> ("q5", "b", 1)
      | ("q5", "X") -> ("q5", "X", 1)
      | ("q5", "c") -> ("q2", "X", 1)
      | ("q6", "c") -> ("q6", "c", 1)
      | ("q6", "_") -> ("q2", "_", 1)
      | ("acc", "a") -> ("acc", "a", 1)
      | ("acc", "b") -> ("acc", "b", 1)
      | ("acc", "c") -> ("acc", "c", 1)
      | ("acc", ">") -> ("acc", ">", 1)
      | ("acc", "X") -> ("acc", "X", 1)
      | ("acc", "_") -> ("acc", "_", 1)
      | (_,c) -> ("rej", c,1))
  in { states = ["start";"q1";"q2";"q3";"q4";"q5";"q6";"acc";"rej"];
       input_alphabet = ["a";"b";"c"];
       tape_alphabet = ["a";"b";"c";"X";"_";">"];
       blank = "_";
       left_marker = ">";
       start = "start";
       accept = "acc";
       reject = "rej";
       delta = d }



(*
 * Helper functions
 *
 *   explode : string -> string list
 *      returns the list of symbols making up a string
 *
 *   printConfig: string tm -> string config -> 'a -> 'a
 *      print a configuration (including newline) to standard output
 *      and return a value
 *
 *)

let explode (str:string):symbol list =
  let rec acc index result =
    if (index<0) then result
    else acc (index-1) ((String.sub str index 1)::result) in
  acc (String.length(str)-1) []


let printConfig (m:string tm) (c:string config):unit =
  let mw = List.fold_right (fun a r -> max (String.length a) r) m.states 0 in
  let padding = max 0 (c.position + 1 - List.length c.tape) in
  let rec mkBlank k = match k with 0 -> [] | _ -> m.blank :: (mkBlank (k -1)) in
  let tape' = c.tape@(mkBlank padding) in
  let _ = print_string (String.sub (c.state^(String.make mw ' ')) 0 mw) in
  let _ = print_string "  "  in
  let _ = List.iteri (fun i sym ->
      if (i=c.position) then Printf.printf "[%s]" sym
      else Printf.printf " %s " sym) tape'  in
  print_newline ()




(* QUESTION 1 *)


let startConfig (m:'a tm) (w:string):'a config =
  let state1 = (asbs.start) in
  let tape1= ([m.left_marker]@ explode w ) in
  let position1 = 0 in
  {state = state1;
   tape = tape1;
   position = position1};;

   let get_1_3 (a,_,_) = a
   let get_3_3 (_,_,a) = a

let acceptConfig (m:'a tm) (c:'a config):bool =
  if get_1_3 (asbs.delta (c.state, List.nth c.tape c.position)) = "acc" then true else false;;


let rejectConfig (m:'a tm) (c:'a config):bool =
  if get_1_3 (asbs.delta (c.state, List.nth c.tape c.position)) = "rej" then true else false;;

let rec replace_nth (lst:'a list) (n:int) (s:'a):'a list =
  List.mapi (fun i x -> if i = n then s else x) lst;;


let get_1_3 (a,_,_) = a

let get_3_3 (_,_,a) = a
let step (m:'a tm) (c:'a config):'a config =
  let state1 = get_1_3 (m.delta (c.state, List.nth c.tape c.position)) in
  let tape1= if (c.position +1) < (List.length c.tape ) then  c.tape else c.tape @ [m.blank] in
  let position1 = c.position + get_3_3(m.delta (c.state, List.nth c.tape c.position)) in
  {state = state1;
   tape = tape1;
   position = position1};;


let run (m:string tm) (w:string):bool =
  failwith "Function run not implemented"





    let rejectConfig (m:'a tm) (c:'a config):bool =
      if List.mem "rej"  (asbs.delta (c.state, Listc.tape c.position)) then true else false

      let rec replace_nth (lst:'a list) (n:int) (s:'a):'a list =
        match lst with
        | [] -> []
        | x::xs'-> if
