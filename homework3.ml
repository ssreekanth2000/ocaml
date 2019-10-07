(*

HOMEWORK 3

Due: Fri Sep 27, 2019 (23h59)

Name:

Email:

Remarks, if any:

*)


(*
 * String <-> characters utility functions:
 *
 *   explode : string -> char list
 *      returns the list of characters making up a string
 *
 *   implode : char list -> string
 *      concatenates the list of characters into a string
 *
 *)

let explode (str) =
  let rec acc (index,result) =
    if (index<0) then
      result
    else
      acc(index-1, (String.get str index)::result)
  in
    acc(String.length(str)-1, [])

let implode cs =
  List.fold_right (fun a r -> (String.make 1 a)^r) cs ""



(*
 *  The type of a finite automaton
 *
 *)

type 'a fa = { states: 'a list;
               alphabet: char list;
               delta: ('a * char * 'a) list;
               start : 'a;
               final : 'a list }


(*
 * Sample FAs
 *
 * The first accepts the language of all strings over {x,y}
 * with a multiple-of-3 number of x's.
 *
 * The second accepts the language of all strings over {x,y,z}
 * whose last three symbols are y's.
 *
 * Notes that states can be of any type -- the first example
 * uses strings as states, while the second uses integers.
 *
 *)

let faThreeX = {
  states = ["start";"one";"two"];
  alphabet = ['x';'y'];
  delta = [ ("start",'x',"one");
	    ("one",'x',"two");
	    ("two",'x',"start");
	    ("start",'y',"start");
	    ("one",'y',"one");
	    ("two",'y',"two") ];
  start = "start";
  final = ["start"]
}

let faLastThreeY = {
  states = [0;1;2;3];
  alphabet = ['x';'y';'z'];
  delta = [ (0,'x',0);
	    (0,'y',0);
	    (0,'z',0);
	    (0,'y',1);
	    (1,'y',2);
	    (2,'y',3); ];
  start = 0;
  final = [3]
}





(* QUESTION 1 *)


let rec hasFinal (m:'a fa) (qs:'a list):bool =
  match qs with
  | [] -> false
  | q :: qs' -> if List.mem q (m.final) then true else hasFinal m qs';;


  let rec reachableStates (m:'a fa) (q:'a) (a:char):'a list =
    match m.delta with
    | []-> []
    | x :: xs' -> let rec matchtwo ar qr zr = match zr with
        | [] -> []
        | (p, t, n)::zr' -> if p = qr && t = ar then n::matchtwo ar qr zr' else matchtwo ar qr zr'
      in
      matchtwo a q m.delta;;



  let rec helpfollow (m:'a fa) qs a =
    match qs with
    | []->[]
    | q :: qs' -> reachableStates m q a :: helpfollow m qs' a;;

  let rec concatAll (xs: 'a list list): 'a list =
    let rec loop1 res = function
      | [] -> List.rev res
      | h::t -> loop1 (List.rev_append h res) t
    in
    loop1 [] xs

  let rec follow (m:'a fa) (qs:'a list) (a:char):'a list =
    concatAll (helpfollow m qs a);;


  let rec helpfollowAll (m:'a fa) qs syms =
    match syms with
    | [] -> []
    | x :: xs' -> follow m qs x :: helpfollowAll m qs xs';;


  let rec followAll (m:'a fa) (qs:'a list) (syms:char list):'a list =
    match syms with
    |  []-> qs
    | a :: syms' -> followAll m (follow m qs a ) syms' ;;





  let rec accept1 m qs input =
    match qs with
    | [] -> false
    | q :: qs' -> if List.mem q (followAll m [m.start] (explode input)) then true else accept1 m qs' input;;


    let accept (m:'a fa) (input:string):bool =
      accept1 m m.final input;;

(* QUESTION 2 *)

(* Right now, these are dummy finite automata that always rejects
   replace by your own *)

let dummy = { states = [[]];
              alphabet = [];
	      delta = [];
	      start = [];
              final = []}
let fa_q2_a : 'a fa ={
  states = ["start";"one";"two"];
  alphabet = ['x';'y';'z'];
  delta = [ ("start",'x',"one");
            ("one",'x',"two");
            ("two",'x',"start");
            ("start",'y',"one");
            ("one",'y',"two");
            ("two",'y',"start");
            ("start",'z',"one");
            ("one",'z',"two");
            ("two",'z',"start") ];
  start = "start";
  final = ["one";"two"]
}

let fa_q2_b : 'a fa = {
  states = ["start";"one";"two"];
  alphabet = ['x';'y';'z'];
  delta = [ ("start",'x',"one");
            ("one",'x',"two");
            ("start",'z',"start");
            ("one",'z',"one");
            ("two",'z',"two") ];
  start = "start";
  final = ["two"]
}




let fa_q2_c : 'a fa = {
  states = ["start";"one";"two";"three";"four";"five"];
  alphabet = ['x';'y';'z'];
  delta = [ ("start",'z',"start");
            ("start",'y',"one");
            ("one",'z',"one");
            ("one",'x',"two");
            ("two",'z',"two");
            ("start",'x',"three");
            ("three",'z',"three");
            ("three",'y',"two");
            ("three",'x',"four");
            ("four",'z',"four");
            ("four",'y',"five");
            ("two",'x',"five");
            ("five",'z',"five")];
  start = "start";
  final = ["five"]
}



let fa_q2_d : 'a fa = {
  states = ["start";"one";"two";"three";"four";];
  alphabet = ['x';'y';'z'];
  delta = [ ("start",'x',"four");
            ("four",'x',"one");
            ("four",'z',"four");
            ("two",'z',"two");
            ("one",'y',"two");
            ("two",'y',"four");
            ("four",'y',"two");
            ("two",'x',"three");
            ("start",'z',"start");
            ("three",'z',"three");
            ("three",'y',"four")
          ];
  start = "start";
  final = ["four"]

}



let fa_q2_e : 'a fa = { states = [0;1;2;3;4;5;6;7];
                        alphabet = ['x';'y';'z'];
                        delta = [(0,'y',1);
                                 (1,'y',0);
                                 (1,'x',2);
                                 (2,'x',1);
                                 (2,'y',3);
                                 (3,'y',2);
                                 (4,'y',5);
                                 (5,'y',4);
                                 (5,'x',6);
                                 (6,'x',5);
                                 (6,'y',7);
                                 (7,'y',6);
                                 (0,'x',3);
                                 (3,'x',0);
                                 (4,'x',7);
                                 (7,'x',4);
                                 (0,'z',4);
                                 (1,'z',5);
                                 (2,'z',6);
                                 (3,'z',7)
                                ];
                        start = 0;
                        final = [7]
                      }


(* QUESTION 3 *)

let fa1 = { states = ["fa1_1";"fa1_2";"fa1_3"];
	    alphabet = ['x';'y';'z'];
	    delta = [ ("fa1_1",'x',"fa1_2");
		      ("fa1_2",'x',"fa1_3");
		      ("fa1_3",'x',"fa1_1") ];
	    start = "fa1_1";
	    final = ["fa1_1"] }

let fa2 = { states = ["fa2_1";"fa2_2";"fa2_3"];
	    alphabet = ['x';'y';'z'];
	    delta = [ ("fa2_1",'y',"fa2_2");
		      ("fa2_2",'z',"fa2_3");
                      ("fa2_3",'y',"fa2_2") ];
	    start = "fa2_1";
	    final = ["fa2_3"] }


      let rec replaceStates m q b =
        match m.delta with
        | [] -> []
        | x :: xs -> let rec matchone ar qr zr = match zr with
            | [] -> []
            | (p,t,n) ::zr' -> if p = qr then (ar ,t, n ):: matchone ar qr zr' else matchone ar qr zr'
          in
          matchone b q m.delta ;;

let unionM (m1:string fa) (m2:string fa):string fa =
  let states3 = (concatAll [["fa3_1"];m1.states, m2.states]) in
  let alphabet3 = (m1.alphabet) in
  let delta3 = (concatAll [replaceStates m1 fa1_1 fa3_1; replaceStates m2 fa2_1 fa3_1; m1.delta; m2.delta])in
  let start3  = ("fa3_1") in
  let final3 = (concatAll[m1.final; m2.final;["fa3_1"]]) in
{states = states3;
 alphabet = alphabet3;
 delta = delta3;
 start = start3;
 final = final3};;

let rec replaceStates3 m q b =
  match m.delta with
  | [] -> []
  | x :: xs -> let rec matchone1 del qr zr = match del with
      | [] -> []
      | (p,t,n) ::del' ->((if p = qr then zr else p),t,(if n = qr then zr else n))::matchone1 del' qr zr
    in
    matchone1 m.delta q b;;


let concatM (m1:string fa) (m2:string fa):string fa =
 let states3 = concatAll ["fa3_1";m1.states; m2.states] in
  let alphabet3 = m1.alphabet;in
 let delta3 = concatAll (replaceStates3 m1 m1.start fa3_1; replaceStates3 m2 m2.start fa3_1; m1.delta; m2.delta);in
  let start3  = "fa3_1";in
 let final3 = concatAll[m1.final; m2.final] in
 {states = states3;
  alphabet = alphabet3;
  delta = delta3;
  start = start3;
 final = final3};;


 let rec reverseStates delta =
   match delta with
   | [] -> []
   | (p,t,n):: delta' -> (n,t,p)::reverseStates(delta');;

   let rec startfromfinal delta final =
     match delta with
     | [] -> []
     | (p,t,n):: delta' -> ((if (List.mem p final) then (List.hd final) else p),t,(if (List.mem n final) then (List.hd final)))::startfromfinal delta' final ;;

let rec startfinal m =
  match m.delta with
  | []->[]
  | x :: xs -> let rec startfromfinal delta final = match delta with
      | [] -> []
      | (p,t,n):: delta' -> ((if (List.mem p final) then (List.hd final) else p),t,(if (List.mem n final) then (List.hd final)))::startfromfinal delta' final
    in
    startfromfinal m.delta m.final



let reverseM (m1:string fa):string fa =
let states3 = m1.states in
 let alphabet3 = m1.alphabet;in
let delta3 = reverseStates ( startfromfinal m1);in
 let start3  = List.hd m1.final;in
let final3 = [m1.start] in
{states = states3;
 alphabet = alphabet3;
 delta = delta3;
 start = start3;
final = final3};;


(* This function is the base function that basically loops through all
 * strings
 * of length up to n, and prints those that are accepted by the
 * finite automaton.
 *
 * This is being way too clever to try to not blow the stack
 * while enumerating all strings up to a given length. Basically.
 * we enumerate all integer, convert them to base K (where K is the
 * size of the alphabet) and then replace every digit base K by the
 * letter of the alphabet at the corresponding index in the alphabet.
 *
 * The key is that we can enumerate integers super easily
 *
*)

let lang m n =

  let rec expt a n = if n <= 0 then 1 else a*(expt a (n-1)) in

  let rec take n default l =
    if n <= 0 then []
    else (match l with
          | [] -> default::(take (n-1) default l)
          | x::xs -> x::(take (n-1) default xs)) in

  let to_base_n base size n =
    let rec loop n =
      if n <= 0 then []
      else if n mod base = 0 then 0::(loop (n / base))
      else (n mod base)::(loop ((n - n mod base) / base))  in
    take size 0 (loop n)  in

  let to_string alphabet size n =
    let base = List.length alphabet in
    let num_base = to_base_n base size n in
    implode (List.map (fun i -> List.nth alphabet i) num_base) in

    if n < 0 then ()
    else
      let print_str s = if s = "" then print_string "  <epsilon>\n"
  	              else print_string ("  "^s^"\n")  in
      let rec loop i =
        if i <= n then
  	  let ts = to_string m.alphabet i  in
  	  let bound = expt (List.length m.alphabet) i in
  	  let rec loop2 j =
  	    if j < bound then (if accept m (ts j)
                                 then print_str (ts j)
                               else ();
  			       loop2 (j+1))
  	    else ()  in
  	  (loop2 0; loop (i+1))
        else ()  in
      loop 0
