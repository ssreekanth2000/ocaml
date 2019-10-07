
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


let rec hasFinal (m:'a fa) (qs:'a list):bool =
  match qs with
  | [] -> true
  | q :: qs' -> if List.mem q (m.final) then hasFinal m qs' else false;;
