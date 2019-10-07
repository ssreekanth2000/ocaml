let get_1_3 (a,_,_) = a
let get_3_3 (_,_,a) = a

let step (m:'a tm) (c:'a config):'a config =
  let state1 = get_1_3 (m.delta (c.state, List.nth c.tape c.position)) in
  let tape1= if (c.position +1) < (List.length c.tape ) then  c.tape else c.tape @ [m.blank] in
  let position1 = c.position + get_3_3(m.delta (c.state, List.nth c.tape c.position)) in
{state = state1;
 tape = tape1;
position = position1};;


let rejectConfig (m:'a tm) (c:'a config):bool =
  if get_1_3 (asbs.delta (c.state, List.nth c.tape c.position)) = "rej" then true else false;;


let run (m:string tm) (w:string):bool =
  let rec lo t c =
    if acceptConfig t c then true else if rejectConfig t c then false else lo t (step m c)
  in
  lo m (startConfig m w)
