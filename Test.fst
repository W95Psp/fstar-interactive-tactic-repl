module Test

open FStar.Tactics
open FStar.Tactics.Builtins
module S = FStar.String
module L = FStar.List.Tot

open Main

let test
  = assert (L.fold_left (+) 0 [1;2;3;4] == 10) by (
    // let _ = tcut (`(3 == 3)) in
    interact ();
    ()
  )

