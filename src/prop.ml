(* 命題論理式の型を定義 *) //

module type Prop_type = sig

  type 'a prop = TRUE
               | FALSE
               | PVAR of 'a
               | NOT of 'a prop
               | AND of 'a prop * 'a prop
               | ANDs of 'a prop list
               | OR of 'a prop * 'a prop
               | ORs of 'a prop list
               | IMP of 'a prop * 'a prop (* P -> Q *)
               | IFF of 'a prop * 'a prop (* P ⇔ Q *)
               | XOR of 'a prop * 'a prop
               | ATMOST1 of 'a prop list
               | UNIQUE of 'a prop list
end;;

module Prop : Prop_type = struct

  type 'a prop = TRUE
               | FALSE
               | PVAR of 'a
               | NOT of 'a prop
               | AND of 'a prop * 'a prop
               | ANDs of 'a prop list
               | OR of 'a prop * 'a prop
               | ORs of 'a prop list
               | IMP of 'a prop * 'a prop (* P -> Q *)
               | IFF of 'a prop * 'a prop (* P ⇔ Q *)
               | XOR of 'a prop * 'a prop
               | ATMOST1 of 'a prop list
               | UNIQUE of 'a prop list;;
end;;
