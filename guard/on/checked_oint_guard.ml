type 'a t = Obj.t * 'a

let wrap x = Obj.new_block Obj.abstract_tag 1, x

let unwrap (_guard, x) = x
