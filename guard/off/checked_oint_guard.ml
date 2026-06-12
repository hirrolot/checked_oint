type 'a t = 'a

external wrap : 'a -> 'a t = "%identity"

external unwrap : 'a t -> 'a = "%identity"
