exception Empty

type 'a t

val empty    : 'a t
val is_empty : 'a t -> bool
val mem      : 'a t -> 'a -> bool 
val push     : 'a t -> 'a -> 'a t
val pop      : 'a t -> 'a * 'a t

