(** Validated probabilistic base sampling for logs. *)

module Rate : sig
  type t
  type error = Not_finite | Out_of_range

  exception Invalid_rate of error

  val never : t
  val always : t

  val percent : float -> (t, error) result
  (** Construct a percentage in the closed interval from zero to 100. *)

  val percent_exn : float -> t
  (** Like {!percent}, but raises [Invalid_rate]. *)

  val to_percent : t -> float
  val pp_error : Format.formatter -> error -> unit
end

type stability = Independent | Correlation_stable
type t

val create :
  ?debug:Rate.t ->
  ?info:Rate.t ->
  ?warn:Rate.t ->
  ?error:Rate.t ->
  ?stability:stability ->
  unit ->
  t
(** Build per-level base sampling. Omitted rates retain every log. Errors are
    therefore always retained unless [error] is explicitly supplied. Exact zero
    and 100 percent decisions consume no runtime draw. *)

val rate : t -> Level.t -> Rate.t
val stability : t -> stability
val is_inert : t -> bool

val requires_draw : t -> bool
(** Whether at least one configured level needs runtime randomness. Exact zero
    and 100 percent rates require no draw. *)

module Internal : sig
  val fraction : Rate.t -> float
end
