type roles

type 'error t
(** A caller-owned interpretation of an error value. *)

val roles :
  ?kind:string ->
  ?code:string ->
  ?message:string ->
  ?explanation:string ->
  ?remediation:string ->
  ?documentation:string ->
  unit ->
  roles

val create : ('error -> roles) -> 'error t
(** Build a reusable interpreter without converting the domain error to a
    package-owned exception or error type. *)

val exn : exn t
(** Interpret an explicitly supplied exception by its constructor name and
    printable message. Supply a captured raw backtrace separately at the
    contribution site when one is available. *)

val value : 'error t -> ?backtrace:Printexc.raw_backtrace -> 'error -> Value.t

val freeze :
  'error t ->
  ?backtrace:Printexc.raw_backtrace ->
  'error ->
  (Snapshot.t, Snapshot.error) result
