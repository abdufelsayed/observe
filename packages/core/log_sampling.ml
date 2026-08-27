module Rate = struct
  type t = float
  type error = Not_finite | Out_of_range

  exception Invalid_rate of error

  let never = 0.
  let always = 100.

  let percent value =
    if not (Float.is_finite value) then Error Not_finite
    else if value < 0. || value > 100. then Error Out_of_range
    else Ok value

  let percent_exn value =
    match percent value with
    | Ok rate -> rate
    | Error error -> raise (Invalid_rate error)

  let to_percent rate = rate

  let pp_error formatter = function
    | Not_finite -> Format.pp_print_string formatter "must be finite"
    | Out_of_range ->
        Format.pp_print_string formatter "must be between 0 and 100 inclusive"
end

type stability = Independent | Correlation_stable

type t = {
  debug : Rate.t;
  info : Rate.t;
  warn : Rate.t;
  error : Rate.t;
  stability : stability;
}

let create ?(debug = Rate.always) ?(info = Rate.always) ?(warn = Rate.always)
    ?(error = Rate.always) ?(stability = Independent) () =
  { debug; info; warn; error; stability }

let rate t = function
  | Level.Debug -> t.debug
  | Level.Info -> t.info
  | Level.Warn -> t.warn
  | Level.Error -> t.error

let stability t = t.stability

let is_inert t =
  t.debug = Rate.always
  && t.info = Rate.always
  && t.warn = Rate.always
  && t.error = Rate.always

let requires_draw t =
  let fractional rate = rate > Rate.never && rate < Rate.always in
  fractional t.debug
  || fractional t.info
  || fractional t.warn
  || fractional t.error

module Internal = struct
  let fraction rate = Rate.to_percent rate /. 100.
end
