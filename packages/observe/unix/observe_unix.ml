module Platform = struct
  type t = unit

  let now () = Clock.instant (Ptime_clock.now_d_ps ())

  let write_terminal () value =
    Write.all ~write:Unix.write_substring Unix.stderr value;
    Observe.Platform.Accepted
end
