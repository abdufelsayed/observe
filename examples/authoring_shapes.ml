type event =
  | User_login of { user_id : int; method_ : string }
  | Signed_out of int
[@@deriving observe]

let () =
  ignore (Observe.Logs.text ~tag:"auth" "user logged in");
  ignore
    (Observe.Logs.text_lazy ~tag:"query" (fun () ->
         "query plan constructed only after admission"));
  ignore
    (Observe.Logs.free
       [%observe.value
         {
           action = "user_login";
           user_id = 42;
           metadata = [%observe.value.embed Observe.Type.string, "example"];
         }]);
  ignore
    (Observe.Logs.structured event_t
       (User_login { user_id = 42; method_ = "oauth" }))
