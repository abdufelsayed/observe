let () =
  Deriver.register ();
  Ppxlib.Reserved_namespaces.reserve "observe";
  Ppxlib.Driver.register_transformation "observe"
    ~rules:(Value_extension.rules @ Log_extension.rules)
