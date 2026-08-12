let () =
  Deriver.register ();
  Ppxlib.Reserved_namespaces.reserve "observe";
  Value_extension.register ()
