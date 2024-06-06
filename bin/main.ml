let () =
  Feather.process "ps" [ "-e" ]
  |> Feather.collect Feather.stdout
  |> print_endline
