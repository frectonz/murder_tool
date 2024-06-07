module Parser = struct
  open Angstrom

  let pid =
    take_while1 (function '0' .. '9' -> true | _ -> false) >>| int_of_string

  let user =
    take_while1 (function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false)

  let command = take_while (function '\n' -> false | _ -> true)
  let spaces = skip_while (function ' ' -> true | _ -> false)

  let line =
    let* _ = spaces in
    let* pid = pid in
    let* _ = spaces in
    let* user = user in
    let* _ = spaces in
    let* command = command in
    return (pid, user, command)

  let input = many (line <* end_of_line)
  let parse s = parse_string ~consume:All input s
end

let () =
  match
    Feather.process "ps" [ "-eo"; "pid,user,cmd"; "--no-headers"; "-ww" ]
    |> Feather.collect Feather.stdout
    |> (fun s -> s ^ "\n")
    |> Parser.parse
  with
  | Ok procs ->
      List.iter
        (fun (pid, user, command) ->
          Printf.printf "pid=[%d] user=[%s] commad=[%s]\n" pid user command)
        procs
  | Error err -> Printf.printf "Error: %s\n" err
