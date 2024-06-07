[@@@warning "-69"]
[@@@warning "-32"]

module W = Nottui_widgets
module A = Notty.A

let ( >> ) f g s = f s |> g

type process = { pid : int; user : string; command : string }
type grouped = { group : string; processes : process list }

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

  let make_process (pid, user, command) = { pid; user; command }
  let make_processes = List.map make_process

  let sort_processes =
    List.sort (fun x1 x2 -> String.compare x1.command x2.command)

  let group_processes =
    List.fold_left
      (fun acc proc ->
        match acc with
        | { group; processes } :: tl when String.equal group proc.command ->
            { group; processes = proc :: processes } :: tl
        | acc -> { group = proc.command; processes = [ proc ] } :: acc)
      []

  let get_executable proc =
    { proc with command = proc.command |> String.split_on_char ' ' |> List.hd }

  let get_binary proc =
    {
      proc with
      command = proc.command |> String.split_on_char '/' |> List.rev |> List.hd;
    }

  let input = many (line <* end_of_line)

  let parse =
    parse_string ~consume:All input
    >> Result.map make_processes
    >> Result.map (List.map get_executable)
    >> Result.map (List.map get_binary)
    >> Result.map sort_processes >> Result.map group_processes
end

(*module App = struct
    type app = {
      search_term: string option
    }
  end*)

let () =
  match
    Feather.process "ps" [ "-eo"; "pid,user,cmd"; "--no-headers" ]
    |> Feather.collect Feather.stdout
    |> (fun s -> s ^ "\n")
    |> Parser.parse
  with
  | Ok procs ->
      procs
      |> List.iter (fun { group; processes } ->
             Printf.printf "group=[%s] processes=[%d]\n" group
               (List.length processes))
  | Error err -> Printf.printf "Error: %s\n" err
