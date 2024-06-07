[@@@warning "-69"]
[@@@warning "-32"]
[@@@warning "-26"]

open Nottui
module W = Nottui_widgets
module A = Notty.A

let ( >> ) f g s = f s |> g

module List = struct
  include List

  let intersperse x lst =
    let rec inner acc = function
      | [] -> acc
      | x1 :: [] -> inner (x1 :: x :: acc) []
      | x1 :: x2 :: tl -> inner (x :: x1 :: x :: x2 :: acc) tl
    in
    inner [] lst |> List.rev
end

type process = { pid : int; user : string; command : string }
type grouped = { group : string; processes : process list }

module Parser = struct
  open Angstrom

  let pid =
    take_while1 (function '0' .. '9' -> true | _ -> false) >>| int_of_string

  let user =
    take_while1 (function 'a' .. 'z' | 'A' .. 'Z' | '+' -> true | _ -> false)

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

  let bin_with_path proc = not (String.starts_with ~prefix:"[" proc.command)
  let input = many (line <* end_of_line)

  let parse =
    parse_string ~consume:All input
    >> Result.map make_processes
    >> Result.map (List.map get_executable)
    >> Result.map (List.filter bin_with_path)
    >> Result.map (List.map get_binary)
    >> Result.map sort_processes >> Result.map group_processes
end

module App = struct
  type app = { search_term : string option; processes : grouped list }

  let render_group g active =
    let grey = A.gray 5 in

    let title_style =
      if active then A.(bg green ++ fg black ++ st bold)
      else A.(bg grey ++ fg black)
    in
    let title = W.printf ~attr:title_style " %s " g.group |> Lwd.return in

    let divider_style = if active then A.(fg green) else A.(fg grey) in
    let divider = W.string ~attr:divider_style "| " |> Lwd.return in

    let processes_style =
      if active then A.(fg black ++ bg white) else A.(fg grey)
    in
    let processes = W.string ~attr:processes_style "PIDs" |> Lwd.return in

    let pid_style = if active then A.(fg lightwhite) else A.(fg grey) in
    let pids =
      g.processes
      |> List.map (fun p -> W.printf ~attr:pid_style "[%d]" p.pid |> Lwd.return)
      |> List.intersperse (W.printf ~attr:A.(fg grey) "" |> Lwd.return)
    in
    W.vbox
      [
        W.hbox [ divider; title ];
        W.hbox [ divider; Ui.space 0 1 |> Lwd.return ];
        W.hbox [ divider; processes; Ui.space 2 0 |> Lwd.return; W.hbox pids ];
      ]

  let render_groups groups active_idx =
    groups
    |> List.mapi (fun i g -> render_group g (i == active_idx))
    |> List.intersperse (Ui.space 0 1 |> Lwd.return)
    |> W.vbox

  let run processes =
    let app = { search_term = None; processes } |> Lwd.var in
    Ui_loop.run (render_groups processes 3)
end

let () =
  match
    Feather.process "ps" [ "-eo"; "pid,user,cmd"; "--no-headers" ]
    |> Feather.collect Feather.stdout
    |> (fun s -> s ^ "\n")
    |> Parser.parse
  with
  | Ok procs -> App.run procs
  | Error err -> Printf.printf "Error: %s\n" err
