[@@@warning "-69"]
[@@@warning "-32"]
[@@@warning "-26"]
[@@@warning "-27"]

open Nottui
module W = Nottui_widgets
module A = Notty.A

let ( >> ) f g s = f s |> g
let groups_per_page = 8
let grey = A.gray 5

module List = struct
  include List

  let rec intersperse sep ls =
    match ls with
    | [] | [ _ ] -> ls
    | hd :: tl -> hd :: sep :: intersperse sep tl
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
  let sort_groups = List.sort (fun x1 x2 -> String.compare x1.group x2.group)
  let input = many (line <* end_of_line)

  let parse =
    parse_string ~consume:All input
    >> Result.map make_processes
    >> Result.map (List.map get_executable)
    >> Result.map (List.filter bin_with_path)
    >> Result.map (List.map get_binary)
    >> Result.map sort_processes >> Result.map group_processes
    >> Result.map sort_groups
end

module App = struct
  type app = {
    search_term : string option;
    groups : grouped list;
    active_idx : int;
    curr_page : int;
    total_pages : int;
  }

  let init groups =
    Lwd.var
      {
        search_term = None;
        groups;
        active_idx = 0;
        curr_page = 1;
        total_pages = List.length groups / groups_per_page;
      }

  let start_index { curr_page; _ } = (curr_page - 1) * groups_per_page
  let end_index { curr_page; _ } = curr_page * groups_per_page

  let on_page app =
    app.groups
    |> List.filteri (fun i _ -> i >= start_index app && i < end_index app)

  let render_logo = W.string ~attr:A.(fg red) Logo.logo |> Lwd.return

  let render_group g active =
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
      |> List.intersperse (W.printf ~attr:A.(fg grey) "•" |> Lwd.return)
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

  let render_page app =
    W.hbox
      [
        W.string ~attr:A.(fg white) " Page " |> Lwd.return;
        W.string ~attr:A.(fg green ++ st bold) (string_of_int app.curr_page)
        |> Lwd.return;
        W.string ~attr:A.(fg white) " of " |> Lwd.return;
        W.string ~attr:A.(fg green ++ st bold) (string_of_int app.total_pages)
        |> Lwd.return;
        W.string ~attr:A.(fg white) "." |> Lwd.return;
      ]

  let render app =
    W.vbox
      [
        Ui.space 0 1 |> Lwd.return;
        render_logo;
        Ui.space 0 2 |> Lwd.return;
        render_groups (on_page app) app.active_idx;
        Ui.space 0 2 |> Lwd.return;
        render_page app;
      ]

  let move_up app =
    let old_app = Lwd.peek app in
    let new_app = { old_app with active_idx = old_app.active_idx - 1 } in
    let new_app =
      if new_app.active_idx < 0 then
        {
          new_app with
          active_idx = groups_per_page - 1;
          curr_page = new_app.curr_page - 1;
        }
      else new_app
    in
    let new_app =
      if new_app.curr_page == 0 then
        { new_app with curr_page = new_app.total_pages }
      else new_app
    in
    Lwd.set app new_app

  let move_down app =
    let old_app = Lwd.peek app in
    let new_app = { old_app with active_idx = old_app.active_idx + 1 } in
    let new_app =
      if new_app.active_idx == groups_per_page then
        { new_app with active_idx = 0; curr_page = new_app.curr_page + 1 }
      else new_app
    in
    let new_app =
      if new_app.curr_page > new_app.total_pages then
        { new_app with curr_page = 1 }
      else new_app
    in
    Lwd.set app new_app

  let kill_group app = ()

  let action_handler app = function
    | `Arrow `Up, _ ->
        move_up app;
        `Handled
    | `Arrow `Down, _ ->
        move_down app;
        `Handled
    | `Enter, _ ->
        kill_group app;
        `Handled
    | _ -> `Unhandled

  let ui app =
    Lwd.get app |> Lwd.map ~f:render |> Lwd.join
    |> Lwd.map ~f:(Ui.keyboard_area (action_handler app))

  let run app = Ui_loop.run (ui app)
end

let () =
  match
    Feather.process "ps" [ "-eo"; "pid,user,cmd"; "--no-headers" ]
    |> Feather.collect Feather.stdout
    |> (fun s -> s ^ "\n")
    |> Parser.parse
  with
  | Ok procs -> procs |> App.init |> App.run
  | Error err -> Printf.printf "Error: %s\n" err
