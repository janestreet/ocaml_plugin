open Core.Std
open Async.Std

let fold_result ~f ~init:acc =
  let rec aux acc = function
    | [] -> Or_error.return acc
    | hd :: tl -> Or_error.bind (f acc hd) ~f:(fun acc -> aux acc tl)
  in
  aux acc
;;

(* extract all dependencies from ocamldep output for one compilation unit *)
let post_process ~target lines : String.Set.t Or_error.t =
  let chop_extension = Fn.compose fst Filename.split_extension in
  fold_result lines ~init:String.Set.empty ~f:(fun set line ->
    match String.split ~on:' ' line with
    | target_file :: ":" :: deps ->
      Or_error.try_with_join (fun () ->
        if String.equal target (chop_extension target_file)
        then
          List.fold deps ~init:set ~f:(fun set dep ->
            Set.add set (chop_extension dep)
          ) |> Or_error.return
        else
          Or_error.error "Unexpected target file in ocamldep output"
            (target, line)
            [%sexp_of: string * string]
      )
    | _ ->
      Or_error.error "Fail to parse ocamldep output"
        lines
        [%sexp_of: string list]
  )
;;

let%test_module _ =
  (module struct

    let lines =
      [ "a.cmx : a.cmi c.cmx d.cmx e.cmx"
      ; "a.cmo : a.cmi b.cmo c.cmo"
      ; "a.cmi : f.cmi"
      ]
    ;;

    let expect = String.Set.of_list ["a"; "b"; "c"; "d"; "e"; "f"]
    ;;

    let%test_unit _ =
      [%test_eq: String.Set.t] (post_process ~target:"a" lines |> ok_exn) expect
    ;;
  end)

let topological_sort ~visit_trace ~target ~find_direct_deps =
  (* this uses a Depth-First Search to find a topological order of compilation units and
     report circular dependencies.

     [visiting] is everything currently in call stack and is used to detect circles
     [visit_finish_order] is the order we finish visiting a node and all its descendants,
     and is a topological order

     each time we visit a node, we
     1) push it to [visiting] stack
     2) visit all its descendants which are not visited (i.e. not in [visiting] nor
     [visit_finish_order])
     3) pop it from [visiting] stack and add it to [visit_finish_order]
  *)
  let visiting = Stack.create () in
  let visit_finish_order = Queue.create () in
  let rec visit target =
    (* low number of elements, O(n) lookup is ok there *)
    if Stack.mem ~equal:String.equal visiting target
    then begin
      let circle =
        target ::
        (List.rev (List.take_while (Stack.to_list visiting)
                     ~f:(String.(<>) target)))
      in
      return (Or_error.error "Circular dependency detected"
                circle [%sexp_of: string list])
    end
    else if Queue.mem ~equal:String.equal visit_finish_order target
    then return (Ok ())
    else begin
      visit_trace target;
      Stack.push visiting target;
      find_direct_deps ~target
      >>=? fun deps ->
      Deferred.List.fold (Set.to_list deps) ~init:(Ok ()) ~f:(fun acc dep ->
        return acc >>=? fun () ->
        visit dep
      ) >>|? fun () ->
      let popped = Stack.pop_exn visiting in
      assert (String.(=) target popped);
      Queue.enqueue visit_finish_order target;
    end
  in
  visit target >>|? fun () ->
  Queue.to_list visit_finish_order
;;

let%test_module _ =
  (module struct

    let test graph ~target =
      let graph =
        String.Map.of_alist_exn (List.rev_map graph ~f:(fun (a, deps) ->
          a, String.Set.of_list deps))
      in
      let visited = String.Hash_set.create () in
      let visit_trace s =
        if Hash_set.mem visited s then
          failwiths "complexity violation in toposort, node visited more that once"
            (s, graph)
            [%sexp_of: string * String.Set.t String.Map.t]
        else
          Hash_set.add visited s
      in
      Thread_safe.block_on_async_exn (fun () ->
        topological_sort ~visit_trace ~target ~find_direct_deps:(fun ~target ->
          return (Ok (Map.find_exn graph target))
        )
      )
    ;;

    let%test_unit _ =
      let graph = [
        "a", ["b"; "c"; "d"];
        "b", ["c"];
        "c", ["d"];
        "d", ["e"];
        "e", [];
      ] in
      [%test_eq: string list] (test graph ~target:"a" |> ok_exn) ["e"; "d"; "c"; "b"; "a"]
    ;;

    let%test_unit _ =
      let all =
        List.init 26 ~f:(fun i ->
          String.make 1 (Char.of_int_exn (Char.to_int 'a' + i)))
      in
      let graph =
        let deps a = List.rev_filter all ~f:(String.(<) a) in
        List.map all ~f:(fun a -> a, deps a)
      in
      [%test_eq: string list] (test graph ~target:"a" |> ok_exn) (List.rev all)
    ;;

    let%test_unit _ =
      let graph = [
        "a", ["b"];
        "b", ["c"];
        "c", ["a"];
      ] in
      [%test_eq: Sexp.t]
        (test graph ~target:"a" |> [%sexp_of: _ Or_error.t])
        (Sexp.of_string "(Error (\"Circular dependency detected\" (a b c)))")
    ;;
  end)

let find_dependencies ~prog ~args ~working_dir ~target =
  let args = "-one-line" :: args in
  (* call ocamldep for one target compilation unit to return a list of compilation units *)
  let find_direct_deps ~target =
    (* ocamldep works fine if the ml or mli file doesn't exist *)
    Shell.run_lines ~working_dir prog (args @ [target ^ ".mli"; target ^ ".ml"])
    >>=? fun lines ->
    match post_process ~target lines with
    | Error _ as err -> return err
    | Ok deps ->
      (* remove self-dependency *)
      return (Ok (Set.remove deps target))
  in
  topological_sort ~visit_trace:ignore ~target ~find_direct_deps
;;
