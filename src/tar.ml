let extract ~working_dir tar =
  let args = [
    "-xzf"; tar;
  ] in
  Shell.run ~working_dir "tar" args
;;

let list tar =
  (* curiously, tar doesn't need a z option *)
  let args = [ "-tf" ; tar ] in
  Shell.run_lines "tar" args
;;

let create ~working_dir ~files tar =
  let args = [
    "-czf"; tar;
  ] @ files
  in
  Shell.run ~working_dir "tar" args
;;
