let extract ~working_dir tar =
  let args = [
    "--directory"; working_dir;
    "-vxzf"; tar;
  ] in
  Shell.run_lines ~working_dir "tar" args

let list tar =
  let args = [ "-tf" ; tar ] in
  Shell.run_lines "tar" args

let create ~working_dir ~files tar =
  let args = [
    "-czf"; tar;
  ] @ files
  in
  Shell.run ~working_dir "tar" args
