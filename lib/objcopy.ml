type filename = string

let objcopy = "objcopy"

let (===) = Printf.sprintf "%s=%s"

let embed_cmd ~filename ~section_id ~section ~destination =
  objcopy, [
    "--add-section";
    section_id === section;
    filename;
    destination;
  ]

let retrieve_cmd ~filename ~section_id ~destination =
  objcopy, [
    "-O"; "binary";
    "--only-section"; section_id;
    "--set-section-flags";
    section_id === "alloc";
    filename;
    destination;
  ]

let embed ~filename ~section_id ~section ~destination =
  let cmd, args = embed_cmd ~filename ~section_id ~section ~destination in
  Shell.run cmd args

let retrieve ~filename ~section_id ~destination =
  let cmd, args = retrieve_cmd ~filename ~section_id ~destination in
  Shell.run cmd args
