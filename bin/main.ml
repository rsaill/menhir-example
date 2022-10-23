let run fn =
  ignore (Simple_parser__Parser.parse_file fn)

let _ =
  Arg.parse [] run ("Usage: "^ Sys.argv.(0) ^" files")
