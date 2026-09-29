

let write_file filename content = Out_channel.with_open_text filename (fun oc ->
  Out_channel.output_string oc content  
)


