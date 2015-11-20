let print_iteri iteri lst =
  print_string "[";
  iteri ~f:(fun i v ->
    if i > 0 then
      print_string ", ";
    print_string v) lst;
  print_string "])";;
