open Sqlpp_mariadb
module M = Mariadb.Blocking

let assert_ok = function
  | Ok v -> v
  | Error (code, msg) -> failwith (Printf.sprintf "Error %d: %s" code msg)

[%%sqlpp.env
{|
create table planets (
  planet_id int not null,
  name string not null,
  climate string,
  population int not null
);
|}]

let insert_planet =
  [%sqlpp.exec
    {| insert into planets set
       planet_id=?planet_id,
       name=?name,
       climate=?climate,
       population=?population |}]

let list_planets =
  [%fetch_list
    {| select planet_id, name, climate, population
       from planets
       order by planet_id |}]

let () =
  print_endline "connecting to mariadb...";
  let c = assert_ok @@ M.connect () ~db:"sw" ~socket:"/tmp/mariadb.socket" in
  print_endline "inserting planets...";
  List.iteri
    (fun planet_id (name, climate, population) ->
      insert_planet c ~planet_id ~name ~climate ~population)
    [
      "Tatooine", Some "arid", 200000;
      "Alderaan", Some "temperate", 2000000000;
      "Yavin IV", Some "temperate, tropical", 1000;
      "Hoth", Some "frozen", 0;
      "Dagobah", Some "murky", 0;
      "Bespin", Some "temperate", 6000000;
      "Endor", Some "forests, mountains, lakes", 30000000;
      "Naboo", Some "temperate", 4500000000;
      "Coruscant", Some "temperate", 1000000000000;
      "Kamino", None, 1000000000;
    ];
  print_endline "listing planets...";
  let planets = list_planets c in
  List.iter
    (fun (planet_id, name, climate, population) ->
      let climate = Option.value ~default:"-" climate in
      Printf.printf "planet_id=%d, name=%s, climate=%s, population=%d\n"
        planet_id name climate population)
    planets;
  print_endline "done";
  ()
