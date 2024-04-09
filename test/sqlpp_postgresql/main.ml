open Sqlpp_postgresql
open Sqlpp_postgresql.IO

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

let assert_ok = function
  | Ok v -> v
  | Error msg -> failwith (Printf.sprintf "ERROR: %s" msg)

let test_planets =
  [
    0, "Tatooine", Some "arid", 200000;
    1, "Alderaan", Some "temperate", 2000000000;
    2, "Yavin IV", Some "temperate, tropical", 1000;
    3, "Hoth", Some "frozen", 0;
    4, "Dagobah", Some "murky", 0;
    5, "Bespin", Some "temperate", 6000000;
    6, "Endor", Some "forests, mountains, lakes", 30000000;
    7, "Naboo", Some "temperate", 4500000000;
    8, "Coruscant", Some "temperate", 1000000000000;
    9, "Kamino", None, 1000000000;
  ]

let () =
  assert_ok
  @@ Lwt_main.run
       (print_endline "connecting to mariadb...";
        Postgresql_lwt.connect () ~uri:"postgresql:///sw" >>= fun conn ->
        print_endline "inserting planets...";
        let rec insert_loop xs =
          match xs with
          | [] -> return ()
          | (planet_id, name, climate, population) :: xs ->
              insert_planet conn ~planet_id ~name ~climate ~population
              >>= fun () -> insert_loop xs
        in
        insert_loop test_planets >>= fun () ->
        print_endline "listing planets...";
        list_planets conn >>= fun planets ->
        List.iter
          (fun (planet_id, name, climate, population) ->
            let climate = Option.value ~default:"-" climate in
            Printf.printf "planet_id=%d, name=%s, climate=%s, population=%d\n"
              planet_id name climate population)
          planets;
        print_endline "done";
        Lwt.return_ok ())
