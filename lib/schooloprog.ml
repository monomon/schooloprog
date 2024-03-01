open Icalendar
open Sexplib.Sexp

(* record types *)
type school_class = {
  name: string;
  class_start: Icalendar.timestamp_local;
  class_end: Icalendar.timestamp_local;
}

type holiday = {
  name: string;
  date: Icalendar.timestamp_local;
}

(* parsers *)

let tz_default = (false, "Europe/Sofia")
let to_ptime date time: Icalendar.timestamp_utc = (date, (time, 0)) |> Ptime.of_date_time |> Option.get
let maybe_parse_date (x: string): Icalendar.timestamp_local = x |> Ptime.of_rfc3339 |> Result.get_ok
                                                              |> function (t, tz, _) -> (Ptime.add_span t (Ptime.Span.of_int_s (Option.get tz))) |> Option.get
(* return local timestamp, with added offset *)

let to_class (sexp: Sexplib.Sexp.t): school_class = (match sexp with
    | List [
        (Atom ":name");
        (Atom name);
        (Atom ":teacher");
        (Atom _);
        (Atom ":start");
        (Atom class_start);
        (Atom ":end");
        (Atom class_end)] ->
      { name = name;
        class_start = maybe_parse_date class_start;
        class_end = maybe_parse_date class_end;
      }
    | Atom _ -> failwith "Atom bla"
    | a -> failwith (to_string a))

let to_holiday sexp: holiday = (match sexp with
    | List [
        (Atom ":name");
        (Atom name);
        (Atom ":date");
        (Atom date);
      ] ->
      {
        name = name;
        date = maybe_parse_date date;
      }
    | a -> failwith (to_string a))

let combine_date_time (d1:Icalendar.timestamp_utc) (d2:Icalendar.timestamp_utc): Icalendar.timestamp_utc =
  Option.get (Ptime.of_date_time ((Ptime.to_date d1), (snd (Ptime.to_date_time d2))))

let to_event (holidays: holiday list) (year_end: Icalendar.timestamp_utc) (c: school_class): Icalendar.event = {
  uid = (Params.empty, "");
  dtstart = (Params.empty, `Datetime (`With_tzid (c.class_start, tz_default)));
  dtstamp = (Params.empty, c.class_start);
  rrule = Some (Params.empty,
                (`Weekly,
                 Some (`Until (`Utc year_end)),
                 None,
                 []));
  dtend_or_duration = Some (`Dtend (Params.empty, `Datetime (`With_tzid (c.class_end,
                                                                         tz_default))));
  alarms = [];
  props = [
    `Summary (Params.empty, c.name);
    `Categories (Params.empty, ["school"; "class"]);
    `Exdate (Params.empty,
             `Datetimes (List.map (fun x -> `With_tzid ((combine_date_time
                                                           x.date
                                                           c.class_start),
                                                        tz_default))
                           holidays));
  ];
}

let holiday_to_event (holiday: holiday): Icalendar.event = {
  uid = (Params.empty, "");
  dtstart = (Params.empty, `Datetime (`With_tzid (holiday.date, tz_default)));
  dtstamp = (Params.empty, holiday.date);
  dtend_or_duration = Some (`Duration (Params.empty, Ptime.Span.of_int_s(24 * 60 * 60)));
  rrule = None;
  alarms = [];
  props = [
    `Summary (Params.empty, holiday.name);
    `Categories (Params.empty, ["holiday"]);
  ];
}

let gen_timezone = (`Timezone ([`Timezone_id ((Params.empty, tz_default));
                                `Daylight ([`Tzoffset_from ((Params.empty, Ptime.Span.of_int_s (60*60)));
                                            `Rrule ((Params.empty,
                                                     (`Yearly, None, None,
                                                      [`Bymonth ([3]);
                                                       `Byday ([(-1, `Sunday)])])));
                                            `Dtstart_local ((Params.empty,
                                                             to_ptime (1981,03,29) (02,00,00) ));
                                            `Tzname ((Params.empty, "EEST"));
                                            `Tzoffset_to ((Params.empty, Ptime.Span.of_int_s (60*60*2)))]);
                                `Standard ([`Tzoffset_from ((Params.empty, Ptime.Span.of_int_s (60*60)));
                                            `Rrule ((Params.empty,
                                                     (`Yearly, None, None,
                                                      [`Bymonth ([10]);
                                                       `Byday ([(-1, `Sunday)])])));
                                            `Dtstart_local ((Params.empty,
                                                             to_ptime (1981,03,29) (02,00,00) ));
                                            `Tzname ((Params.empty, "EET"));
                                            `Tzoffset_to ((Params.empty, Ptime.Span.of_int_s (60*60*2)));
                                           ]);
                               ]))

(*create calendar object*)
let to_calendar (sexp: Sexplib.Sexp.t): Icalendar.calendar = match sexp with
  | List [
      (Atom ":classes");
      (List cs);
      (Atom ":year-start");
      (Atom _);
      (Atom ":year-end");
      (Atom year_end);
      (Atom ":holidays");
      (List holidays);
    ] -> (let holi = (List.map to_holiday holidays) in
          ([`Version (Params.empty, "2.0") ;
            `Prodid (Params.empty, ".//mois@monomon.me//schooloprog//BG");
           ],
           cs |> List.map to_class
           |> List.map (fun x -> `Event (to_event
                                           holi
                                           (maybe_parse_date year_end)
                                           x)) (* events for classes *)
           |> List.append (List.map (fun x -> `Event (holiday_to_event x)) holi) (* events for holidays*)
           |> (List.cons gen_timezone)
          ))
  | _ -> failwith "Other bla"


let sexp_to_ics_file input_file output_file =
  let fd = open_out output_file in
  (load_sexps input_file |> List.map to_calendar |> List.hd |>
   Icalendar.to_ics |>  Printf.fprintf fd "%s")

let sexp_to_ics input_file =
  (load_sexps input_file |> List.map to_calendar |> List.hd |>
   Icalendar.to_ics |>  Printf.printf "%s")
