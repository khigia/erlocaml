open Ocamerl


let _ = Tests.register "create" (fun () ->
    let q = Fifo.create () in
    OUnit.assert_equal (Fifo.is_empty q) true;
    OUnit.assert_equal (Fifo.length q) 0
)

let _ = Tests.register "put-get" (fun () ->
    let q = Fifo.create () in
    let _ = Fifo.put q 42 in
    OUnit.assert_equal (Fifo.length q) 1;
    let e = Fifo.get q in
    OUnit.assert_equal e 42
)

let _ = Tests.register "blocking get" (fun () ->
    let got_element = Event.new_channel () in
    let q = Fifo.create () in
    let getter = Thread.create
        (fun () ->
            let _ = Fifo.get q in
            let evt = Event.send got_element (Some true) in
            Event.sync evt
        )
        ()
    in
    let evt = Event.receive got_element in
    Thread.delay 0.001;
    OUnit.assert_equal (Event.poll evt) None;
    Fifo.put q 42;
    Thread.delay 0.001;
    OUnit.assert_equal (Event.poll evt) (Some (Some true));
    Thread.join getter
)

let _ = Tests.run "Fifo test suite"

