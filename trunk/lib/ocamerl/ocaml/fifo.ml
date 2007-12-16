(* TODO implement a maximum size (not_full Condition.t) *)

type 'a t = {
    queue: 'a Queue.t;
    mutex: Mutex.t;
    not_empty: Condition.t;
}

let create () = {
    queue = Queue.create();
    mutex = Mutex.create();
    not_empty = Condition.create();
}

let length q =
    Mutex.lock q.mutex;
    let len = Queue.length q.queue in
    Mutex.unlock q.mutex;
    len

let put q i =
    Mutex.lock q.mutex;
    Queue.push i q.queue;
    Condition.signal q.not_empty;
    Mutex.unlock q.mutex

let get q =
    Mutex.lock q.mutex;
    while Queue.is_empty q.queue do
        Condition.wait q.not_empty q.mutex
    done;
    let i = Queue.pop q.queue in
    Mutex.unlock q.mutex;
    i

let test () =
    let tn = 100 in (* thread number *)
    let gn = 10 in (* each thread try to get/put gn times *)
    let q = create () in
    for n = tn downto 1 do
        ignore(Thread.create
            (fun () -> for c = 1 to gn do let i = get q in Printf.printf "%i got %i at game %i" n i c; print_newline (); put q n; Thread.delay 0.001 done)
            ()
        )
    done;
    ignore (Thread.create
        (fun () -> Printf.printf "Start by putting 0"; print_newline (); put q 0)
        ()
    )
