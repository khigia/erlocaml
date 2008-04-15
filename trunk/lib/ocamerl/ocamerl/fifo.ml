(* TODO implement a maximum size (not_full Condition.t) *)

type 'a t = {
    queue: 'a Queue.t;
    mutex: Mutex.t;
    not_empty: Condition.t;
}

let create () = {
    queue = Queue.create ();
    mutex = Mutex.create ();
    not_empty = Condition.create ();
}

let is_empty q =
    Mutex.lock q.mutex;
    let isEmpty = Queue.is_empty q.queue in
    Mutex.unlock q.mutex;
    isEmpty

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
