let n1 = Enode.make "ocamerltest@localhost" in
let result, n2 = Enode.publish n1 in
let _ = Trace.info (lazy (Trace.printf "Node publication result: %B\n" result)) in
let n3 = Enode.unpublish n2 in
let _ = Trace.info (lazy (Trace.printf "Node unpublished\n")) in
()
