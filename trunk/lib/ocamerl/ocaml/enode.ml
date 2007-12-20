module NodeServer =
    struct
        type t = {
            name: string;
            addr: Unix.inet_addr;
            port: int;
        }

        let make nodeName =
            let sock = Serv.listen 0 in
            let addr, port = Serv.inet_addr sock in
            Trace.debug (lazy (Trace.printf "NodeServer listening on port %i for node '%s'\n" port nodeName));
            (* TODO run a thread to serve sock *)
            {
                name = nodeName;
                addr = addr;
                port = port;
            }

    end

type node = {
    epmc: Epmc.t;
    server: NodeServer.t;
}

let make nodeName =
    Trace.info (lazy (Trace.printf "Making node '%s'\n" nodeName));
    let server = NodeServer.make nodeName in
    let epmc = Epmc.make server.NodeServer.name server.NodeServer.port in
    {
        epmc = epmc;
        server = server;
    }

let publish node =
    let result, newEpmc = Epmc.connect node.epmc in
    (result, {node with epmc = newEpmc})
    
let unpublish node =
    let newEpmc = Epmc.disconnect node.epmc in
    {node with epmc = newEpmc}
    
