var wt = (function() {

    var ws;

    function sendJSON(data) {
        var string = JSON.stringify(data);
        ws.send(string);
    }

    function sendMessage(name, ...args) {
        var data = [name].concat(args);
        console.log("Send:", data);

        if (!ws) {
            console.error("Connection not opened");
            return;
        }

        sendJSON(data);
    }

    function handleMessage(message) {
        console.log("Receive:", message);

        var name = message[0];
        switch (name) {
        case 'eval':
            if (message.length < 2) {
                console.error('Eval wat?');
                return;
            }
            try {
                eval(message[1]);
            } catch (e) {
                console.error('When eval:', message[1]);
                console.error(e);
            }
            break;
        default:
            console.error('Unable to handle message:', message);
        }
    }

    function handleRawMessage(data) {
        var message;
        try {
            message = JSON.parse(data);
        } catch (e) {
            console.error('Unable to parse message:', data);
            console.error(e);
            return;
        }
        if (!Array.isArray(message)) {
            console.error('Unable to handle message:', message);
            return;
        }
        if (message.length < 1) {
            console.error('Unable to handle message:', message);
            return;
        }
        handleMessage(message);
    }

    function connect(url) {
        var deferred = $.Deferred();

        ws = new WebSocket(url);

        console.log('Connect:', url);

        ws.addEventListener('open', function (event) {
            console.log('Open:', url);

            deferred.resolve();
        });

        ws.addEventListener('message', function (event) {
            handleRawMessage(event.data);
        });

        ws.addEventListener('error', function(event) {
            console.error("Error:", event);

            deferred.reject(event);
        });

        ws.addEventListener('close', function(event) {
            console.error("Close:", event);
        });

        return deferred;
    }

    function call(name, ...args) {
        sendMessage.apply(null, ["call", name].concat(args));
    }

    function cast(name, ...args) {
        sendMessage.apply(null, ["cast", name].concat(args));
    }

    return {
        connect: connect,
        sendMessage: sendMessage,
        call: call,
        cast: cast
    };

})();
