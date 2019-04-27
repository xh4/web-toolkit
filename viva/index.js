const ws = new WebSocket('ws://localhost:4001');


function sendJSON(data) {
    console.log("Send JSON:", data);
    ws.send(JSON.stringify(data));
}

ws.addEventListener('open', function (event) {
    console.log('WebSocket connected');
    sendJSON({foo: "bar"});
});

ws.addEventListener('message', function (event) {
    console.log("Receive message:", event.data);
    let message;
    try {
        message = JSON.parse(event.data);
    } catch (e) {
        console.error('Unable to parse message:', event.data);
        console.error(e);
        return;
    }
    if (!Array.isArray(message)) {
        console.error('Unable to handle message:', message);
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
    let name = message[0];
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
});

ws.addEventListener('error', function(event) {
    console.error("WebSocket error observed:", event);
});

ws.addEventListener('close', function(event) {
    console.error("WebSocket closed:", event);
});
