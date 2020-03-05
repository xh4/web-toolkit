var url = "ws://127.0.0.1:4000/welcome-page";

var ws = new WebSocket(url);

ws.addEventListener('open', function (event) {
    console.log("Open:", url);
});

ws.addEventListener('message', function (event) {
    // console.log("Message:", event);
    // handleRawMessage(event.data);
});

ws.addEventListener('error', function(event) {
    console.error("Error:", event);
});

ws.addEventListener('close', function(event) {
    console.error("Close:", event);
});
