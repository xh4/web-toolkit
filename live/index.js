wt.live.connect('ws://118.190.145.4:8003/live')
    .then(function() {
        setTimeout(function() {
            wt.live.call({type: "symbol",
                  name: "+"}, 1, 2, 3, 4);
        }, 1000);
    });
