wt.connect('ws://118.190.145.4:4002')
    .then(function() {
        setTimeout(function() {
            wt.call({type: "symbol",
                  name: "+"}, 1, 2, 3, 4);
        }, 1000);
    });
