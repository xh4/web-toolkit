if (typeof wt === "undefined") {
    var wt = {};
}

// https://github.com/kelektiv/node-uuid
//
wt.uuid = (function() {

    var getRandomValues = (typeof(crypto) != 'undefined' && crypto.getRandomValues && crypto.getRandomValues.bind(crypto)) ||
        (typeof(msCrypto) != 'undefined' && typeof window.msCrypto.getRandomValues == 'function' && msCrypto.getRandomValues.bind(msCrypto));

    var rng;

    if (getRandomValues) {
        // WHATWG crypto RNG - http://wiki.whatwg.org/wiki/Crypto
        var rnds8 = new Uint8Array(16); // eslint-disable-line no-undef

        rng = function() {
            getRandomValues(rnds8);
            return rnds8;
        };
    } else {
        // Math.random()-based (RNG)
        //
        // If all else fails, use Math.random().  It's fast, but is of unspecified
        // quality.
        var rnds = new Array(16);

        rng = function() {
            for (var i = 0, r; i < 16; i++) {
                if ((i & 0x03) === 0) r = Math.random() * 0x100000000;
                rnds[i] = r >>> ((i & 0x03) << 3) & 0xff;
            }
            return rnds;
        };
    }

    var byteToHex = [];
    for (var i = 0; i < 256; ++i) {
        byteToHex[i] = (i + 0x100).toString(16).substr(1);
    }

    function bytesToUuid(buf, offset) {
        var i = offset || 0;
        var bth = byteToHex;
        // join used to fix memory issue caused by concatenation: https://bugs.chromium.org/p/v8/issues/detail?id=3175#c4
        return ([bth[buf[i++]], bth[buf[i++]],
                 bth[buf[i++]], bth[buf[i++]], '-',
                 bth[buf[i++]], bth[buf[i++]], '-',
                 bth[buf[i++]], bth[buf[i++]], '-',
                 bth[buf[i++]], bth[buf[i++]], '-',
                 bth[buf[i++]], bth[buf[i++]],
                 bth[buf[i++]], bth[buf[i++]],
                 bth[buf[i++]], bth[buf[i++]]]).join('');
    }

    function v4(options, buf, offset) {
        var i = buf && offset || 0;

        if (typeof(options) == 'string') {
            buf = options === 'binary' ? new Array(16) : null;
            options = null;

        }
        options = options || {};

        var rnds = options.random || (options.rng || rng)();

        // Per 4.4, set bits for version and `clock_seq_hi_and_reserved`
        rnds[6] = (rnds[6] & 0x0f) | 0x40;
        rnds[8] = (rnds[8] & 0x3f) | 0x80;

        // Copy bytes to buffer, if provided
        if (buf) {
            for (var ii = 0; ii < 16; ++ii) {
                buf[i + ii] = rnds[ii];
            }
        }
        return buf || bytesToUuid(rnds);
    }

    return {
        v4: v4
    };

})();


wt.live = (function() {

    var ws;

    var messageDispatcher = {};

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
        var handler = messageDispatcher[name];

        if (handler) {
            handler(message);
        } else {
            console.error('No handler found for message:', message);
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

    var callResultDispatcher = {};

    function call(name, ...args) {
        var id = wt.uuid.v4(),
            deferred = $.Deferred();

        callResultDispatcher[id] = deferred;

        sendMessage.apply(null, ["call", id, name].concat(args));
    }

    function cast(name, ...args) {
        sendMessage.apply(null, ["cast", name].concat(args));
    }

    return {
        connect: connect,
        sendMessage: sendMessage,
        call: call,
        cast: cast,
        messageDispatcher: messageDispatcher,
        callResultDispatcher: callResultDispatcher
    };

})();


wt.live.messageDispatcher['eval'] = function(message) {
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
};


wt.live.messageDispatcher['reply'] = function(message) {
    if (message.length < 2) {
        console.error('Eval wat?');
        return;
    }

    var id = message[1];

    try {
        eval(message[1]);
    } catch (e) {
        console.error('When eval:', message[1]);
        console.error(e);
    }
};
