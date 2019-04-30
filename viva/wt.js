var wt = (function() {

    var wt_internal = 42;

    function get() {
        return "wt";
    }

    return {
        get: get
    };

})();
