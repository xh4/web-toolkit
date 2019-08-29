module.exports = function(api) {
    api.cache(true);

    const presets = [
        [
            "@babel/env",
            {
                targets: {
                    edge: "17",
                    firefox: "60",
                    chrome: "67",
                    safari: "11.1",
                },
                useBuiltIns: "usage",
            },
        ],
    ];

    const plugins = [
        "@babel/plugin-transform-classes",
        "@babel/plugin-transform-arrow-functions"
    ];

    return {
        presets: presets,
        plugins: plugins
    };
};
