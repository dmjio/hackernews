"use strict";
var system = require('system');
if (system.args.length !== 2) {
    console.log('Usage: phantomjsOpen.js URL');
    phantom.exit(1);
}
var page = require('webpage').create();

page.onConsoleMessage = function (msg) {
    console.log(msg);
};
page.onError = function (msg, trace) {
    error.append(msg);
    console.log(msg);
};
page.open(system.args[1], function (status) {
    if (status !== "success") {
        console.log("Unable to open " + system.args[1]);
        phantom.exit(1);
    }
});
