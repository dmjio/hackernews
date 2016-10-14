"use strict";
var system = require('system');
if (system.args.length !== 2) {
    console.log('Usage: phantomjsOpen.js URL');
    phantom.exit(1);
}

var page = require('webpage').create();

page.onError = function (msg, trace) {
  if (msg.match("error").length > 0) {
    phantom.exit(1);
  }
};

page.open(system.args[1], function (status) {
    page.onConsoleMessage = function (msg) {
	if (msg.match("done")) {
	    phantom.exit(0);
	} else if (msg.match("error")) {
	    phantom.exit(1);
	} else {
	    console.log(msg);
	}
    };
    if (status !== "success") {
        console.log("Unable to open " + system.args[1]);
        phantom.exit(1);
    }
});
