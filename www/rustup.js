function detect_platform() {
    "use strict";
    var os = "unknown";

    if (os == "unknown") {
	if (navigator.platform == "Linux x86_64") {os = "unix";}
	if (navigator.platform == "Linux i686") {os = "unix";}
    }

    // I wish I knew by now, but I don't. Try harder.
    if (os == "unknown") {
	if (navigator.appVersion.indexOf("Win")!=-1) {os = "win";}
	if (navigator.appVersion.indexOf("Mac")!=-1) {os = "unix";}
	if (navigator.appVersion.indexOf("Linux")!=-1) {os = "unix";}
    }

    return os;
}

(function () {
    "use strict";
    var platform = detect_platform();

    var unix_div = document.getElementById("platform-instructions-unix");
    var win_div = document.getElementById("platform-instructions-win");
    var unknown_div = document.getElementById("platform-instructions-unknown");

    if (platform == "unix") {
	unix_div.style.display = "block";
	unknown_div.style.display = "none";
    } else if (platform == "win") {
	win_div.style.display = "block";
	unknown_div.style.display = "none";
    }
}());
