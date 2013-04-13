/* A Lisp to JavaScript converter.
 * Based on Dmitry Nizhegorodov's converter and node.js.
 * @author Zoltan Kovacs <zoltan@geogebra.org>
 */

if (process.argv.length < 3) {
    console.log('This script converts well-behaving Lisp files to Javascript.');
    console.log('Usage: nodejs ' + process.argv[1] + ' <filename>');
    process.exit(1);
}

var fs = require('fs'),
    filename = process.argv[2];

fs.readFile(filename, 'utf8', function (err, __code) {

    if (err) throw err;

    require("./lisp.js");
    require("./lisp-lib.js");
    Lisp.use_package("Lisp");

    var processToplevels = function (action) {
        action = action || out;
        var curr = 0;
        while (true) {
            prec = 0;
            retval = false;
            var res = read_from_string(__code, curr);
            if (!res) break;
            curr = res.cdr;
            action(res.car);
        }
    };

    var __js = [];
    processToplevels(function (e) {
        __js.push(translate(macroexpand(e)))
    });

    for (var i = 0; i < __js.length; i++) {
      console.log(__js[i] + ";");
    }

});
