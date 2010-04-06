/*
 * String buffer, for Jaml templates.
 * Each instance is used to build up the JS for a single template.
 *
 *    - At start of template, instantiate, which starts an empty buffer.
 *
 *    - To add strings to it:
 *        + s()  -- for simple strings
 *        + ns() -- strings which MAY (usually) need to be preceeded with a newline
 *
 *
 * All the method names are VERY SHORT, because:
 *
 *    - No loss of expressiveness.
 *      Devs never use them directly; they're used only in generated code.
 *
 *    - Less data on client.
 *      They're used over and over in the generated code.  Good to keep small.
 *
 *
 * Class variable: "skip_newline_p".
 *
 *    - When set to true, the very next call to ns()
 *      will NOT prefix the provided string with a newline.
 *
 *    - It's a CLASS var because it's used to control behavior across instances.
 *      Templates are composable.  When one template includes another,
 *      the value of skip_newline_p affects it as well.
 */
var Jaml = Class.create({

    initialize : function() {
        this.buffer = [];   // [String]
    },

    // 's' == add _S_tring
    // :: String -> ()
    s : function(s) {
        this.buffer.push(s);
    },

    // 'ns' == add possible _N_ewline and then _S_tring
    // :: String -> ()
    ns : function(s) {
        if (Jaml.skip_newline_p) {
            Jaml.skip_newline_p = false;
        } else {
            this.buffer.push('\n');
        }
        this.s(s);
    },

    // 'v' == resultant _V_alue (string)
    // :: () -> String
    v : function() {
        return this.buffer.join('');
    }
});


/*
   Class var and method.
*/
Jaml.skip_newline_p = false;

// 'x' signifies "don't output newline".
// :: () -> ()
Jaml.x = function() {
    this.skip_newline_p = true;
}
