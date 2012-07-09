function shenjs_load_files() {
	var dir = "/home/ramil/dev/shen/modules/javascript/"
	var shen_src_js = ["vector-closures.js",
										 "javascript.js",
										 "core.js",
										 "sys.js",
										 "sequent.js",
										 "yacc.js",
										 "writer.js",
										 "reader.js",
										 "prolog.js",
										 "track.js",
										 "declarations.js",
										 "load.js",
										 "macros.js",
										 "types.js",
										 "t-star.js",
										 "toplevel.js"]

	load(dir + "runtime.js")
	load(dir + "io-cli.js")
	load(dir + "io.js")
	load(dir + "js/primitives.js")
	load(dir + "dbg.js")

	load(dir + "test.js")

	shenjs_dbg_log = false

	/* dummy functions to bypass defstruct's declarations */
	shen_process_datatype = [shen_type_func, function(args) {return []}, 2, []]
	shen_compile = [shen_type_func, function(args) {return []}, 3, []]

	for (var i = 0; i < shen_src_js.length; ++i) {
		var f = dir + "js/" + shen_src_js[i]
		print("# loading " + f)
		load(f)
	}
}

function shenjs_start_shen() {
	shenjs_globals["shen-*exit-from-repl*"] = false
	shenjs_call(shen_shen, [])
}

shenjs_load_files()

shenjs_dbg_log = true
shenjs_globals['shen-*show-eval-js*'] = false

shenjs_start_shen()
