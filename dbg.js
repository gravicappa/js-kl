shenjs_dbg_log = false

function dbg_list(x) {
	var ret = []
	for (var i = x.length - 1; i >= 0; --i)
		ret = [shen_type_cons, x[i], ret]
	return ret
}

function dbg_output(s) {
	if (!shenjs_dbg_log)
		return
	var arg = []
	for (var i = arguments.length - 1; i > 0; --i)
		arg = [shen_tuple, arguments[i], arg]
	print(shenjs_call(shen_intmake_string, [s, arg]))
}

function dbg_princ(s, x) {
	if (x == shen_fail_obj)
		x = "fail!"
	if (!shenjs_dbg_log)
		return x
	dbg_output("~A~A~%", s, x)
	return x
}

function dbg_trace(fn, traceret) {
	traceret = (traceret == undefined) ? true : traceret;
	var name = str_js_from_shen_js(fn)
  var x = "try {old_" + name + ";} catch (e) { \
             old_" + name + " = " + name + "; \
             " + name + " = (function(X) { \
               dbg_output(\"(~A ~A)~%\", \"" + fn + "\" , X); \
							 if (" + traceret + ") \
								 return dbg_princ(\"" + fn + " => \", old_"+ name + "(X)); \
								 return old_"+ name + "(X); \
             }); \
           }"
	eval(x);
}

function _s (s) {
	return shen_intern_js([s])
}

function shen_print(args) {
	if (args.length < 1) return [shen_print, 1, args];
	print(args[0])
}

function dbg_timed(name, fn) {
	var t = (new Date()).getTime()
	var ret = fn()
	var t = ((new Date()).getTime() - t) / 1000.0
	puts_js("\n## run time of " + name + ": " + t + "s\n\n")
	return ret
}

js_print = shenjs_mkfunction("js-print", 1, function self(Args) {
	if (Args.length < 1) return [shen_type_func, self, 1, Args]
	print(Args[0])
	return []
})
