Shen_tco_obj = function(fn) {this.fn = fn}

shen_fail_obj = new Object
shenjs_globals = []

shen_counter_type = 0
shen_type_func = --shen_counter_type
shen_type_symbol = --shen_counter_type
shen_type_cons = --shen_counter_type
shen_type_stream_in = --shen_counter_type
shen_type_stream_out = --shen_counter_type
shen_type_stream_inout = --shen_counter_type
shen_type_error = --shen_counter_type

shen_true = true
shen_false = false

function shenjs_call(x, args) {
  var j = 0, nargs = args.length
  do {
    if (typeof(x) == "function") {
      x = x([args[j++]])
    } else if (x[0] == shen_type_func) {
      var c = x[3], n = c.length, k = x[2], a
      k = (k > n + nargs) ? n + nargs : k
      a = new Array(k)

      for (var i = 0; i < n; ++i)
        a[i] = c[i]
      for (;i < k && j < nargs; ++j, ++i)
        a[i] = args[j]
      x = (x[1])(a)
    } else if (x[0] == shen_type_symbol) {
      x = shenjs_get_fn(x)
    } else
      return shenjs_error(["shenjs_call: Wrong function: '" + x + "'"])
    while (x instanceof Shen_tco_obj)
      x = x.fn()
  } while (j < nargs && x[0] == shen_type_func);
  return x
}

function shenjs_get_fn(x) {
  if (typeof(x) == "function")
    return x
	switch (x[0]) {
  case shen_type_symbol:
    try {
      return eval(shenjs_str_js_from_shen(x[1]))
    } catch (e) {
      try {
        return eval(shenjs_str_js_from_shen("shen-" + x[1]))
      } catch (e) {
        shenjs_error(["Cannot find (|shen_)'" + x[1] + "'"])
      }
    }
    return shen_fail_obj
	case shen_type_func: return x
  }
  throw new Error("function " + shenjs_str_shen_from_js(x[1]) + " not found")
}

function shenjs_error(args) {
  if (args.length < 1) return [shen_type_func, shenjs_error, 1, args]
  if (shenjs_is_true(shenjs_globals['shen-*show-error-js*']))
    shenjs_puts("# err: " + args[0] + "\n")
  throw new Error(args[0]);
  return shen_fail_obj
}

function shenjs_error_to_string(args) {
  if (args.length < 1)
		return [shen_type_func, shenjs_error_to_string, 1, args]
  var s = args[0], stack = s.stack;
  return (stack == undefined) ? ("" + s) : ("" + s + " " + stack);
}

function shenjs_get_time(args) {
  if (args.length < 1) return [shen_type_func, shenjs_get_time, 1, args]
  return (new Date()).getTime() / 1000.0
}

shenjs_simple_error = shenjs_error

shenjs_log_eq = false

function shenjs_trap_error(fn, handler) {
  try {
    return fn()
  } catch (e) {
    return shenjs_call(handler, [e])
  }
}

function shenjs_notrap_error(fn, handler) {
  return fn()
}

function shenjs_equal_boolean(b, x) {
  return ((x instanceof Array)
          && x[0] == shen_type_symbol
          && ((x[1] == "true" && b === true)
              || (x[1] == "false" && b === false)))
}

function shenjs_equal_function(f, x) {
  return ((f.name.length > 0)
           && (x instanceof Array)
           && x[0] == shen_type_symbol
           && x[1] == f.name)
}

function shenjs_$eq$(args) {
  if (args.length < 2) return [shen_type_func, shenjs_$eq$, 2, args]
  var x = args[0], y = args[1], tx = typeof(x), ty = typeof(y)
  if (tx != ty)
    return ((tx == "boolean" && shenjs_equal_boolean(x, y))
            || (ty == "boolean" && shenjs_equal_boolean(y, x))
            || (tx == "function" && shenjs_equal_function(x, y))
            || (ty == "function" && shenjs_equal_function(y, x)))
  switch (tx) {
  case "number":
  case "boolean":
  case "function":
  case "string":
    return x == y;

  case "object":
    if (((x instanceof Array) ^ (y instanceof Array))
        || (x.length != y.length))
      return false;
    if (x.length == 0)
      return true;
    if (x == shen_fail_obj && y == shen_fail_obj)
      return true;
    if (x[0] != y[0])
      return false;
    switch (x[0]) {
    case shen_type_func: 
      if (x[1] != y[1] || x[2] != y[2])
        return false
      var n = x[3].length
      if (n != y[3].length)
        return false
      for (var i = 0; i < n; ++i)
        if (x[3][i] != y[3][i])
          return false
      return true
    case shen_type_symbol: return x[1] == y[1];
    case shen_type_cons:
      var r = shenjs_call(shen_$eq$, [x[1], y[1]])
      if (!r)
        return false
      return new Shen_tco_obj(function () {
        return shenjs_call(shen_$eq$, [x[2], y[2]])
      });
    case shen_type_stream_out:
    case shen_type_stream_in: return x[1] == y[1] && x[2] == y[2];
    default:
      for (var i = 1; i < x.length; ++i) {
        var r = shenjs_call(shen_$eq$, [x[i], y[i]])
        if (!r)
          return false;
      }
      return true;
    }
    break;
  default: return false;
  }
}

function shenjs_empty$question$(args) {
  if (args.length < 1)
    return [shen_type_func, shenjs_empty$question$, 1, args]
  var x = args[0]
  return ((x instanceof Array) && !x.length)
}

function shenjs_is_type(x, type) {
  if (type == shen_type_symbol && (x === true || x === false))
    return true
  return ((x instanceof Array) && x[0] == type)
}

function shenjs_boolean$question$(args) {
  if (args.length < 1) 
    return [shen_type_func, shenjs_boolean$question$, 1, args]
  var x = args[0]
  return (typeof(x) == "boolean") || (shenjs_is_type(x, shen_type_symbol)
                                      && (x[1] == "true" || x[1] == "false"))
}

function shenjs_vector$question$(args) {
  if (args.length < 1)
    return [shen_type_func, shenjs_vector$question$, 1, args]
  var x = args[0]
  return ((x instanceof Array) && x[0] > 0)
}

function shenjs_absvector$question$(args) {
  if (args.length < 1)
    return [shen_type_func, shenjs_absvector$question$, 1, args]
  var x = args[0]
  return ((x instanceof Array) && x.length > 0
          && ((typeof(x[0]) != "number")
              || x[0] >= 0 || x[0] <= shen_counter_type))
}

function shenjs_absvector(args) {
  if (args.length < 1) return [shen_type_func, shenjs_absvector, 1, args]
  var n = args[0]
  var ret = new Array(n)
  for (var i = 0; i < n; ++i)
    ret[i] = shen_fail_obj
  return ret
}

function dbg_princ(s, x) {
  dbg_print(" " + s + x)
  return x
}

function dbg_print(s) {
  if (shenjs_is_true(shenjs_globals['shen-*show-error-js*']))
    shenjs_puts(s + "\n")
}

function shenjs_is_true(x) {
  return x != false || ((x instanceof Array)
                        && (x[0] == shen_type_symbol)
                        && (x[1] != "false"))
}

function shenjs_absvector_ref(x, i) {
  if (x.length <= i || i < 0)
    shenjs_error(["out of range"])
  return x[i]
}

function shenjs_absvector_set(x, i, v) {
  if (x.length <= i)
    shenjs_error(["out of range"])
  x[i] = v
  return x
}

function shenjs_value(args) {
  if (args.length < 1) return [shen_type_func, shenjs_value, 1, args]
  var s = args[0], x = shenjs_globals[s[1]]
  if (x == undefined)
    shenjs_error(["The variable " + s + " is unbound."])
  else
    return x
}

function shenjs_set(args) {
  if (args.length < 2) return [shen_type_func, shenjs_set, 2, args]
  var s = args[0]
  return (shenjs_globals[s[1]] = args[1])
}

function shenjs_vector(args) {
  if (args.length < 1) return [shen_type_func, shenjs_vector, 1, args]
  var n = args[0]
  var r = new Array(n + 1)
  r[0] = n
  for (var i = 1; i <= n; ++i)
    r[i] = shen_fail_obj
  return r
}

function shenjs_esc(x) {
  var ret = ""
  for (var i = 0; i < x.length; ++i)
    switch (x[i]) {
      case '"': ret += '\\"'; break;
      default: ret += x[i]; break
    }
  return ret
}

shenjs_sym_map_shen = []
shenjs_sym_map_js = []

shenjs_word_restricted = []

function shenjs_init_restricted () {
  var words = [
    "return", "new", "delete", "function", "while", "for", "var", "if", "do",
    "in", "super", "load", "print", "eval", "read", "readline", "write",
    "putstr", "let", "Array", "Object", "document"
  ];
	var nwords = words.length;
  for (var i = 0; i < nwords; ++i)
    shenjs_word_restricted[words[i]] = 1
}
shenjs_init_restricted()

function shenjs_register_sym_map(js, shen) {
  shenjs_sym_map_js[shen] = js
  shenjs_sym_map_shen[js] = shen
}

function shenjs_str_shen_from_js(s) {
  return shenjs_str_map([], shenjs_sym_map_shen, s)
}

function shenjs_str_js_from_shen(s) {
  return shenjs_str_map(shenjs_word_restricted, shenjs_sym_map_js, s)
}

shenjs_register_sym_map("_", "-")
shenjs_register_sym_map("$_", "_")
shenjs_register_sym_map("$$", "$")
shenjs_register_sym_map("$quote$", "'")
shenjs_register_sym_map("$bquote$", "`")
shenjs_register_sym_map("$slash$", "/")
shenjs_register_sym_map("$asterisk$", "*")
shenjs_register_sym_map("$plus$", "+")
shenjs_register_sym_map("$percent$", "%")
shenjs_register_sym_map("$eq$", "=")
shenjs_register_sym_map("$question$", "?")
shenjs_register_sym_map("$excl$", "!")
shenjs_register_sym_map("$gt$", ">")
shenjs_register_sym_map("$lt$", "<")
shenjs_register_sym_map("$dot$", ".")
shenjs_register_sym_map("$bar$", "|")
shenjs_register_sym_map("$sharp$", "#")
shenjs_register_sym_map("$tilde$", "~")
shenjs_register_sym_map("$colon$", ":")
shenjs_register_sym_map("$sc$", ";")
shenjs_register_sym_map("$amp$", "&")
shenjs_register_sym_map("$at$", "@")
shenjs_register_sym_map("$cbraceopen$", "{")
shenjs_register_sym_map("$cbraceclose$", "}")
shenjs_register_sym_map("$shen$", "")

function shenjs_str_starts_with(s, start) {
  var len = start.length
  if (s.length < len)
    return false
  return (s.substring(0, len) == start)
}

function shenjs_str_map(word_tbl, sym_tbl, s) {
  if (word_tbl[s])
    return "$shen$" + s
  var ret = ""
  var replaced
  while (s != "") {
		replaced = false
    for (k in sym_tbl)
      if (k != "" && shenjs_str_starts_with(s, k)) {
        ret += sym_tbl[k]
        s = s.substring(k.length, s.length)
        replaced = true
        break
      }
	  if (!replaced) {
      ret += s[0]
      s = s.substring(1, s.length)
    }
  }
  return ret
}

function shenjs_str(args) {
  if (args.length < 1) return [shen_type_func, shenjs_str, 1, args]
  var x = args[0]
  var err = " is not an atom in Shen; str cannot print it to a string."
  switch (typeof(x)) {
    case "string": return "\"" + shenjs_esc(x) + "\""
    case "number":
    case "boolean": return "" + x
    case "function":
      if (x.name.length > 0)
        return shenjs_str_shen_from_js(x.name)
      return "#<function>"
    case "object":
      if (x == shen_fail_obj)
        return "fail!"
      if (x instanceof Array) {
        if (x.length <= 0) {
          shenjs_error(["[]" + err])
          return shen_fail_obj
        }
        switch (x[0]) {
          case shen_type_symbol: return x[1]
          case shen_type_func:
						// TODO: probably I should store function name if possible
						return "#<function>"
        }
      }
  }
  shenjs_error([x + err])
  return shen_fail_obj
}

function shenjs_intern(args) {
  if (args.length < 1) return [shen_type_func, shenjs_intern, 1, args]
  var s = args[0]
  switch (s) {
  case "true": return true
  case "false": return false
  default: return [shen_type_symbol, s]
  }
}

function shenjs_tlstr(args) {
  if (args.length < 1) return [shen_type_func, shenjs_tlstr, 1, args]
  var x = args[0]
  if (x == "")
    return [shen_type_symbol, "shen_eos"]
  return x.substring(1, x.length)
}

function shenjs_n_$gt$string(args) {
  if (args.length < 1) return [shen_type_func, shenjs_n_$gt$string, 1, args]
  return String.fromCharCode(args[0])
}

function shenjs_string_$gt$n(args) {
  if (args.length < 1) return [shen_type_func, shenjs_string_$gt$n, 1, args]
  return args[0].charCodeAt(0)
}

function shenjs_eval_in_global(x) {
  try {
    var g = window;
  } catch (e) {
    var g = this;
  }
  if (g.execScript) // eval in global scope for IE
    return g.execScript(x);
  else // other browsers
    return eval.call(null, x);
}

function shenjs_eval_kl (args) {
  if (args.length < 1) return [shen_type_func, shenjs_eval_kl, 1, args]
  var x = args[0]
  var log = false
  if (shenjs_is_true(shenjs_globals['shen-*show-eval-js*']))
    log = true
  if (log) {
    shenjs_puts("# eval-kl[KL]: " + "\n")
    shenjs_puts(shenjs_call(shen_intmake_string, 
                            ["~R~%", [shen_tuple, x, []]]))
  }
  var js = shenjs_call(js_from_kl, [x])
  if (log)
    shenjs_puts("eval-kl[JS]:\n" + js + "\n\n")
  var ret = shenjs_eval_in_global(js)
  if (log)
    shenjs_puts("eval-kl => '" + ret + "'\n\n")
  if (ret === undefined)
    shenjs_error(["evaluated '" + js + "' to undefined"])
  return ret
}

function shenjs_eval_js(x) {
	if (x.length < 1) return [shen_type_func, shenjs_eval_js, 1, []]
	return new Shen_tco_obj(function() {return shenjs_eval_in_global(js)})
}

shenjs_globals["*language*"] = "Javascript"
shenjs_globals["*implementation*"] = "cli"
shenjs_globals["*port*"] = "0.3"
shenjs_globals["*porters*"] = "Ramil Farkhshatov"
shenjs_globals["js-skip-internals"] = true

shenjs_globals["shen-*show-error-js*"] = false
shenjs_globals["shen-*show-eval-js*"] = false
