
function shenjs_random(n) {
  return Math.floor(Math.random() * (n + 1))
}

shen_random = shenjs_mkfunction("shen-random", 1, function self(X) {
  if (X.length < 1) return [shen_type_func, self, 1, X]
  return shenjs_random(X[0]);
})

shenjs_call(shen_initialise$_arity$_table,
            [[shen_type_cons, [shen_type_symbol, "shen-random"],
                              [shen_type_cons, 1, []]]]);

shenjs_call(shen_declare, [[shen_type_symbol, "shen-random"], [shen_type_cons, [shen_type_symbol, "number"], [shen_type_cons, [shen_type_symbol, "-->"], [shen_type_cons, [shen_type_symbol, "number"], []]]]]);
